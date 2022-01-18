import Foundation

class LSS {
    /* MG 17.8.2022, NW 16.10.93 / 7.11.96 / 21.10.97 */
    
    static let IdLen = 32; //static let NofKeys = 11
    static let EOL : Character = "\n"; static let CR : Character = "\u{0D}"
    
    /* symbols */
    public enum Symbols : Int, Comparable {
        case null = 0,
             arrow = 1, times = 2, `div` = 3, and = 4, plus = 5, minus = 6, or = 7, xor = 8,  not = 9,
             eql = 10, neq = 11, lss = 12, leq = 13, gtr = 14, geq = 15,
             at = 16, apo = 17, period = 18, comma = 19, colon = 20, rparen = 21, rbrak = 22, rbrace = 23,
             then = 24, lparen = 26, lbrak = 27, lbrace = 28, repl = 29, becomes = 30,
             ident = 31, integer = 32, ts = 33, semicolon = 40, end = 41,
             const = 51, type = 52, reg = 53, `var` = 54, out = 55, `inout` = 56, `in` = 57,
             begin = 58, module = 59, eof = 60
        
        static func == (lhs: Symbols, rhs: Symbols) -> Bool { lhs.rawValue == rhs.rawValue }
        static func < (lhs: Symbols, rhs: Symbols) -> Bool  { lhs.rawValue < rhs.rawValue }
    }
    
    typealias Ident = String
    
    static public var val = 0
    static public var id: Ident = ""
    static public var error = false
    
    static private var ch: Character = " "
    static private var K: Int = 0
    static private var line: Int = 0
    static private var chpos: Int = 0
    static private var errpos: Int = 0
    static private var R = File()
    static private var keySym = [
        "BEGIN":Symbols.begin, "CONST":.const, "END":.end, "IN":.`in`, "INOUT":.`inout`,
        "MODULE":.module, "OUT":.out, "REG":.reg, "TYPE":.type, "VAR":.`var`, "TS":.ts
    ]
    
    static public func mark (_ errorString:String) {
        let fpos = Files1.Tell(R)
        if fpos > errpos+2 {
            print("*** ERROR: \(errorString)")
            print("    Line = \(line); pos = \(chpos)")
            print("")
        }
        errpos = fpos; error = true
    } // mark
    
    static private func Read (_ ch: inout Character) {
        if Files1.Eof(R) { ch = "\0"; return }
        ch = Files1.ReadChar(&R); chpos += 1
        if ch == EOL { line += 1; chpos = 1 }
    } // Read
    
    static private func identifier(_ sym: inout Symbols) {
        id = ""
        repeat {
            if id.count < IdLen { id.append(ch) }
            Read(&ch)
        } while (ch >= "0") && (ch <= "9") || (ch >= "A") && (ch <= "Z") || (ch >= "a") && (ch <= "z")
        if ch == "'" {
            if id.count < IdLen { id.append(ch) }
            Read(&ch)
        }
        if id.count == IdLen { mark("ident too long") }
        sym = keySym[id] ?? .ident
    } // identifier
    
    static private func number (_ sym: inout Symbols) {
        let maxDigits = 16
        let zeroASCII = Character("0").asciiValue!
        let aOffset = Character("A").asciiValue! - zeroASCII - 10
        var k = 0
        var hex = false
        var dig = [Int](); dig.reserveCapacity(maxDigits)
        sym = .integer
        repeat {
            if dig.count < maxDigits {
                var d = Int(ch.asciiValue! - zeroASCII)
                if d >= 10 { hex = true; d -= Int(aOffset) }
                dig.append(d)
            }  else {
                mark("too many digits"); dig = [Int]()
            }
            Read(&ch)
        } while (ch >= "0") && (ch <= "9") || (ch >= "A") && (ch <= "F")
        if ch == "H" { /* hex */
            for d in dig { k = k*16 + d } /* no overflow check */
            Read(&ch)
        }  else {
            if hex { mark("illegal hex digit") }
            for d in dig { k = k*10 + d }
        }
        val = k
    } // number
    
    static private func comment () {
        Read(&ch)
        repeat {
            while ch != "\0" && ch != "*" {
                if ch == "(" {
                    Read(&ch);
                    if ch == "*" { comment() }
                } else {
                    Read(&ch)
                }
            }
            while ch == "*" { Read(&ch) }
        } while (ch != ")") && ch != "\0"
        if ch != "\0" { Read(&ch) } else { mark("comment not terminated") }
    } // comment
    
    static public func Get (_ sym: inout Symbols) {
        repeat {
            while ch != "\0" && ch <= " " { Read(&ch) }
            if ch == "\0" { sym = .eof }
            else if ch < "A" {
                if ch < "0" {
                    if ch == "!" { Read(&ch); sym = .repl }
                    else if ch == "#" { Read(&ch); sym = .neq }
                    else if ch == "$" { Read(&ch); sym = .null }
                    else if ch == "&" { Read(&ch); sym = .and }
                    else if ch == "'" { Read(&ch); sym = .apo }
                    else if ch == "(" { Read(&ch)
                        if ch == "*" { sym = .null; comment() } else { sym = .lparen } }
                    else if ch == ")" { Read(&ch); sym = .rparen }
                    else if ch == "*" { Read(&ch); sym = .times }
                    else if ch == "+" { Read(&ch); sym = .plus }
                    else if ch == "," { Read(&ch); sym = .comma }
                    else if ch == "-" { Read(&ch)
                        if ch == ">" { Read(&ch); sym = .`then` } else { sym = .minus } }
                    else if ch == "." { Read(&ch); sym = .period }
                    else if ch == "/" { Read(&ch); sym = .`div` }
                    else { sym = .null }
                }
                else if ch <= "9" { number(&sym) }
                else if ch == ":" { Read(&ch)
                    if ch == "=" { Read(&ch); sym = .becomes } else { sym = .colon } }
                else if ch == ";" { Read(&ch); sym = .semicolon }
                else if ch == "<" { Read(&ch)
                    if ch == "=" { Read(&ch); sym = .leq } else { sym = .lss } }
                else if ch == "=" { Read(&ch); sym = .eql }
                else if ch == ">" { Read(&ch)
                    if ch == "=" { Read(&ch); sym = .geq } else { sym = .gtr } }
                else if ch == "?" { Read(&ch); sym = .`then` }
                else if ch == "@" { Read(&ch); sym = .at }
                else { sym = .null }
            }
            else if ch <= "Z" { identifier(&sym) }
            else if ch < "a" {
                if ch == "[" { Read(&ch); sym = .lbrak }
                else if ch == "]" { Read(&ch); sym = .rbrak }
                else if ch == "^" { Read(&ch); sym = .xor }
                else { sym = .null }
            }
            else if ch <= "z" { identifier(&sym) }
            else if ch <= "{" { Read(&ch); sym = .lbrace }
            else if ch <= "|" { Read(&ch); sym = .or }
            else if ch <= "}" { Read(&ch); sym = .rbrace }
            else if ch <= "~" { Read(&ch); sym = .not }
            else { sym = .null }
        } while sym == .null
    } // Get

    static public func Init (_ fname: String) {
        error = false; errpos = 0; chpos = 1; line = 1
        print("Open File \(fname)")
        if let file = Files1.Open(fname, mode:"r") {
            R = file; Read(&ch)
            print("Read ch = '\(ch)'")
        } else { print(""); print("Couldn't open \(fname)!") }
    } // Init
    
//    static func Enter(_ word: String, _ val: Symbols) {
//       key.append(word); symno.append(val)
//    } // Enter;
    
//    static func AddError(_ pos: Int, _ str: String) {
//        errors.append(str)
//    } // AddError;

} // LSS.
