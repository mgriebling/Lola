import Foundation

func == (lhs: LSS.Symbols, rhs: LSS.Symbols) -> Bool { return lhs.rawValue == rhs.rawValue }
func < (lhs: LSS.Symbols, rhs: LSS.Symbols) -> Bool { return lhs.rawValue < rhs.rawValue }

class LSS {
    /* MG 17.8.2014, NW 16.10.93 / 7.11.96 / 21.10.97 */
    //	IMPORT Out, Files;
    
    static let IdLen = 32; static let NofKeys = 32; static let NofErrors = 50;
    static let EOL : Character = "\n"; static let CR : Character = "\u{0D}"
    
    typealias SHORTINT = Int8
    typealias INTEGER = Int
    typealias LONGINT = Int
    typealias CHAR = Character
    typealias BOOLEAN = Bool
    
    /* symbols */
    enum Symbols : SHORTINT, Comparable {
        case null = 0,
        not = 1, exp = 2, log = 3,
        times = 4, div = 5, mod = 6, plus = 7, minus = 8,
        eql = 10, neq = 11, lss = 12, leq = 13, gtr = 14, geq = 15,
        period = 18, comma = 19, colon = 20, rparen = 22, rbrak = 23,
        then = 24, Do = 25, to = 26,
        lparen = 27, lbrak = 28, becomes = 29, pos = 30,
        ident = 31, number = 32, zero = 33, one = 34,
        reg = 35, latch = 36, sr = 37, mux = 38, bar = 39,
        semicolon = 40, end = 41, Else = 42, elsif = 43, If = 44, For = 45, clock = 46, reset = 47,
        integer = 48, bit = 49, ts = 50, oc = 51, type = 52, In = 53, Inout = 54,
        out = 55, spos = 56, const = 57, Var = 58, begin = 59, Import = 60, module = 61, eof = 62
    }
    
    typealias Ident = String
    
    static var val: LONGINT = 0
    static var id: Ident = ""
    static var error: BOOLEAN = false
    static var errors: [String] = []
    static var ch: CHAR = "\0"
    static var K: INTEGER = 0
    static var line: INTEGER = 0
    static var chpos: INTEGER = 0
    static var errpos: LONGINT = 0
    static var R = Files.File()
    static var key: [String] = []
    static var symno: [Symbols] = []
    
    static func Mark (num: INTEGER) {
        var fpos: LONGINT;
        
        fpos = Files.Tell(R)
        if fpos > errpos+2 {
            print("*** ERROR: \(errors[num])")
            print("    Line = \(line); pos = \(chpos)")
            print("")
        }
        errpos = fpos; error = true
    } // Mark
    
    static func Read (inout ch: CHAR) {
        if Files.Eof(R) { ch = "\0"; return }
        ch = Files.ReadChar(&R); chpos += 1
        if (ch == EOL) { line += 1; chpos = 1 }
    } // Read
    
    static func Get (inout sym: Symbols) {
        
        func Ident() {
            var i: INTEGER
            i = 0; id = ""
            repeat {
                if i < IdLen { id.append(ch); i += 1 }
                Read(&ch)
            } while !( (ch < "0") || (ch > "9") && (ch.uppercase < "A") || (ch.uppercase > "Z"))
            if ch == "'" {
                if i < IdLen { id.append(ch); i += 1 }
                Read(&ch)
            } // ;
            if i == IdLen {
                Mark(2)
            }
            sym = LSS.Symbols.ident
        } // Ident
        
        func SearchKey () {
            var i, j, m: INTEGER
            i = 0; j = NofKeys
            while i < j {
                m = (i + j) / 2
                if key[m] < id { i = m+1 } else { j = m }
            }
            if key[j] == id { sym = symno[i] }
        } // SearchKey
        
        func Number () {
            val = 0; sym = LSS.Symbols.number
            repeat {
                if val <= (LONGINT.max - ch.unicodeValue() + Character("0").unicodeValue()) / 10 {
                    val = 10 * val + ch.unicodeValue() - Character("0").unicodeValue()
                } else { Mark(7); val = 0
                }
                Read(&ch)
            } while !(ch < "0" || ch > "9")
        } // Number
        
        func comment () {
            Read(&ch)
            repeat {
                repeat {
                    while ch == "(" { Read(&ch);
                        if ch == "*" { comment() }
                    }
                    if ch == "*" { Read(&ch); break }
                    if Files.Eof(R) { break }
                    Read(&ch)
                } while true
                if ch == ")" { Read(&ch); break }
                if Files.Eof(R) { Mark(8); break }
            } while true
        } // comment
        
        while ch <= " " && !Files.Eof(R) { Read(&ch) }
        switch ch {
        case "'":
            Read(&ch)
            if ch == "0" {
                sym = LSS.Symbols.zero
            } else {
                sym = LSS.Symbols.one;
                if ch != "1" { Mark(9) }
            }
            Read(&ch)
        case "*":
            Read(&ch); sym = LSS.Symbols.times
        case "+":
            Read(&ch); sym = LSS.Symbols.plus
        case "-":
            Read(&ch)
            if ch == ">" { Read(&ch); sym = LSS.Symbols.bar } else { sym = LSS.Symbols.minus }
        case "=":
            Read(&ch); sym = LSS.Symbols.eql
        case "#":
            Read(&ch); sym = LSS.Symbols.neq
        case "<":
            Read(&ch)
            if ch == "=" { Read(&ch); sym = LSS.Symbols.leq } else { sym = LSS.Symbols.lss }
        case ">":
            Read(&ch);
            if ch == "=" { Read(&ch); sym = LSS.Symbols.geq } else { sym = LSS.Symbols.gtr }
        case ";":
            Read(&ch); sym = LSS.Symbols.semicolon
        case ",":
            Read(&ch); sym = LSS.Symbols.comma
        case ":":
            Read(&ch);
            if ch == "=" { Read(&ch); sym = LSS.Symbols.becomes
            } else if  ch == ":" { Read(&ch); sym = LSS.Symbols.pos
            } else { sym = LSS.Symbols.colon
            }
        case "." :
            Read(&ch);
            if ch == "." { Read(&ch); sym = LSS.Symbols.to } else { sym = LSS.Symbols.period }
        case "/":
            Read(&ch); sym = LSS.Symbols.div
        case  "(":
            Read(&ch);
            if ch == "*" { comment(); Get(&sym) } else { sym = LSS.Symbols.lparen }
        case ")":
            Read(&ch); sym = LSS.Symbols.rparen
        case "[":
            Read(&ch); sym = LSS.Symbols.lbrak
        case "]":
            Read(&ch); sym = LSS.Symbols.rbrak
        case  "^":
            Read(&ch); sym = LSS.Symbols.null
        case "0"..."9":
            Number()
        case "A"..."Z":
            Ident(); SearchKey()
        case "a"..."z":
            Ident()
        case  "|":
            Read(&ch); sym = LSS.Symbols.bar
        case "~":
            Read(&ch); sym = LSS.Symbols.not
        default:
            Read(&ch); sym = LSS.Symbols.null
        }
    } // Get

    static func Init (fname: String) {
        error = false; errpos = 0; chpos = 1; line = 1
        if let file = Files.Open(fname, mode:"r") { R = file }
        else { print(""); print("Couldn't open \(fname)!") }
        Read(&ch)
        
        K = 0
        Enter("BEGIN", LSS.Symbols.begin)
        Enter("BIT", LSS.Symbols.bit)
        Enter("CLOCK", LSS.Symbols.clock)
        Enter("CONST", LSS.Symbols.const)
        Enter("DIV", LSS.Symbols.div)
        Enter("DO", LSS.Symbols.Do)
        Enter("ELSE", LSS.Symbols.Else)
        Enter("ELSIF", LSS.Symbols.elsif)
        Enter("END", LSS.Symbols.end)
        Enter("EXP", LSS.Symbols.exp)
        Enter("FOR", LSS.Symbols.For)
        Enter("IF", LSS.Symbols.If)
        Enter("IMPORT", LSS.Symbols.Import)
        Enter("IN", LSS.Symbols.In)
        Enter("INOUT", LSS.Symbols.Inout)
        Enter("LATCH", LSS.Symbols.latch)
        Enter("LOG", LSS.Symbols.log)
        Enter("MOD", LSS.Symbols.mod)
        Enter("MODULE", LSS.Symbols.module)
        Enter("MUX", LSS.Symbols.mux)
        Enter("OC", LSS.Symbols.oc)
        Enter("OUT", LSS.Symbols.out)
        Enter("POS", LSS.Symbols.spos)
        Enter("REG", LSS.Symbols.reg)
        Enter("RESET", LSS.Symbols.reset)
        Enter("SR", LSS.Symbols.sr)
        Enter("THEN", LSS.Symbols.then)
        Enter("TO", LSS.Symbols.to)
        Enter("TS", LSS.Symbols.ts)
        Enter("TYPE", LSS.Symbols.type)
        Enter("VAR", LSS.Symbols.Var)
        key.append("~ ")
        
        AddError(0, "undefined identifier")
        AddError(1, "multiple definition of identifier")
        AddError(2, "identifier too long")
        AddError(3, "field identifier not visible")
        AddError(4, "identifier mismatch")
        AddError(5, "field identifier undefined")
        AddError(6, "exponent base not 2")
        AddError(7, "number too large")
        AddError(8, "non-terminated comment")
        AddError(9, "illegal constant")
        
        AddError(10, "identifier expected")
        AddError(11, "MODULE expected")
        AddError(12, ". must be followed by identifier or numbe")
        AddError(14, "( expected")
        AddError(15, ") expected")
        AddError(16, "] expected")
        AddError(17, "bad factor")
        AddError(18, "relation expected")
        AddError(19, ", expected")
        AddError(20, ": expected")
        AddError(21, "= or ( expected")
        AddError(22, "{ expected")
        AddError(23, ".. expected")
        AddError(24, "; expected")
        AddError(25, ". expected")
        AddError(26, "} // expected")
        AddError(27, "{ expected")
        AddError(28, "BIT, TS, OC, or identifier expected")
        AddError(29, "= expected")
        
        AddError(32, "indexed variable is not an array")
        AddError(34, ". is not preceded by a record or an array variable")
        AddError(35, "too few actual parameters")
        AddError(36, "too many actual parameters")
        AddError(37, "record type expected")
        AddError(38, "expression is not a constant")
        AddError(39, "integer expression expected")
        AddError(40, "parameter type mismatch")
        AddError(42, "index is not an integer")
        AddError(43, "index out of range")
        AddError(44, "incompatible types")
        AddError(46, "illegal assignment, y not of type BIT")
        AddError(47, "illegal assignment to input")
        AddError(48, "illegal bus assignment")
        AddError(49, "illegal TS-assignment to a non-bus")
    } // Init
    
    static func Enter(word: String, _ val: Symbols) {
       key.append(word); symno.append(val)
    } // Enter;
    
    static func AddError(pos: INTEGER, _ str: String) {
        errors.append(str)
    } // AddError;

} // LSS.
