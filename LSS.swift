import Foundation

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
    static var null : SHORTINT = 0;
    static var not : SHORTINT = 1; static var exp : SHORTINT = 2; static var log : SHORTINT = 3;
    static var times : SHORTINT = 4; static var div : SHORTINT = 5; static var mod : SHORTINT = 6;
    static var plus : SHORTINT = 7; static var minus : SHORTINT = 8;
    static var eql : SHORTINT = 10; static var neq : SHORTINT = 11; static var lss : SHORTINT = 12;
    static var leq : SHORTINT = 13; static var gtr : SHORTINT = 14; static var geq : SHORTINT = 15;
    static var period : SHORTINT = 18; static var comma : SHORTINT = 19; static var colon : SHORTINT = 20;
    static var rparen : SHORTINT = 22; static var rbrak : SHORTINT = 23;
    static var then : SHORTINT = 24; static var Do : SHORTINT = 25; static var to : SHORTINT = 26;
    static var lparen : SHORTINT = 27; static var lbrak : SHORTINT = 28; static var becomes : SHORTINT = 29; static var pos : SHORTINT = 30;
    static var ident : SHORTINT = 31; static var number : SHORTINT = 32; static var zero : SHORTINT = 33; static var one : SHORTINT = 34;
    static var reg : SHORTINT = 35; static var latch : SHORTINT = 36; static var sr : SHORTINT = 37; static var mux : SHORTINT = 38;
    static var bar : SHORTINT = 39;
    static var semicolon : SHORTINT = 40; static var end : SHORTINT = 41; static var Else : SHORTINT = 42;
    static var elsif : SHORTINT = 43; static var If : SHORTINT = 44; static var For : SHORTINT = 45;
    static var clock : SHORTINT = 46; static var reset : SHORTINT = 47;
    static var integer : SHORTINT = 48; static var bit : SHORTINT = 49; static var ts : SHORTINT = 50;
    static var oc : SHORTINT = 51; static var type : SHORTINT = 52; static var In : SHORTINT = 53; static var Inout : SHORTINT = 54;
    static var out : SHORTINT = 55; static var spos : SHORTINT = 56; static var const : SHORTINT = 57;
    static var Var : SHORTINT = 58; static var begin : SHORTINT = 59; static var Import : SHORTINT = 60;
    static var module : SHORTINT = 61; static var eof : SHORTINT = 62
    
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
    static var R: Files.File?
    static var key: [String] = []
    static var symno: [SHORTINT] = []
    
    static func Mark (num: INTEGER) {
        var fpos: LONGINT;
        
        fpos = Files.Tell(R);
        if fpos > errpos+2 {
            print("*** ERROR: \(errors[num])")
            print("    Line = \(line); pos = \(chpos)")
            print("")
        } // ;
        errpos = fpos; error = true
    } // Mark;
    
    static func Read (inout ch: CHAR) {
        if Files.Eof(R) { ch = "\0" } //;
        ch = Files.ReadChar(R); chpos++
        if (ch == EOL) { line++; chpos = 1 } 
    } // Read;
    
    static func Get (inout sym: SHORTINT) {
        
        func Ident() {
            var i: INTEGER;
            i = 0; id = ""
            repeat {
                if i < IdLen { id.append(ch); i++ }
                Read(&ch)
            } while !( (ch < "0") || (ch > "9") && (ch.uppercase < "A") || (ch.uppercase > "Z"))
            if ch == "'" {
                if i < IdLen { id.append(ch); i++ }
                Read(&ch)
            } // ;
            if i == IdLen {
                Mark(2)
            }
            sym = LSS.ident
        } // Ident;
        
        func SearchKey () {
            var i, j, m: INTEGER;
            i = 0; j = NofKeys;
            while i < j {
                m = (i + j) / 2;
                if key[m] < id { i = m+1 } else { j = m } //
            } // ;
            if key[j] == id { sym = symno[i] } //
        } // SearchKey;
        
        func Number () {
            val = 0; sym = LSS.number
            repeat {
                if val <= (LONGINT.max - ch.unicodeValue() + Character("0").unicodeValue()) / 10 {
                    val = 10 * val + ch.unicodeValue() - Character("0").unicodeValue()
                } else { Mark(7); val = 0
                } // ;
                Read(&ch)
            } while !((ch < "0") || (ch > "9"))
        } // Number;
        
        func comment () {
            Read(&ch);
            repeat {
                repeat {
                    while ch == "(" { Read(&ch);
                        if ch == "*" { comment() } //
                    } // ;
                    if ch == "*" { Read(&ch); break } // ;
                    if Files.Eof(R) { break } // ;
                    Read(&ch)
                } while true
                if ch == ")" { Read(&ch); break } // ;
                if Files.Eof(R) { Mark(8); break } //
            } while true
        } // comment;
        
        
        while (ch <= " ") && (!Files.Eof(R)) { Read(&ch) }
        /* if Files.Eof(R) { sym = eof; RETURN } //; */
        switch ch {
        case "'":
            Read(&ch);
            if ch == "0" {
                sym = LSS.zero
            } else {
                sym = LSS.one;
                if ch != "1" { Mark(9) } //
            }
            Read(&ch)
        case "*":
            Read(&ch); sym = LSS.times
        case "+":
            Read(&ch); sym = LSS.plus
        case "-":
            Read(&ch);
            if ch == ">" { Read(&ch); sym = LSS.bar } else { sym = LSS.minus } //
        case "=":
            Read(&ch); sym = LSS.eql
        case "#":
            Read(&ch); sym = LSS.neq
        case "<":
            Read(&ch);
            if ch == "=" { Read(&ch); sym = LSS.leq } else { sym = LSS.lss } //
        case ">":
            Read(&ch);
            if ch == "=" { Read(&ch); sym = LSS.geq } else { sym = LSS.gtr } //
        case ";":
            Read(&ch); sym = LSS.semicolon
        case ",":
            Read(&ch); sym = LSS.comma
        case ":":
            Read(&ch);
            if ch == "=" { Read(&ch); sym = LSS.becomes
            } else if  ch == ":" { Read(&ch); sym = LSS.pos
            } else { sym = LSS.colon
            } //
        case "." :
            Read(&ch);
            if ch == "." { Read(&ch); sym = LSS.to } else { sym = LSS.period } //
        case "/":
            Read(&ch); sym = LSS.div
        case  "(":
            Read(&ch);
            if ch == "*" { comment(); Get(&sym) } else { sym = LSS.lparen } //
        case ")":
            Read(&ch); sym = LSS.rparen
        case "[":
            Read(&ch); sym = LSS.lbrak
        case "]":
            Read(&ch); sym = LSS.rbrak
        case  "^":
            Read(&ch); sym = LSS.null
        case "0"..."9":
            Number()
        case "A"..."Z":
            Ident(); SearchKey()
        case "a"..."z":
            Ident()
        case  "|":
            Read(&ch); sym = LSS.bar
        case "~":
            Read(&ch); sym = LSS.not
        default: Read(&ch); sym = LSS.null
        } //
    } // Get;

    static func Init (fname: String) {
        error = false; errpos = 0; chpos = 1; line = 1; R = Files.Open(fname, mode:"r"); Read(&ch)
        K = 0;
        Enter("", LSS.begin);
        Enter("BIT", LSS.bit);
        Enter("CLOCK", LSS.clock);
        Enter("CONST", LSS.const);
        Enter("DIV", LSS.div);
        Enter("DO", LSS.Do);
        Enter("ELSE", LSS.Else);
        Enter("ELSIF", LSS.elsif);
        Enter("END", LSS.end);
        Enter("EXP", LSS.exp);
        Enter("FOR", LSS.For);
        Enter("IF", LSS.If);
        Enter("IMPORT", LSS.Import);
        Enter("IN", LSS.In);
        Enter("INOUT", LSS.Inout);
        Enter("LATCH", LSS.latch);
        Enter("LOG", LSS.log);
        Enter("MOD", LSS.mod);
        Enter("MODULE", LSS.module);
        Enter("MUX", LSS.mux);
        Enter("OC", LSS.oc);
        Enter("OUT", LSS.out);
        Enter("POS", LSS.spos);
        Enter("REG", LSS.reg);
        Enter("RESET", LSS.reset);
        Enter("SR", LSS.sr);
        Enter("THEN", LSS.then);
        Enter("TO", LSS.to);
        Enter("TS", LSS.ts);
        Enter("TYPE", LSS.type);
        Enter("VAR", LSS.Var);
        key.append("~ ")
        
        AddError(0, "undefined identifier");
        AddError(1, "multiple definition of identifier");
        AddError(2, "identifier too long");
        AddError(3, "field identifier not visible");
        AddError(4, "identifier mismatch");
        AddError(5, "field identifier undefined");
        AddError(6, "exponent base not 2");
        AddError(7, "number too large");
        AddError(8, "non-terminated comment");
        AddError(9, "illegal constant");
        
        AddError(10, "identifier expected");
        AddError(11, "MODULE expected");
        AddError(12, ". must be followed by identifier or numbe");
        AddError(14, "( expected");
        AddError(15, ") expected");
        AddError(16, "] expected");
        AddError(17, "bad factor");
        AddError(18, "relation expected");
        AddError(19, ", expected");
        AddError(20, ": expected");
        AddError(21, "= or ( expected");
        AddError(22, "{ expected");
        AddError(23, ".. expected");
        AddError(24, "; expected");
        AddError(25, ". expected");
        AddError(26, "} // expected");
        AddError(27, "{ expected");
        AddError(28, "BIT, TS, OC, or identifier expected");
        AddError(29, "= expected");
        
        AddError(32, "indexed variable is not an array");
        AddError(34, ". is not preceded by a record or an array variable");
        AddError(35, "too few actual parameters");
        AddError(36, "too many actual parameters");
        AddError(37, "record type expected");
        AddError(38, "expression is not a constant");
        AddError(39, "integer expression expected");
        AddError(40, "parameter type mismatch");
        AddError(42, "index is not an integer");
        AddError(43, "index out of range");
        AddError(44, "incompatible types");
        AddError(46, "illegal assignment, y not of type BIT");
        AddError(47, "illegal assignment to input");
        AddError(48, "illegal bus assignment");
        AddError(49, "illegal TS-assignment to a non-bus");
    } // Init;
    
    static func Enter(word: String, _ val: SHORTINT) {
       key.append(word); symno.append(val)
    } // Enter;
    
    static func AddError(pos: INTEGER, _ str: String) {
        errors.append(str)
    } // AddError;

} // LSS.
