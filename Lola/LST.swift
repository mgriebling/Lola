import Foundation

class LST {
    
    // IMPORT LSB, Files, Strings;
    
    static let LOLA = "LOLA";
    static let NOTFOUND = -1;
    static let NumberOfSymbols = 2000;
    
    typealias SHORTINT = Int8
    typealias BOOLEAN = Bool
    typealias INTEGER = Int
    typealias CHAR = Character
    
    static var NILL: INTEGER = 0
    static var LVAR: INTEGER = 0
    static var SIG: INTEGER = 0
    static var f : Files.File?
    static var maxSym: INTEGER = 0
    static var Var: LSB.Signal?
    static var sig: LSB.Signal?
    static var symbols: [LSB.Signal?] = []
    
    /* Symbol Storage */
    
    static func SymbolFound (s: LSB.Signal?) -> INTEGER {
        var pos: INTEGER
        
        /* check if the symbol is already present */
        pos = 0;
        while (pos < maxSym) && (s !== symbols[pos]) { pos += 1 }
        if pos == maxSym {
            return NOTFOUND
        } else {
            return pos
        } //
    } // SymbolFound;
    
    static func AddSymbol (s: LSB.Signal?) -> INTEGER {
        var pos: INTEGER;
        
        /* check if the symbol is already present */
        pos = SymbolFound(s);
        
        /* add the symbol if new */
        if pos == NOTFOUND { symbols.append(s); maxSym += 1 }
        return pos;
    } // AddSymbol;
    
    static func Read (inout s : SHORTINT) {
        var c: CHAR;
        
        c = Files.ReadChar(&f!)
        var si = c.unicodeValue()
        if si > 127 { si -= 256 }
        s = SHORTINT(si)
    } // Read;
    
    static func ReadNum (inout num: INTEGER) {
        /* Read integers in a compressed and portable format. */
        var s: INTEGER; var x: CHAR; var y: INTEGER;
        
        s=0; y=0; x = Files.ReadChar(&f!)
        while x.unicodeValue() >= 128 {
            y += (x.unicodeValue()-128) << s; s += 7
            x = Files.ReadChar(&f!)
        } //;
        num = y + (x.unicodeValue() % 64 - x.unicodeValue() / 64 * 64) << s
    } // ReadNum;
    
    static func ReadString (inout s : String) {
        var i : INTEGER;
        var c : CHAR;
        
        i = 0; s = ""
        repeat {
            c = Files.ReadChar(&f!)
            if Files.Eof(f!) || (c == "\0") { break }
            s.append(c)
            i += 1
        } while true
    } // ReadString;
    
    static func ReadSignal (inout x: LSB.Signal?) {
        var v: INTEGER = 0
        var y: LSB.Variable;
        var t: LSB.Signal?
        
        ReadNum(&v);
        if v == NILL {
            x = nil
        } else if (v == LVAR) || (v == SIG) {
            /* new variable or signal */
            if v == LVAR {
                y = LSB.Variable(); x = y; v = AddSymbol(x!);
                ReadString(&y.name);
                Read(&y.classv);
                ReadSignal(&t);
                if t != nil { y.next = (t as! LSB.Variable) } else { y.next = nil }
                ReadSignal(&t);
                if t != nil { y.dsc = (t as! LSB.Variable) } else { y.dsc = nil }
            } else { x = LSB.Signal(); v = AddSymbol(x!)
            }
            ReadSignal(&x!.x); ReadSignal(&x!.y)
            Read(&x!.fct); Read(&x!.val)
            Read(&x!.u); Read(&x!.v)
        } else {
            /* existing variable or signal */
            if v < maxSym { x = symbols[v]
            } else { x = nil /* ERROR */
            } //
        } //
    } // ReadSignal;
    
    static func AddDefaultSymbols () {
        Var = LSB.Signal()
        sig = LSB.Signal()
        AddSymbol(nil); NILL = SymbolFound(nil)
        AddSymbol(Var); LVAR = SymbolFound(Var)
        AddSymbol(sig); SIG = SymbolFound(sig)
        AddSymbol(LSB.zero)
        AddSymbol(LSB.one)
        AddSymbol(LSB.clk)
    } // AddDefaultSymbols;
    
    static func Init () {
        maxSym = 0
        symbols = []        /* No symbols yet */
        AddDefaultSymbols()	/* add the standard predefined symbols */
    } // Init;
    
    static func Import (modName: String) {
        var fname : String
        var v: LSB.Signal?
        
        Init()
        fname = modName + ".lola"
        f = Files.Open(fname, mode: "r");
        if f == nil { return }
        
        /* check header */
        LSB.org = nil
        ReadString(&fname);
        if fname == LOLA {
            /* read body */
            ReadSignal(&v); LSB.org = (v as! LSB.Variable)
        }
        Files.Close(f!);
    } // Import;
    
    /*----------------- Export --------------------*/
    
    static func Write (s : SHORTINT) {
        Files.WriteChar(f!, ch: s)
    } // Write;
    
    static func WriteNum (lint: INTEGER) {
        /** Write integers in a compressed and portable format.  */
        var lint = lint
        while (lint < -64) || (lint > 63) {
            Files.WriteChar(f!, ch:Int8(lint % 128 + 128));
            lint = lint / 128
        }
        Files.WriteChar(f!, ch:Int8(lint % 128));
    } // WriteNum;
    
    static func WriteString (s: String) {
        Files.WriteString(f!, s:s); Write(0);
    } // WriteString;
    
    static func WriteSignal(x: LSB.Signal?) {
        var v: INTEGER;
        
        if x != nil {
            v = AddSymbol(x);
            if v != NOTFOUND {
                WriteNum(v)
            } else {
                if let x = (x as? LSB.Variable) {
                    WriteNum(LVAR);
                    WriteString(x.name);
                    Write(x.classv);
                    WriteSignal(x.next);
                    WriteSignal(x.dsc);
                } else { WriteNum(SIG)
                }
                WriteSignal(x!.x);  WriteSignal(x!.y);
                Write(x!.fct); Write(x!.val);
                Write(x!.u); Write(x!.v)
            } //
        } else { WriteNum(NILL)
        } //
    } // WriteSignal;
    
    static func Export (modName: String) {
        var fname : String
        
        Init()
        if LSB.org == nil { return }
        fname = modName + ".lola"
        f = Files.Open(fname, mode:"wm");  // was "w"
        if f == nil { return }
        
        /* write header and body */
        WriteString(LOLA);
        WriteSignal(LSB.org);
        
        /* output as C file */
        Files.DumpToC(f!)
        Files.Close(f!)
    } // Export;
    
}
