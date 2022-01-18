
import Foundation

///
///  Display data structures;  NW 28.8.2015
///  Lola
///
///  Created by Mike Griebling on 2022-01-17.
///  Copyright © 2022 Computer Inspirations. All rights reserved.
///
class LSP {
    
    static var C: [String] = {
        var C = [String](repeating: "", count: 64)
        C[LSB.const] = "CONST"; C[LSB.typ] = "TYPE"; C[LSB._var] = "VAR"
        C[LSB.lit] = "LIT"; C[LSB.sel] = "MUX"; C[LSB.range] = ": "; C[LSB.cons] = ", "; C[LSB.repl] = "**"
        C[LSB.or] = "| "; C[LSB.xor] = "^ "; C[LSB.and] = "& ";  C[LSB.not] = "~ "
        C[LSB.add] = "+ "; C[LSB.sub] = "- "; C[LSB.mul] = "* "; C[LSB.div] = "/ "
        C[LSB.eql] = "= "; C[LSB.neq] = "# "; C[LSB.lss] = "< "; C[LSB.geq] = ">="; C[LSB.leq] = "<="; C[LSB.gtr] = "> "
        C[LSB.then] = " -> "; C[LSB._else] = " :: "; C[LSB.ts] = "TS "; C[LSB.next] = "--"
        return C
    }()
    
    // for convenience
    static func write(_ s:String) { print(s, terminator: "") }
    static func write(_ i:Int, _ len:Int) { print(i, terminator: "") }
    static func writeln(_ s:String = "") { print(s) }
    
    static func PrintType(_ typ: LSB.TType) {
        // let obj: LSB.Object
        if let typ = typ as? LSB.ArrayType {
            write("["); write(typ.len, 1); write("]"); PrintType(typ.eltyp)
        } else if let _ = typ as? LSB.UnitType {
            write("UnitType "); /* obj = typ.firstobj */ // never used?
        } else {
            write("BIT")
        }
    } // PrintType;
    
    static func PrintTree(_ x: LSB.Item!, _ n: Int) {
        var i: Int;
        if x !== nil {  i = n;
            if let x = x as? LSB.Object {
                while i > 0 { write("  "); i-=1 }
                write(x.name); writeln()
            } else {
                PrintTree(x.a, n+1);
                while i > 0 { write("  "); i-=1 }
                if x.tag == LSB.lit { write(x.val, 1) } else { write(C[x.tag]) }
                writeln()
                PrintTree(x.b, n+1)
            }
        }
    } // PrintTree;
    
    static func PrintObj(_ obj: LSB.Object, _ n: Int) {
        var apar: LSB.Item!; var obj1: LSB.Object!
        if n > 0 { write("  ") }
        write(C[obj.tag]); write(" "); write(obj.name)
        if obj.tag == LSB.const { write(" = "); PrintTree(obj.b, 1); writeln()
        } else if obj.tag == LSB.typ {
            if let objType = obj.type as? LSB.UnitType {  /*formal param list*/
                obj1 = objType.firstobj;
                writeln(" BEGIN ")
                while (obj1 !== nil) && (obj1 !== LSB.root) { PrintObj(obj1, 0); obj1 = obj1.next }
                writeln("END")
            } else {
                PrintType(obj.type)
            }
        } else {
            /*var*/ write(": ");
            if obj.type is LSB.UnitType {
                write(obj.type.typobj.name);
                apar = obj.b; write(" [");  /*actual param list*/
                while apar !== nil { PrintTree(apar.b, 1); apar = apar.a }
                write("]"); writeln()
            } else { PrintType(obj.type);
                write(" #"); write(obj.val, 1);
                if obj.a !== nil {
                    if obj.val == 0 { write(" CLK") } else if obj.val == 1 { /*indexed*/ write(" DEMUX") }
                    PrintTree(obj.a, 1)
                }
                if obj.b != nil { write(" := "); writeln(); PrintTree(obj.b, 1)
                } else { writeln()
                }
            }
        }
    } // PrintObj;
    
    static public func List() {
        var obj: LSB.Object!
        obj = LSB.top;
        write("listing "); write(LSB.modname); writeln()
        while obj !== LSB.root && obj != nil { PrintObj(obj, 0); obj = obj.next }
    } // List;
    
}
