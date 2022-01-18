//
//  Lola System: display Verilog; generate txt-File; NW 31.8.2015
//
//  Created by Mike Griebling on 2022-01-17.
//  Copyright © 2022 Computer Inspirations. All rights reserved.
//

import Foundation

class LSV {

    static var nofgen: Int = 0
    static var Constructor = Constructor0   /*to avoid forward reference*/
    static var F = File()
    static var C: [String] = {
        var C = [String](repeating: "", count: 64)
        C[LSB.const] = "CONST"; C[LSB.typ] = "TYPE"; C[LSB._var] = "VAR";
        C[LSB.lit] = "LIT"; C[LSB.sel] = "SEL"; C[LSB.range] = ":"; C[LSB.cons] = ",";
        C[LSB.or] = " | "; C[LSB.xor] = " ^ "; C[LSB.and] = " & ";  C[LSB.not] = "~";
        C[LSB.add] = " + "; C[LSB.sub] = " - "; C[LSB.mul] = " * "; C[LSB.div] = " / ";
        C[LSB.eql] = " == "; C[LSB.neq] = " != "; C[LSB.lss] = " <  "; C[LSB.geq] = " >= "; C[LSB.leq] = " <= "; C[LSB.gtr] = " >  ";
        C[LSB.then] = " ? "; C[LSB._else] = " : "; C[LSB.ts] = "TS"; C[LSB.next] = "--"
        return C
    }()

    static func Write(_ ch: Character) { Files1.WriteChar(F, ch) }
    static func WriteLn() { Write("\r\n") }
    static func WriteInt(_ x: Int) { WriteString("\(x)") }
    static func WriteString(_ s: String) { Files1.WriteString(F, s) }
    static func WriteHex(_ x: Int) { WriteString(String(abs(x), radix: 16, uppercase: true)) }
    
    /* ------------------------------- */
    
    static func Type(_ typ: LSB.TType) {
        if let typ = typ as? LSB.ArrayType {
            if typ.eltyp !== LSB.bitType {
                Write("["); WriteInt(typ.len - 1); WriteString(":0]"); Type(typ.eltyp)
            }
        } else if typ is LSB.UnitType { /* obj = typ(LSB.UnitType).firstobj; */
        }
    } //Type;
    
    static func BitArrLen(_ typ: LSB.TType) {
        var eltyp: LSB.TType; var typ = typ
        if typ is LSB.ArrayType {
            eltyp = (typ as! LSB.ArrayType).eltyp
            while eltyp is LSB.ArrayType { typ = eltyp; eltyp = (typ as! LSB.ArrayType).eltyp }
            if eltyp === LSB.bitType {
                Write("["); WriteInt(typ.len - 1); WriteString(":0] ")
            }
        }
    } //BitArrLen;
    
    static func Expression(_ x: LSB.Item!) {
        var x: LSB.Item! = x
        if x !== nil {
            if let x = x as? LSB.Object { WriteString(x.name)
            } else if x.tag == LSB.cons {
                Write("{"); Constructor(&x); Write("}")
            } else {
                if x.tag == LSB.repl {
                    Write("{"); WriteInt(x.b.val); Write("{"); Expression(x.a);
                    Write("}"); Write("}")
                } else {
                    if (x.tag >= LSB.and) && (x.tag <= LSB.then) { Write("(") }
                    Expression(x.a);
                    if x.tag == LSB.sel { Write("["); Expression(x.b); Write("]")
                    } else if x.tag == LSB.lit {
                        if x.size != 0 { WriteInt(x.size); Write("'"); Write("h"); WriteHex(x.val)
                        } else { WriteInt(x.val)
                        }
                    } else { WriteString(C[x.tag]); Expression(x.b)
                    }
                    if (x.tag >= LSB.and) && (x.tag <= LSB.then) { Write(")") }
                }
            }
        }
    } //Expression;
    
    static func Elem(_ x: inout LSB.Item) {
        if x.tag == LSB.repl {
            Write("{"); WriteInt(x.b.val); Write("{"); Expression(x.a);  WriteString("}}")
        } else { Expression(x)
        }
    } //Elem;

    static func Constructor0(_ x: inout LSB.Item) {
        if x.tag == LSB.cons { Constructor(&x.a); WriteString(", "); Elem(&x.b) } else { Elem(&x) }
    } //Constructor0;

    static func Declaration(_ obj: LSB.Object) {
        var apar: LSB.Item!
        if obj.type is LSB.UnitType { WriteString("unit ") } else { Type(obj.type) }
        if obj.tag == LSB._var {
            if obj.type is LSB.UnitType {
                apar = obj.a; WriteLn(); Write("[");
                while apar !== nil { Expression(apar.b); apar = apar.a }
                Write("]")
            }
        } else if obj.tag == LSB.const { WriteString(" = "); WriteInt(obj.val)
        }
    } //Declaration;

    static func ObjList0(_ obj: LSB.Object) {  /*declarations*/
        var obj1: LSB.Object; var param = true
        var obj = obj
        while obj !== LSB.root {
            if (obj.tag == LSB._var) && !(obj.type is LSB.UnitType) {
                if obj.val <= 1 { WriteString("reg ")
                } else if obj.val == 2 { WriteString("wire ")
                } else if obj.val == 3 { WriteString("output ")
                } else if obj.val == 4 { WriteString("output reg ")
                } else if obj.val == 5 { WriteString("inout ")
                } else if obj.val == 6 { WriteString("input ")
                } else { WriteString("??? ")
                }
                BitArrLen(obj.type); WriteString(obj.name);
                obj1 = obj.next;
                while (obj1 !== LSB.top) && (obj1.type === obj.type) && (obj1.val == obj.val) {
                    WriteString(", "); obj = obj1; WriteString(obj.name); obj1 = obj.next
                }
                if param && (obj.val >= 3) && (obj1.val < 3) {  /*end param list*/ param = false; Write(")")
                }
                if obj.type !== LSB.bitType && (obj.type as! LSB.ArrayType).eltyp !== LSB.bitType { Type(obj.type) }
                if param { Write(",") } else { Write(";") }
                WriteLn()
            } else if obj.tag == LSB.const {
            }
            obj = obj.next
        }
    } //ObjList0;
    
    static func ActParam(_ x: inout LSB.Item, _ fpar: LSB.Object) {
        Write("."); WriteString(fpar.name); Write("("); Expression(x); Write(")")
    } //ActParam;

    static func ObjList1(_ obj: LSB.Object) {  /*assignments to variables*/
        var apar, x: LSB.Item!; var fpar: LSB.Object; var size: Int;
        var obj = obj
        
        while obj !== LSB.root {
            if (obj.tag == LSB._var) || (obj.tag == LSB.const) {
                if obj.type is LSB.UnitType {
                    WriteString(obj.type.typobj.name); Write(" "); WriteString(obj.name);
                    apar = obj.b; fpar = (obj.type as!LSB.UnitType).firstobj;
                    Write("("); ActParam(&apar.b, fpar); apar = apar.a; fpar = fpar.next;  /*actual param list*/
                    while apar !== nil { WriteString(", "); ActParam(&apar.b, fpar); apar = apar.a; fpar = fpar.next }
                    Write(")"); Write(";"); WriteLn()
                } else if (obj.b != nil) && (obj.val == 5) {  /*tri-state*/
                    size = obj.type.size; x = obj.b;
                    if x.tag == LSB.ts {
                        if obj.type === LSB.bitType {
                            WriteString("IOBUF block"); nofgen+=1; WriteInt(nofgen); WriteString(" (.IO("); WriteString(obj.name);
                            WriteString("), .O("); WriteString((x.a as! LSB.Object).name); WriteString("), .I("); x = x.b;
                            if x.a.type === LSB.bitType { Expression(x.a) } else { WriteString((x.a as! LSB.Object).name) }
                            WriteString("), .T(");
                            if x.b.type === LSB.bitType { Expression(x.b) } else {  WriteString((x.b as! LSB.Object).name) }
                            WriteString("));")
                        } else {  /*array type*/
                            if nofgen == 0 { WriteString("genvar i;"); WriteLn() }
                            nofgen+=1; WriteString("generate"); WriteLn()
                            WriteString("for (i = 0; i < "); WriteInt(size); WriteString("; i = i+1) begin : bufblock"); WriteInt(nofgen); WriteLn()
                            WriteString("IOBUF block (.IO("); WriteString(obj.name);
                            WriteString("[i]), .O("); WriteString((x.a as! LSB.Object).name); WriteString("[i]), .I("); x = x.b;
                            WriteString((x.a as! LSB.Object).name); WriteString("[i]), .T(");
                            if x.b.type === LSB.bitType { Expression(x.b) } else { WriteString((x.b as! LSB.Object).name); WriteString("[i]") }
                            WriteString("));"); WriteLn(); WriteString("end"); WriteLn(); WriteString("endgenerate")
                        }
                        WriteLn()
                    }
                } else if (obj.b !== nil) && (obj.val >= 2) {
                    WriteString("assign "); WriteString(obj.name);
                    if (obj.a != nil) { Write("["); Expression(obj.a); Write("]") }
                    WriteString(" = "); Expression(obj.b); Write(";"); WriteLn()
                }
            } else if obj.tag == LSB.typ { /*instantiation; actual parameters*/
            }
            obj = obj.next
        }
    } //ObjList1;

    static func ObjList2(_ obj: LSB.Object) {  /*assignments to registers*/
        var kind: Int
        var obj = obj
        while obj !== LSB.root {
            if (obj.tag == LSB._var) && !(obj.type is LSB.UnitType) && (obj.val < 2) {
                WriteString("always @ (posedge "); kind = obj.val;
                if kind == 0 { Expression(obj.a)
                } else { /*kind == 1*/ WriteString("clk")
                }
                WriteString(") begin ");
                repeat { WriteString(obj.name);
                    if (kind == 1) && (obj.a !== nil) { Write("["); Expression(obj.a); Write("]") }
                    WriteString(" <= "); Expression(obj.b); Write(";"); WriteLn(); obj = obj.next
                } while !((obj === LSB.top) || (obj.val != kind))
                WriteString("end"); WriteLn()
            } else { obj = obj.next
            }
        }
    } //ObjList2;
    
    static public func List (_ outFile : String) {
//        var S: Texts.Scanner;
        /* Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S); */
        if !outFile.isEmpty {
            print(LSB.modname, " translating to  ", outFile, terminator: "")
            F = Files1.Open(outFile, mode: "w")!
            WriteString("`timescale 1ns / 1 ps"); WriteLn(); nofgen = 0
            WriteString("module " + LSB.modname + "(   // translated from Lola"); WriteLn()
            ObjList0(LSB.top); ObjList1(LSB.top); ObjList2(LSB.top);
            WriteString("endmodule"); WriteLn()
            Files1.Close(F)
            print(" done")
        }
    } //List;
    
} //LSV.
