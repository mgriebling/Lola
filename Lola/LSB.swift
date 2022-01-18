import Foundation


/// Lola System Base   NW 25.2.95 / 16.4.96 / 21.10.97
class LSB {
    /* type forms */
    public static let bit = 0; public static let array = 1; public static let unit = 2
      
    /* tags in output */
    public static let const = 1; public static let typ = 2; public static let _var = 3;
    public static let lit = 4; public static let sel = 7; public static let range = 8;
    public static let cons = 9; public static let repl = 10; public static let not = 11;
    public static let and = 12; public static let mul = 13; public static let div = 14;
    public static let or = 15; public static let xor = 16; public static let add = 17;
    public static let sub = 18; public static let eql = 20; public static let neq = 21;
    public static let lss = 22; public static let geq = 23; public static let leq = 24;
    public static let gtr = 25; public static let then = 30; public static let _else = 31;
    public static let ts = 32; public static let next = 33;

    public class Item {
        var tag = 0
        var type: TType!
        var val = 0, size = 0
        var a, b: Item!
        
        init(_ tag:Int=0, _ a:Item! = nil, _ b:Item! = nil) {
            self.tag = tag; self.a = a; self.b = b; self.val = b?.val ?? 0
        }
    }

    public class Object : Item {
        var next: Object!
        var name = ""
        var marked = false
        
        init() { super.init() }
        init(tag:Int, name:String, type:TType!, next:Object!) {
            super.init(tag); self.type = type
            self.next = next; self.name = name
        }
    }

    public class TType {
        var len = 0, size = 0; var typobj: Object!
        init(len:Int=0, size:Int=0) { self.len = len; self.size = size; self.typobj = nil }
    }
    public class ArrayType : TType {
        var eltyp: TType!
        init(len:Int, size:Int, type:TType) { self.eltyp = type; super.init(len: len, size: size) }
    }
    public class UnitType : TType {
        var firstobj: Object!
        init() { }
    }

    public static var top : Object = root
    public static var root : Object = {
        // create a list of types
        let word = Object(tag: typ, name: "WORD", type: wordType, next: nil)
        let byte = Object(tag: typ, name: "BYTE", type: byteType, next: word)
        return Object(tag: typ, name: "BIT", type: bitType, next: byte)
    }()
    public static let bitType = TType(len: 0, size: 1),
                      integer = TType(),
                      string = TType()
    public static let byteType = ArrayType(len: 8, size: 8, type: bitType),
                      wordType = ArrayType(len: 32, size: 32, type: bitType)
    public static var modname: String = ""

    public static func Register(_ name: String, _ list: Object) {
        modname = name; top = list
    } // Register

}
