
class Lola {
    /* MG 17.8.2014, NW 2.2.95 / 7.11.96 / 21.10.97 */
    //	IMPORT LSB, LSC, LST, Out, Args;

    /*
    static var scope: LSB.Variable?; static var clk: LSB.Signal?
    static var null: String = ""
    
    static func OutString(_ s: String)  { print(s, terminator:"") }
    
    static func Log(_ x: Int) -> Int {
        var y, e: Int
        e = 0; y = 1
        while x > y { y = 2*y; e += 1 }
        return e
    } // Log
    
    static func V(_ x: LSC.Item?) -> Int {
        var y: Int = 0; var v: LSB.Variable?; var id: LSB.Name
        if x is LSC.Object {
            v = scope; id = (x as! LSC.Object).name
            while v!.name != id { v = v!.next }
            y = Int(v!.val)
        } else {
            switch x!.tag {
            case LSC.lit: y = x!.val
            case LSC.add: y = V(x!.a) + V(x!.b)
            case LSC.sub: y = V(x!.a) - V(x!.b)
            case LSC.neg: y = -V(x!.b)
            case LSC.mul: y = V(x!.a) * V(x!.b)
            case LSC.div: y = V(x!.a) / V(x!.b)
            case LSC.mod: y = V(x!.a) % V(x!.b)
            case LSC.exp: y = (1 << V(x!.b))       // ASH
            case LSC.log: y = Log(V(x!.b))
            default: break
            }
        }
        return y
    } // V
    
    static func Index(_ n: Int, _ name: inout String) {
        var i, j: Int
        var n = n
        var d = [Int](repeating: 0, count: 4)
        i = 0; j = 0; name = ""
        repeat { d[i] = n % 10; i += 1; n = n / 10 } while n != 0
        repeat { i -= 1; name.append(Character(d[i] + 0x30)); j += 1 } while i != 0
    } // Index
    
    static func NewVar(_ typ: LSC.Item, _ mode: Int, _ id: String, _ link: LSB.Variable?, _ anc: LSB.Variable) -> LSB.Variable {
        var form: Int8; var len: Int
        var fp: LSC.Object?; var ap: LSC.Item?
        var v, tempscope: LSB.Variable
        var x, el, y: LSB.Variable?
        form = typ.tag
        v = LSB.NewVar(form, LSB.black, nil, anc, link, id); v.classv = Int8(mode) - LSC.var0
        if form == LSB.array {
            len = V(typ.b); v.val = Int8(len); el = nil;
            while len > 0 { len -= 1; el = NewVar(typ.a!, mode, null, el, v); Index(len, &el!.name) } // ;
            v.dsc = el
        } else if form == LSB.record {
            v.name = id; el = nil
            ap = typ.b; fp = (typ.a!.a as! LSC.Object)
            while ap != nil {
                el = LSB.NewVar(LSB.integer, Int8(V(ap!.a)), nil, v, el, fp!.name); fp = fp!.next; ap = ap!.b
            }
            tempscope = scope!; scope = el
            while fp!.tag == LSC.const {
                el = LSB.NewVar(LSB.integer, Int8(V(fp!.b)), nil, v, el, fp!.name); scope = el; fp = fp!.next
            }
            while fp !== LSC.guard0 { el = NewVar(fp!.a!, Int(fp!.tag), fp!.name, el, v); fp = fp!.next }
            y = nil;  /*invert list*/
            while el != nil { x = el; el = x!.next; x!.next = y; y = x }
            scope = tempscope; v.dsc = y
        }
        return v
    } // NewVar
    
    static func E(_ x: LSC.Item) -> LSB.Signal {
        var y = LSB.Signal(); var u, v: LSB.Variable?
        var k, n: Int; var id: LSB.Name
        /* tag = x.tag; */
        if x is LSC.Object {
            v = scope!; id = (x as! LSC.Object).name
            while v!.name != id { v = v!.next }
            while v!.classv == LSB.IO && v!.x != nil && v!.x is LSB.Variable { v = (v!.x as! LSB.Variable) }
            if v!.classv == LSB.In && v!.x != nil { y = v!.x! } else { y = v! } //  /*elim in params*/
        } else {
            switch x.tag {
            case LSC.asel:
                y = E(x.a!); v = (y as! LSB.Variable).dsc; k = V(x.b)
                while (k > 0) && (v != nil) { v = v!.next; k -= 1 }
                if (v == nil) || (k < 0) {
                    v = (y as! LSB.Variable); OutString("index off range in ")
                    LSB.WriteName(v!); print(""); v = v!.dsc
                }
                y = v!
            case LSC.rsel:
                y = E(x.a!); v = (y as! LSB.Variable).dsc; id = (x.b as! LSC.Object).name
                while v!.name != id { v = v!.next } // ;
                y = v!
            case LSC.sect:
                y = E(x.a!); k = V(x.b!.a); n = V(x.b!.b) - k + 1;  /*nof elems*/
                v = (y as! LSB.Variable).dsc;
                while (k > 0) && (v != nil) { v = v!.next; k -= 1 } // ;
                if (v == nil) || (k < 0) {
                    v = (y as! LSB.Variable); OutString("index off range in ")
                    LSB.WriteName(v!); print(""); v = v!.dsc
                } // ;
                u = LSB.Variable(); u!.fct = LSB.array; u!.val = Int8(n); u!.dsc = v; u!.name = "***"; y = u!
            case LSC.and, LSC.or, LSC.xor, LSC.mux, LSC.mux1, LSC.reg, LSC.reg1, LSC.latch, LSC.sr, LSC.tsg:
                y = LSB.New(Int8(x.tag), E(x.a!), E(x.b!))
            case LSC.not, LSC.buf:
                y = LSB.New(Int8(x.tag), nil, E(x.b!))
            case LSC.lit:
                if x.val == 0 { y = LSB.zero } else if x.val == 1 { y = LSB.one } else { y = clk! }
            default:
                break
            }
        }
        return y
    } // E
    
    static func Link(_ fp: LSB.Variable, _ ap: LSB.Signal) {
        var fel, ael: LSB.Variable?; var n: Int;
        
        if fp.x != nil {
            OutString("mult def "); LSB.WriteName(fp); print("")
        } else { fp.x = ap;
            if fp.fct == LSB.array {
                if fp.val == ap.val {  /*lengths*/
                    fel = fp.dsc; ael = (ap as! LSB.Variable).dsc; n = Int(fp.val)
                    while n > 0 { Link(fel!, ael!); ael = ael!.next; fel = fel!.next; n -= 1 }
                } else {
                    OutString("array mismatch "); LSB.WriteName(fp); print("")
                }
            }
        }
    } // Link
    
    static func AssignPos(_ v: LSB.Variable?, _ anc: LSB.Signal, _ x: LSC.Item?) {
        var pos: Int
        var v = v
        var x = x

        if x!.tag == LSC.next { /*list*/
            v = v!.dsc
            while v != nil && x != nil { AssignPos(v, anc, x!.a); v = v!.next; x = x!.b }
        } else { pos = V(x!.a)
            if pos < 0 { v!.u = -1 } else { v!.u = Int8(pos) + anc.u }
            if x!.b != nil { pos = V(x!.b!.a);
                if pos < 0 { v!.v = -1 } else { v!.v = Int8(pos) + anc.v }
            }
        }
    } // AssignPos
    
    static func S(_ s: LSC.Item?) {
        var s = s
        var tag, lim, u, v: Int
        var cond, cv: LSC.Item;
        var x, ael, ap: LSC.Item?
        var y: LSB.Signal
        var tempscope: LSB.Variable
        var fp, fel: LSB.Variable?
        
        while s != nil {
            x = s!.a; s = s!.b; tag = Int(x!.tag)
            switch Int8(tag) {
            case LSC.assign:
                y = E(x!.a!);
                if y.x != nil {
                    OutString("mult ass "); LSB.WriteName((y as! LSB.Variable));  print("")
                }
                y.x = E(x!.b!)
            case LSC.tsass, LSC.ocass:
                y = E(x!.a!); y.x = LSB.New(LSB.link, E(x!.b!), y.x)
            case LSC.clkass: clk = E(x!.a!)
            case LSC.rstass: LSB.reset = E(x!.a!)
            case LSC.posass: y = E(x!.a!); AssignPos((y as! LSB.Variable), y.y!, x!.b)
            case LSC.if0:
                cond = x!.a!; x = x!.b
                u = V(cond.a); v = V(cond.b)
                switch cond.tag {
                case LSC.eql: if u == v { S(x!.a) } else { S(x!.b) }
                case LSC.neq: if u != v { S(x!.a) } else { S(x!.b) }
                case LSC.lss: if u < v { S(x!.a) } else { S(x!.b) }
                case LSC.geq: if u >= v { S(x!.a) } else { S(x!.b) }
                case LSC.leq: if u <= v { S(x!.a) } else { S(x!.b) }
                case LSC.gtr: if u > v { S(x!.a) } else { S(x!.b) }
                default: break
                } //
            case LSC.for0:
                cv = x!.a!; x = x!.b;
                scope = LSB.NewVar(LSB.integer, Int8(V(x!.a)), nil, nil, scope!, (cv as! LSC.Object).name);
                x = x!.b; lim = V(x!.a);
                while scope!.val <= Int8(lim) { S(x!.b); scope!.val += 1 }
                scope = scope!.next
            case LSC.call:
                y = E(x!.a!); fp = (y as! LSB.Variable).dsc; ap = x!.b
                if fp != nil {
                    while fp!.fct == LSB.integer { fp = fp!.next }
                    while ap != nil && ap!.tag != LSC.type {
                        if ap!.a!.tag == LSC.next { /*array constructor*/
                            ael = ap!.a; fel = fp!.dsc
                            while ael != nil && fel != nil {
                                Link(fel!, E(ael!.a!)); ael = ael!.b; fel = fel!.next
                            }
                            if fel != nil || ael != nil {
                                OutString("array constructor mismatch "); LSB.WriteName(fp!);  print("")
                            }
                        } else { Link(fp!, E(ap!.a!))
                        }
                        fp = fp!.next; ap = ap!.b
                    }
                }
                tempscope = scope!; scope = (y as! LSB.Variable).dsc;
                S(ap!.b); scope = tempscope
            default: break
            }
        }
    } // S

    
    /* -------------------------------------------------*/
    
    static func Generate () {
        var obj: LSC.Object?
        var root, new: LSB.Variable?
        
        if LSC.body != nil {
            print("generating DS \(LSC.globalScope.name)")
            LSB.reset = nil;
            root = LSB.Variable(name: LSC.globalScope.name); root!.fct = LSB.record
            obj = LSC.localScope.next; new = nil
            while (obj !== LSC.guard0) && (obj!.tag == LSC.const) {
                new = LSB.NewVar(LSB.integer, Int8(V(obj!.b)), nil, nil, new, obj!.name)
                scope = new; obj = obj!.next
            }
            while obj !== LSC.guard0 {
                new = NewVar(obj!.a!, Int(obj!.tag), obj!.name, new, root!); obj = obj!.next
            }
            scope = new; clk = LSB.clk; S(LSC.body); root!.dsc = new; LSB.Assign(root!)
            LSB.Simplify(LSB.org!)
            LSB.Loops(LSB.org)
        } //
    } // Generate;

    
    static func Show() {
        if LSB.org !== nil { LSB.Show(LSB.org) }
    } // Show;
     */
    
    static public func Compile (_ Args: [String]) {
        print("Arg count = ", Args.count)
        var i = 1
        while i < Args.count {
            let name = Args[i]; i+=1
            print("Arg = ", name)
            LSC.Module(name); LSP.List() // Generate(); Show()
            if i < Args.count {
                let outFile = Args[i]; i+=1
                LSV.List(outFile)
            }
            // LST.Export(LSC.globalScope.name)
            // Simulator.Interactive()
        }
    } // Compile
    
} // Lola
