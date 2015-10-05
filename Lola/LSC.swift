import Foundation

class LSC {

    /* Lola System Compiler, NW 8.1.95 / 29.10.96 */
    
    typealias SHORTINT = Int8
    typealias INTEGER = Int
    typealias LONGINT = Int
    typealias CHAR = Character
    typealias BOOLEAN = Bool
	
	static var bit : SHORTINT = 0; static var ts : SHORTINT = 1; static var oc : SHORTINT = 2; static var integer : SHORTINT = 3
    static var array : SHORTINT = 4; static var record : SHORTINT = 5; static var sect : SHORTINT = 6; static var buf : SHORTINT = 7;
    static var not : SHORTINT = 8; static var and : SHORTINT = 9
	static var or : SHORTINT = 10; static var xor : SHORTINT = 11; static var mux : SHORTINT = 12; static var mux1 : SHORTINT = 13;
    static var reg : SHORTINT = 14; static var reg1 : SHORTINT = 15; static var latch : SHORTINT = 16; static var sr : SHORTINT = 17;
    static var tsg : SHORTINT = 18; static var link : SHORTINT = 19
	static var lit : SHORTINT = 20; static var asel : SHORTINT = 21; static var rsel : SHORTINT = 22; static var add : SHORTINT = 23;
    static var sub : SHORTINT = 24; static var neg : SHORTINT = 25; static var mul : SHORTINT = 26; static var div : SHORTINT = 27;
    static var mod : SHORTINT = 28
	static var exp : SHORTINT = 29; static var log : SHORTINT = 30
	static var eql : SHORTINT = 31; static var neq : SHORTINT = 32; static var lss : SHORTINT = 33; static var geq : SHORTINT = 34;
    static var leq : SHORTINT = 35; static var gtr : SHORTINT = 36
	static var assign : SHORTINT = 40; static var tsass : SHORTINT = 41; static var ocass : SHORTINT = 42; static var clkass : SHORTINT = 43;
    static var rstass : SHORTINT = 44; static var posass : SHORTINT = 45
	static var call : SHORTINT = 46; static var if0 : SHORTINT = 47; static var if1 : SHORTINT = 48; static var for0 : SHORTINT = 49;
    static var for1 : SHORTINT = 50; static var for2 : SHORTINT = 51
	static var next : SHORTINT = 52; static var par : SHORTINT = 53; static var const : SHORTINT = 54; static var var0 : SHORTINT = 55;
    static var in0 : SHORTINT = 56; static var out : SHORTINT = 57; static var io : SHORTINT = 58; static var spos : SHORTINT = 59;
    static var type : SHORTINT = 60

    class Item {
        var tag: SHORTINT = LSC.bit; var val: INTEGER = 0
        var a, b: Item?
        
        init(t: SHORTINT, v: INTEGER) { tag = t; val = v }
    }

    class Object : Item {
        var next: Object?
        var name: LSS.Ident = ""
        
        init() {
            super.init(t: LSC.bit, v: 0)
        }
        
        init(t: SHORTINT, a: Item?, next: Object) {
            super.init(t: t, v: 0)
            self.next = next
        }
    }
    
    static var sym: SHORTINT = LSS.zero
    static var globalScope = Object()
    static var localScope = Object()
    static var body: Item?
    
    static var guard0 : Object = Object(t: LSC.var0, a: LSC.bitType, next: LSC.guard0) /* linked lists, end with guard0 */
    
    static var intType = Item(t: LSC.integer, v: 0)
    static var bitType = Item(t: LSC.bit, v: 0)
    static var tsType  = Item(t: LSC.ts, v: 0)
    static var ocType  = Item(t: LSC.oc, v: 0)
    
    static var zero    = Item(t: LSC.lit, v: 0)
    static var one     = Item(t: LSC.lit, v: 1)
    static var clock   = Item(t: LSC.lit, v: 2)
    
    static func NewObj(scope: Object, classv: SHORTINT) -> Object {
        var new, x: Object?
        x = scope; guard0.name = LSS.id;
        while x!.next!.name != LSS.id { x = x!.next }
        if x!.next === guard0 {
            new = Object(); new!.name = LSS.id; new!.tag = classv; new!.next = guard0;
            x!.next = new
        } else { new = x!.next; LSS.Mark(1)
        } //;
        return new!
    } //NewObj;
    
    static func This(scope: Object) -> Object {
        var x: Object?
        guard0.name = LSS.id; x = scope.next
        while x!.name != LSS.id { x = x!.next }
        if x === guard0 { LSS.Mark(0) } //;
        return x!
    } //This;
    
    static func ThisField(var list: Object?) -> Object {
        guard0.name = LSS.id;
        while list!.name != LSS.id { list = list!.next } //;
        if list === guard0 { LSS.Mark(5)
        } else if list!.tag != LSC.out { LSS.Mark(3)
        } //;
        return list!
    } //ThisField;
    
    static func Remove(obj: Object) {
        var x: Object?
        x = localScope;
        while (x!.next !== guard0) && (x!.next !== obj) { x = x!.next } //;
        if x!.next !== guard0 { x!.next = obj.next }
    } //Remove;

	/* -------------------- Parser ---------------------*/

    static func New(tag: SHORTINT, _ a: Item?, _ b: Item?) -> Item {
		let z = Item(t: tag, v: 0)
        z.a = a; z.b = b; return z
	} //New;
	
    static func check(x: Item?) {
        /*verify that type is a signal*/
		if (x != nil) && (x!.tag > LSC.oc) { LSS.Mark(44) }
	} //check;

    static func selector(inout x: Item?, inout _ t: Item?) {
		var obj: Object
        var z, y, ty, tz: Item?
	
		while (sym == LSS.lbrak) || (sym == LSS.period) {
			if sym == LSS.lbrak {
				LSS.Get(&sym); expression(&y, &ty);
				if ty !== intType { LSS.Mark(42) }
				if t!.tag == LSC.array {
					if (y!.tag == LSC.lit) && (t!.b != nil) && (t!.b!.tag == LSC.lit) {
						if (y!.val < 0) || (y!.val >= t!.b!.val) { LSS.Mark(43) }
					}
				} else { LSS.Mark(32)
				} //;
				if sym == LSS.to { /* array section */
					LSS.Get(&sym); expression(&z, &tz);
					if tz !== intType { LSS.Mark(42) } //;
					if (z!.tag == LSC.lit) && (t!.b != nil) && (t!.b!.tag == LSC.lit) {
						if (z!.val < 0) || (z!.val >= t!.b!.val) { LSS.Mark(43) }
					} //;
					x = New(LSC.sect, x, New(LSC.sect, y, z))
				} else { x = New(LSC.asel, x, y)
					if t!.a != nil { t = t!.a }
				} //;
				if sym == LSS.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
			} else { LSS.Get(&sym);
				if t!.tag == LSC.array {
					if sym == LSS.ident {
						obj = This(localScope); LSS.Get(&sym);
						if (obj.tag == LSC.const) || (obj.tag == LSC.par) || (obj.tag == LSC.lit) { x = New(LSC.asel, x, obj); t = t!.a
						} else { LSS.Mark(42)
						}
					} else if sym == LSS.number {
						y = Item(t: LSC.lit, v: LSS.val); LSS.Get(&sym)
						if (y!.val < 0) || (t!.b!.tag == LSC.lit) && (y!.val >= t!.b!.val) { LSS.Mark(43) }
						x = New(LSC.asel, x, y); t = t!.a
					} else { LSS.Mark(12)
					}
				} else if t!.tag == LSC.record {
					y = ThisField((t!.a!.a as! Object)); LSS.Get(&sym); x = New(LSC.rsel, x, y); t = y!.a
				} else { LSS.Mark(34)
				}
			}
		}
	} //selector;
	
    static func factor(inout r: Item?, inout _ t: Item?) {
        var x, y, z: Item?
        t = bitType
		if sym == LSS.ident {
			r = This(localScope); t = r!.a!; LSS.Get(&sym); selector(&r, &t)
		} else if sym == LSS.lparen {
			LSS.Get(&sym); expression(&r, &t);
			if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.number {
			x = Item(t: LSC.lit, v: LSS.val); r = x; t = intType; LSS.Get(&sym)
		} else if sym == LSS.not {
			LSS.Get(&sym); factor(&x, &t); check(t); r = New(LSC.not, nil, x)
		} else if sym == LSS.mux {
			LSS.Get(&sym);
			if sym == LSS.lparen { LSS.Get(&sym) } else { LSS.Mark(14) } //;
			expression(&x, &t); check(t);
			if sym == LSS.colon { LSS.Get(&sym) } else { LSS.Mark(20) } //;
			expression(&y, &t); check(t);
			if sym == LSS.comma { LSS.Get(&sym) } else { LSS.Mark(19) } //;
			expression(&z, &t); check(t);
			r = New(LSC.mux, x, New(LSC.mux1, y, z));
			if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.reg {
			LSS.Get(&sym);
			if sym == LSS.lparen { LSS.Get(&sym) } else { LSS.Mark(14) } //;
			expression(&x, &t); check(t);
			if sym == LSS.colon { LSS.Get(&sym); expression(&y, &t); check(t)
			} else { y = x; x = clock
			} //;
			if sym == LSS.comma { LSS.Get(&sym); expression(&z, &t); check(t)
			} else { z = y; y = one
			} //;
			r = New(LSC.reg, x, New(LSC.reg1, y, z));
			if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.latch {
			LSS.Get(&sym);
			if sym == LSS.lparen { LSS.Get(&sym) } else { LSS.Mark(14) } //;
			expression(&x, &t); check(t);
			if sym == LSS.comma { LSS.Get(&sym) } else { LSS.Mark(19) } //;
			expression(&y, &t); check(t);
			r = New(LSC.latch, x, y);
			if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.sr {
			LSS.Get(&sym);
			if sym == LSS.lparen { LSS.Get(&sym) } else { LSS.Mark(14) } //;
			expression(&x, &t); check(t);
			if sym == LSS.comma { LSS.Get(&sym) } else { LSS.Mark(19) } //;
			expression(&y, &t); check(t);
			r = New(LSC.sr, x, y);
			if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.exp {
			LSS.Get(&sym); factor(&x, &t); r = New(LSC.exp, nil, x)
		} else if sym == LSS.log {
			LSS.Get(&sym); factor(&x, &t); r = New(LSC.log, nil, x)
		} else if sym == LSS.zero { LSS.Get(&sym); r = zero; t = bitType
		} else if sym == LSS.one { LSS.Get(&sym); r = one; t = bitType
		} else { LSS.Mark(17)
		}
	} //factor;

    static func checkT(t0: SHORTINT, _ t1: SHORTINT) {
		if !((t0 <= LSC.oc) && (t1 <= LSC.oc) || (t0 == LSC.integer) && (t1 == LSC.integer)) { LSS.Mark(44) }
	} //checkT;
	
    static func term(inout x: Item?, inout _ t: Item?) {
		var tag: SHORTINT
        var y, yt: Item?
	
		tag = LSC.bit
		factor(&x, &t);
		while (sym >= LSS.times) && (sym <= LSS.mod) {
			if sym == LSS.times {
				if t === intType { tag = LSC.mul } else { tag = LSC.and }
			} else {
				if t !== intType { LSS.Mark(44) } //;
				if sym == LSS.div { tag = LSC.div
				} else if sym == LSS.mod { tag = LSC.mod
				}
			} //;
			LSS.Get(&sym); factor(&y, &yt); checkT(yt!.tag, t!.tag)
			x = New(tag, x, y)
		}
	} //term;
	
    static func expression(inout x: Item?, inout _ t: Item?) {
		var tag: SHORTINT
        var y, yt: Item?
	
		if sym == LSS.minus {
			LSS.Get(&sym); term(&y, &t); x = New(LSC.neg, nil, y);
			if t !== intType { LSS.Mark(44) }
		} else if sym == LSS.plus {
			LSS.Get(&sym); term(&x, &t);
			if t !== intType { LSS.Mark(44) }
		} else { term(&x, &t)
		} //;
		while (sym == LSS.plus) || (sym == LSS.minus) {
			if sym == LSS.plus {
				if t === intType { tag = LSC.add } else { tag = LSC.or }
			} else { /*sym == minus*/
				if t === intType { tag = LSC.sub } else { tag = LSC.xor }
			} //;
			LSS.Get(&sym); term(&y, &yt); checkT(yt!.tag, t!.tag);
			x = New(tag, x, y)
		}
	} //expression;
	
    static func condition(inout x: Item) {
		var tag: SHORTINT
        var y, z, t: Item?
        expression(&y, &t)
		if t !== intType { LSS.Mark(44) } //;
		if (sym >= LSS.eql) && (sym <= LSS.geq) {
			if sym == LSS.eql { tag = LSC.eql
			} else if sym == LSS.neq { tag = LSC.neq
			} else if sym == LSS.lss { tag = LSC.lss
			} else if sym == LSS.geq { tag = LSC.geq
			} else if sym == LSS.leq { tag = LSC.leq
			} else { tag = LSC.gtr
			} //;
			LSS.Get(&sym); expression(&z, &t);
			if t !== intType { LSS.Mark(44) }
			x = New(tag, y, z)
		} else { LSS.Mark(18)
		}
	} //condition;
	
    static func parameter(fp: Object, inout _ par: Item?) {
		var ftyp, eltyp, par0, par1: Item
        var x, y, atyp: Item?
        ftyp = fp.a!
		if (fp.tag != LSC.in0) && (fp.tag != LSC.io) { LSS.Mark(36) } //;
		if sym == LSS.lbrak {
			if ftyp.tag == LSC.array { eltyp = ftyp.a! } else { eltyp = bitType } //;
			LSS.Get(&sym); expression(&y, &atyp); x = New(LSC.next, y, nil); par0 = x!
			while sym == LSS.comma {
				LSS.Get(&sym); expression(&y, &atyp); par1 = New(LSC.next, y, nil); par0.b = par1; par0 = par1;
				if !((atyp === eltyp) || (eltyp.tag == LSC.bit) && (atyp!.tag <= LSC.oc)) { LSS.Mark(40) }
			} //;
			if sym == LSS.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) } //;
			if fp.tag == LSC.in0 {
				if ftyp.tag != LSC.array { LSS.Mark(40) }
			} else { LSS.Mark(36)
			}
		} else { expression(&x, &atyp);
			if atyp != nil {
				while (atyp!.tag == LSC.array) && (ftyp.tag == LSC.array) { atyp = atyp!.a; ftyp = ftyp.a! } //;
				if !((atyp === ftyp) || (ftyp.tag == LSC.bit) && (atyp!.tag <= LSC.oc)) { LSS.Mark(40) }
			} else { LSS.Mark(36)
			}
		} //;
		par = New(LSC.next, x, nil)
	} //parameter;

    static func position(vtyp: Item, inout _ x: Item?) {
        var z, y, t: Item?
        
		if sym == LSS.lbrak {
			LSS.Get(&sym);
			if vtyp.tag == LSC.array {
				if sym != LSS.rbrak {
					position(vtyp.a!, &z); y = New(LSC.next, z, nil); x = y;
					while sym == LSS.semicolon {
						LSS.Get(&sym); position(vtyp.a!, &z); y!.b = New(LSC.next, z, nil); y = y!.b
					} //;
					if sym == LSS.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
				} else { x = nil
				}
			} else { LSS.Mark(32); x = nil;
				while sym != LSS.rbrak { LSS.Get(&sym) }
			}
		} else { expression(&z, &t);
			if t!.tag != LSC.integer { LSS.Mark(42) }
			y = New(LSC.posass, z, nil); x = y;
			while sym == LSS.comma {
				LSS.Get(&sym); expression(&z, &t);
				if t!.tag != LSC.integer { LSS.Mark(42) }
				y!.b = New(LSC.posass, z, nil); y = y!.b
			}
		}
	} //position;

    static func StatSequence(inout seq: Item?) {
		var tag: SHORTINT; var fp, obj: Object?
        var x, y, z, s, ss, xtyp, ytyp, ztyp, ap: Item?

        func IfPart(inout s: Item?) {
            var x = Item(t: LSC.bit, v: 0); var y, z: Item?
            condition(&x); y = nil; z = nil
			if sym == LSS.then { LSS.Get(&sym) } else { LSS.Mark(22) } //;
			StatSequence(&y)
			if sym == LSS.elsif { LSS.Get(&sym); IfPart(&z); z = New(LSC.next, z, nil)
			} else if sym == LSS.Else { LSS.Get(&sym); StatSequence(&z)
			} else { z = nil
			} //;
			s = New(LSC.if0, x, New(LSC.if1, y, z))
		} //IfPart;

        ss = nil; tag = LSC.bit
        repeat {
            s = nil; /* obj = guard0; */
			if sym < LSS.ident { LSS.Mark(24);
				repeat { LSS.Get(&sym) } while !(sym >= LSS.ident)
			} //;
			if sym == LSS.ident {
				x = This(localScope); LSS.Get(&sym); xtyp = x!.a!; y = x; ytyp = xtyp
				selector(&x, &xtyp);
				if sym == LSS.becomes {
					if ytyp!.tag == LSC.record { LSS.Mark(46) } //;
					z = x;
					while z!.tag == LSC.asel { z = z!.a } //;
					if z!.tag == LSC.in0 { LSS.Mark(47) } //;
					check(xtyp); LSS.Get(&sym); expression(&y, &ytyp); check(ytyp);
					if sym == LSS.bar {
						LSS.Get(&sym); expression(&z, &ztyp); check(ztyp); y = New(LSC.tsg, y, z); tag = LSC.tsass
						if xtyp !== tsType { LSS.Mark(49) } //;
					} else if xtyp === bitType { tag = LSC.assign
					} else if xtyp === tsType { LSS.Mark(48); tag = LSC.assign
					} else if xtyp === ocType { tag = LSC.ocass
					} //;
					s = New(tag, x, y)
				} else if sym == LSS.pos {
					if (x !== y) && (ytyp!.tag == LSC.record) { LSS.Mark(46) }
					LSS.Get(&sym); position(xtyp!, &y); s = New(LSC.posass, x, y)
				} else if sym == LSS.lparen {
					LSS.Get(&sym);
					if xtyp!.tag == LSC.record {
						fp = (xtyp!.a!.a as! Object); s = New(LSC.call, x, nil);
						while fp!.tag < LSC.var0 { fp = fp!.next }
						if sym != LSS.rparen {
							parameter(fp!, &ap); s!.b = ap; fp = fp!.next;
							while sym == LSS.comma {
								LSS.Get(&sym); parameter(fp!, &y); ap!.b = y; ap = y; fp = fp!.next
							} //;
							if (fp!.tag == LSC.in0) || (fp!.tag == LSC.io) { LSS.Mark(35) } //;
							ap!.b = xtyp!.a
						} else { s!.b = xtyp!.a
						} //;
						if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					} else { LSS.Mark(37); expression(&x, &xtyp);
						while sym == LSS.comma { LSS.Get(&sym); expression(&x, &xtyp) }
					}
				} else { LSS.Mark(21)
				}
			} else if sym == LSS.If {
				LSS.Get(&sym); IfPart(&s)
				if sym == LSS.end { LSS.Get(&sym) } else { LSS.Mark(26) }
			} else if sym == LSS.For {
				LSS.Get(&sym);
				if sym == LSS.ident {
					obj = NewObj(localScope, classv: LSC.par); obj!.a = intType; LSS.Get(&sym);
					if sym == LSS.becomes { LSS.Get(&sym) } else { LSS.Mark(21) } //;
					expression(&x, &xtyp);
					if xtyp !== intType { LSS.Mark(39) } //;
					if sym == LSS.to { LSS.Get(&sym) } else { LSS.Mark(23) } //;
					expression(&y, &ytyp);
					if ytyp !== intType { LSS.Mark(39) } //;
					if sym == LSS.Do { LSS.Get(&sym) } else { LSS.Mark(27) } //;
					z = nil; StatSequence(&z);
					s = New(LSC.for0, obj, New(LSC.for1, x, New(LSC.for2, y, z)));
					Remove(obj!)
					if sym == LSS.end { LSS.Get(&sym) } else { LSS.Mark(26) }
				}
			} //;
			if s != nil { ss = New(LSC.next, s, ss) } //;
			if sym == LSS.semicolon { LSS.Get(&sym)
			} else if (sym <= LSS.ident) || (sym == LSS.If) || (sym == LSS.For) { LSS.Mark(24)
			} else { break
			}
		} while true
		x = nil; /*invert list*/
		while ss != nil { y = ss!.b; ss!.b = x; x = ss; ss = y } //;
		seq = x
	} //StatSequence;

	/*---------------------------------------------------*/
	
    static func IdentList(classv: SHORTINT, inout _ first: Object?) {
        first = NewObj(localScope, classv: classv); LSS.Get(&sym);
		while sym == LSS.comma {
			LSS.Get(&sym);
			if sym == LSS.ident { NewObj(localScope, classv: classv); LSS.Get(&sym)
			} else { LSS.Mark(10)
			}
		}
	} //IdentList;

    static func TypParam(fp: Object?, inout _ typ: Item?) {
		var x, t: Item?
		if fp!.tag != LSC.par { LSS.Mark(36) } //;
		expression(&x, &t);
		if t !== intType { LSS.Mark(39) } //;
		typ = New(LSC.next, x, nil)
	} //TypParam;

    static func Type(inout x: Item?, _ forms: Set<SHORTINT>) {
		var t, y, z: Item?; var obj, fp: Object?
	
		if sym == LSS.lbrak {
			LSS.Get(&sym); expression(&z, &t);
			if t !== intType { LSS.Mark(39) } //;
			if sym == LSS.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) } //;
			Type(&y, forms); x = New(LSC.array, y, z);
		} else {
			if sym == LSS.ident {
				obj = This(globalScope); LSS.Get(&sym);
				if obj!.tag == LSC.type {
					fp = (obj!.a as! Object); x = New(LSC.record, obj, nil);
					if sym == LSS.lparen {
						LSS.Get(&sym); TypParam(fp, &z); x!.b = z; fp = fp!.next;
						while sym == LSS.comma {
							LSS.Get(&sym); TypParam(fp, &y); z!.b = y; z = y; fp = fp!.next
						} //;
						if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					}
				} else { fp = guard0; LSS.Mark(37);
					if sym == LSS.lparen {
						LSS.Get(&sym); expression(&y, &t);
						while sym == LSS.comma { LSS.Get(&sym); expression(&y, &t) } //;
						if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					}
				} //;
				if fp!.tag == LSC.par { LSS.Mark(38) }
			} else if sym == LSS.bit { LSS.Get(&sym); x = bitType
			} else if sym == LSS.ts { LSS.Get(&sym); x = tsType
			} else if sym == LSS.oc { LSS.Get(&sym); x = ocType
			} else { LSS.Mark(28); x = bitType
			} //;
			if !forms.contains(x!.tag) { LSS.Mark(28) }
		}
	} //Type;

    static func FormalType(inout x: Item?, _ forms: Set<SHORTINT>) {
		var y, z, t: Item?
	
		if sym == LSS.lbrak {
			LSS.Get(&sym); expression(&z, &t);
			if sym == LSS.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) } //;
			FormalType(&y, forms); x = New(LSC.array, y, z)
		} else {
			if sym == LSS.bit { x = bitType; LSS.Get(&sym)
			} else if sym == LSS.ts { x = tsType; LSS.Get(&sym)
			} else if sym == LSS.oc { x = ocType; LSS.Get(&sym)
			} else { LSS.Mark(28); x = bitType
			} //;
			if !forms.contains(x!.tag) { LSS.Mark(28) }
		}
	} //FormalType;

    static func ConstDecl () {
		var obj: Object;
	
		while sym == LSS.ident {
			obj = NewObj(localScope, classv: LSC.const); LSS.Get(&sym);
			if (sym == LSS.becomes) || (sym == LSS.eql) { LSS.Get(&sym)
			} else { LSS.Mark(21)
			} //;
			expression(&obj.b, &obj.a);
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //ConstDecl;

    static func PosDecl () {
		var p: Object; var x, r, t: Item?
	
		while sym == LSS.ident {
			/* TBD - need to find out how to pass position to front end */
			p = This(localScope); t = p.a; LSS.Get(&sym); selector(&r, &t);
			if sym == LSS.eql { LSS.Get(&sym)
			} else { LSS.Mark(29)
			};
			expression(&x, &t);
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //PosDecl;

    static func ParDecl(classv: SHORTINT, _ forms: Set<SHORTINT>) {
        var list: Object?; var obj: Object?; var type: Item?
	
		while sym == LSS.ident {
			IdentList(classv, &list);
			if sym == LSS.colon { LSS.Get(&sym) } else { LSS.Mark(20) } //;
			FormalType(&type, forms)
			obj = list;
			while obj !== guard0 { obj!.a = type; obj = obj!.next }
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //ParDecl;

    static func VarDecl(classv: SHORTINT, _ forms: Set<SHORTINT>) {
        var list: Object?; var obj: Object?; var type: Item?
	
		while sym == LSS.ident {
			IdentList(classv, &list);
			if sym == LSS.colon { LSS.Get(&sym) } else { LSS.Mark(20) } //;
			Type(&type, forms)
			obj = list
			while obj !== guard0 { obj!.a = type; obj = obj!.next } //;
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //VarDecl;

    static func TypeDecl() {
		var obj, this: Object?
        LSS.Get(&sym)
		obj = nil;
		if sym == LSS.ident {
			this = NewObj(globalScope, classv: LSC.type); LSS.Get(&sym);
			if sym == LSS.times { LSS.Get(&sym) } //;
			localScope.next = guard0; /*start new scope*/
			if sym == LSS.lparen {
				LSS.Get(&sym);
				if sym == LSS.ident { IdentList(LSC.par, &obj) } //;
				if sym == LSS.rparen { LSS.Get(&sym) } else { LSS.Mark(15) } //;
				while obj !== guard0 { obj!.a = intType; obj = obj!.next }
			} //;
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) };
			if sym == LSS.const { LSS.Get(&sym); ConstDecl() } //;
			if sym == LSS.In { LSS.Get(&sym); ParDecl(LSC.in0, [LSC.bit]) } //;
			if sym == LSS.Inout { LSS.Get(&sym); ParDecl(LSC.io, [LSC.ts, LSC.oc]) } //;
			if sym == LSS.out { LSS.Get(&sym); VarDecl(LSC.out, [LSC.bit]) } //;
			if sym == LSS.Var { LSS.Get(&sym); VarDecl(LSC.var0, [LSC.bit, LSC.ts, LSC.oc, LSC.record]) } //;
			this!.a = localScope.next;
			if sym == LSS.begin { LSS.Get(&sym); StatSequence(&this!.b) } //;
			if sym == LSS.end { LSS.Get(&sym) } else { LSS.Mark(26) } //;
			if sym == LSS.ident {
				if LSS.id != this!.name { LSS.Mark(4) } //;
				LSS.Get(&sym)
			} else { LSS.Mark(10)
			}
		}
	} //TypeDecl;

    static func Module (name: String) {
		var t, clk, rst: Item?
        print("compiling Lola ", terminator: "")
		globalScope.next = guard0; body = nil; clk = nil;
		LSS.Init(name); LSS.Get(&sym);
		if sym == LSS.module {
			LSS.Get(&sym);
			if sym == LSS.ident {
				globalScope.name = LSS.id; LSS.Get(&sym);
				print(LSS.id)
			} else { LSS.Mark(10)
			} //;
			if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) } //;
			while sym == LSS.type {
				TypeDecl()
				if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} //;
			localScope.next = guard0;
			if sym == LSS.const { LSS.Get(&sym); ConstDecl() } //;
			if sym == LSS.In { LSS.Get(&sym); VarDecl(LSC.in0, [LSC.bit]) } //;
			if sym == LSS.Inout { LSS.Get(&sym); VarDecl(LSC.io, [LSC.ts, LSC.oc]) } //;
			if sym == LSS.out { LSS.Get(&sym); VarDecl(LSC.out, [LSC.bit]) } //;
			if sym == LSS.Var { LSS.Get(&sym); VarDecl(LSC.var0, [LSC.bit, LSC.ts, LSC.oc, LSC.record]) } //;
			if sym == LSS.pos { LSS.Get(&sym); PosDecl() };
			if sym == LSS.clock {
				LSS.Get(&sym); expression(&clk, &rst); check(t)
				if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} //;
			if sym == LSS.reset {
				LSS.Get(&sym); expression(&rst, &t); check(t)
				if sym == LSS.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} else { rst = nil
			} //;
			if sym == LSS.begin { LSS.Get(&sym); StatSequence(&body) } //;
			if sym == LSS.end { LSS.Get(&sym) } else { LSS.Mark(26) } //;
			if sym == LSS.ident {
				if LSS.id != globalScope.name { LSS.Mark(4) } //;
				LSS.Get(&sym)
			} else { LSS.Mark(10)
			} //;
			if sym != LSS.period { LSS.Mark(25) } //;
			if rst != nil { body = New(LSC.next, New(LSC.rstass, rst, nil), body) } //;
			if clk != nil { body = New(LSC.next, New(LSC.clkass, clk, nil), body) }
		} else { LSS.Mark(11)
		} //;
		if LSS.error { globalScope.next = guard0; localScope.next = guard0; body = nil }
	} //Module;

} //LSC.
