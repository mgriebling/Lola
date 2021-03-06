import Foundation

class LSC {

    /* Lola System Compiler, NW 8.1.95 / 29.10.96 */
    
    typealias SHORTINT = Int8
    typealias INTEGER = Int
    typealias LONGINT = Int
    typealias CHAR = Character
    typealias BOOLEAN = Bool
	
	static let bit : SHORTINT = 0; static let ts : SHORTINT = 1; static let oc : SHORTINT = 2; static let integer : SHORTINT = 3
    static let array : SHORTINT = 4; static let record : SHORTINT = 5; static let sect : SHORTINT = 6; static let buf : SHORTINT = 7
    static let not : SHORTINT = 8; static let and : SHORTINT = 9
	static let or : SHORTINT = 10; static let xor : SHORTINT = 11; static let mux : SHORTINT = 12; static let mux1 : SHORTINT = 13
    static let reg : SHORTINT = 14; static let reg1 : SHORTINT = 15; static let latch : SHORTINT = 16; static let sr : SHORTINT = 17
    static let tsg : SHORTINT = 18; static let link : SHORTINT = 19
	static let lit : SHORTINT = 20; static let asel : SHORTINT = 21; static let rsel : SHORTINT = 22; static let add : SHORTINT = 23
    static let sub : SHORTINT = 24; static let neg : SHORTINT = 25; static let mul : SHORTINT = 26; static let div : SHORTINT = 27
    static let mod : SHORTINT = 28
	static let exp : SHORTINT = 29; static let log : SHORTINT = 30
	static let eql : SHORTINT = 31; static let neq : SHORTINT = 32; static let lss : SHORTINT = 33; static let geq : SHORTINT = 34
    static let leq : SHORTINT = 35; static let gtr : SHORTINT = 36
	static let assign : SHORTINT = 40; static let tsass : SHORTINT = 41; static let ocass : SHORTINT = 42; static let clkass : SHORTINT = 43
    static let rstass : SHORTINT = 44; static let posass : SHORTINT = 45
	static let call : SHORTINT = 46; static let if0 : SHORTINT = 47; static let if1 : SHORTINT = 48; static let for0 : SHORTINT = 49
    static let for1 : SHORTINT = 50; static let for2 : SHORTINT = 51
	static let next : SHORTINT = 52; static let par : SHORTINT = 53; static let const : SHORTINT = 54; static let var0 : SHORTINT = 55
    static let in0 : SHORTINT = 56; static let out : SHORTINT = 57; static let io : SHORTINT = 58; static let spos : SHORTINT = 59
    static let type : SHORTINT = 60

    class Item {
        var tag = LSC.bit; var val: INTEGER = 0
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
    
    static var sym = LSS.Symbols.zero
    static var globalScope = Object()
    static var localScope = Object()
    static var body: Item?
    
    static let guard0 : Object = Object(t: LSC.var0, a: LSC.bitType, next: LSC.guard0) /* linked lists, end with guard0 */
    
    static let intType = Item(t: LSC.integer, v: 0)
    static let bitType = Item(t: LSC.bit, v: 0)
    static let tsType  = Item(t: LSC.ts, v: 0)
    static let ocType  = Item(t: LSC.oc, v: 0)
    
    static let zero    = Item(t: LSC.lit, v: 0)
    static let one     = Item(t: LSC.lit, v: 1)
    static let clock   = Item(t: LSC.lit, v: 2)
    
    static func NewObj(_ scope: Object, classv: SHORTINT) -> Object {
        var new, x: Object?
        x = scope; guard0.name = LSS.id
        while x!.next!.name != LSS.id { x = x!.next }
        if x!.next === guard0 {
            new = Object(); new!.name = LSS.id; new!.tag = classv; new!.next = guard0
            x!.next = new
        } else { new = x!.next; LSS.Mark(1)
        }
        return new!
    } //NewObj
    
    static func This(_ scope: Object) -> Object {
        var x: Object?
        guard0.name = LSS.id; x = scope.next
        while x!.name != LSS.id { x = x!.next }
        if x === guard0 { LSS.Mark(0) }
        return x!
    } //This
    
    static func ThisField(_ list: Object?) -> Object {
        var list = list
        guard0.name = LSS.id
        while list!.name != LSS.id { list = list!.next }
        if list === guard0 { LSS.Mark(5)
        } else if list!.tag != LSC.out { LSS.Mark(3)
        }
        return list!
    } //ThisField;
    
    static func Remove(_ obj: Object) {
        var x: Object?
        x = localScope
        while x!.next !== guard0 && x!.next !== obj { x = x!.next }
        if x!.next !== guard0 { x!.next = obj.next }
    } //Remove

	/* -------------------- Parser ---------------------*/

    static func New(_ tag: SHORTINT, _ a: Item?, _ b: Item?) -> Item {
		let z = Item(t: tag, v: 0)
        z.a = a; z.b = b; return z
	} //New
	
    static func check(_ x: Item?) {
        /*verify that type is a signal*/
		if x != nil && x!.tag > LSC.oc { LSS.Mark(44) }
	} //check

    static func selector(_ x: inout Item?, _ t: inout Item?) {
		var obj: Object
        var z, y, ty, tz: Item?
	
		while sym == LSS.Symbols.lbrak || sym == LSS.Symbols.period {
			if sym == LSS.Symbols.lbrak {
				LSS.Get(&sym); expression(&y, &ty)
				if ty !== intType { LSS.Mark(42) }
				if t!.tag == LSC.array {
					if (y!.tag == LSC.lit) && (t!.b != nil) && (t!.b!.tag == LSC.lit) {
						if (y!.val < 0) || (y!.val >= t!.b!.val) { LSS.Mark(43) }
					}
				} else { LSS.Mark(32)
				}
				if sym == LSS.Symbols.to { /* array section */
					LSS.Get(&sym); expression(&z, &tz)
					if tz !== intType { LSS.Mark(42) }
					if z!.tag == LSC.lit && t!.b != nil && t!.b!.tag == LSC.lit {
						if z!.val < 0 || z!.val >= t!.b!.val { LSS.Mark(43) }
					}
					x = New(LSC.sect, x, New(LSC.sect, y, z))
				} else { x = New(LSC.asel, x, y)
					if t!.a != nil { t = t!.a }
				}
				if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
			} else { LSS.Get(&sym);
				if t!.tag == LSC.array {
					if sym == LSS.Symbols.ident {
						obj = This(localScope); LSS.Get(&sym);
						if obj.tag == LSC.const || obj.tag == LSC.par || obj.tag == LSC.lit { x = New(LSC.asel, x, obj); t = t!.a
						} else { LSS.Mark(42)
						}
					} else if sym == LSS.Symbols.number {
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
	} //selector
	
    static func factor(_ r: inout Item?, _ t: inout Item?) {
        var x, y, z: Item?
        t = bitType
		if sym == LSS.Symbols.ident {
			r = This(localScope); t = r!.a!; LSS.Get(&sym); selector(&r, &t)
		} else if sym == LSS.Symbols.lparen {
			LSS.Get(&sym); expression(&r, &t);
			if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.Symbols.number {
			x = Item(t: LSC.lit, v: LSS.val); r = x; t = intType; LSS.Get(&sym)
		} else if sym == LSS.Symbols.not {
			LSS.Get(&sym); factor(&x, &t); check(t); r = New(LSC.not, nil, x)
		} else if sym == LSS.Symbols.mux {
			LSS.Get(&sym)
			if sym == LSS.Symbols.lparen { LSS.Get(&sym) } else { LSS.Mark(14) }
			expression(&x, &t); check(t)
			if sym == LSS.Symbols.colon { LSS.Get(&sym) } else { LSS.Mark(20) }
			expression(&y, &t); check(t)
			if sym == LSS.Symbols.comma { LSS.Get(&sym) } else { LSS.Mark(19) }
			expression(&z, &t); check(t)
			r = New(LSC.mux, x, New(LSC.mux1, y, z))
			if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.Symbols.reg {
			LSS.Get(&sym)
			if sym == LSS.Symbols.lparen { LSS.Get(&sym) } else { LSS.Mark(14) }
			expression(&x, &t); check(t)
			if sym == LSS.Symbols.colon { LSS.Get(&sym); expression(&y, &t); check(t)
			} else { y = x; x = clock
			}
			if sym == LSS.Symbols.comma { LSS.Get(&sym); expression(&z, &t); check(t)
			} else { z = y; y = one
			}
			r = New(LSC.reg, x, New(LSC.reg1, y, z))
			if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.Symbols.latch {
			LSS.Get(&sym)
			if sym == LSS.Symbols.lparen { LSS.Get(&sym) } else { LSS.Mark(14) }
			expression(&x, &t); check(t)
			if sym == LSS.Symbols.comma { LSS.Get(&sym) } else { LSS.Mark(19) }
			expression(&y, &t); check(t)
			r = New(LSC.latch, x, y)
			if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.Symbols.sr {
			LSS.Get(&sym)
			if sym == LSS.Symbols.lparen { LSS.Get(&sym) } else { LSS.Mark(14) }
			expression(&x, &t); check(t);
			if sym == LSS.Symbols.comma { LSS.Get(&sym) } else { LSS.Mark(19) }
			expression(&y, &t); check(t)
			r = New(LSC.sr, x, y);
			if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
		} else if sym == LSS.Symbols.exp {
			LSS.Get(&sym); factor(&x, &t); r = New(LSC.exp, nil, x)
		} else if sym == LSS.Symbols.log {
			LSS.Get(&sym); factor(&x, &t); r = New(LSC.log, nil, x)
		} else if sym == LSS.Symbols.zero { LSS.Get(&sym); r = zero; t = bitType
		} else if sym == LSS.Symbols.one { LSS.Get(&sym); r = one; t = bitType
		} else { LSS.Mark(17)
		}
	} //factor

    static func checkT(_ t0: SHORTINT, _ t1: SHORTINT) {
		if !(t0 <= LSC.oc && t1 <= LSC.oc || t0 == LSC.integer && t1 == LSC.integer) { LSS.Mark(44) }
	} //checkT
	
    static func term(_ x: inout Item?, _ t: inout Item?) {
		var tag: SHORTINT
        var y, yt: Item?
	
		tag = LSC.bit
		factor(&x, &t)
		while sym >= LSS.Symbols.times && sym <= LSS.Symbols.mod {
			if sym == LSS.Symbols.times {
				if t === intType { tag = LSC.mul } else { tag = LSC.and }
			} else {
				if t !== intType { LSS.Mark(44) }
				if sym == LSS.Symbols.div { tag = LSC.div
				} else if sym == LSS.Symbols.mod { tag = LSC.mod
				}
			}
			LSS.Get(&sym); factor(&y, &yt); checkT(yt!.tag, t!.tag)
			x = New(tag, x, y)
		}
	} //term
	
    static func expression(_ x: inout Item?, _ t: inout Item?) {
		var tag: SHORTINT
        var y, yt: Item?
	
		if sym == LSS.Symbols.minus {
			LSS.Get(&sym); term(&y, &t); x = New(LSC.neg, nil, y)
			if t !== intType { LSS.Mark(44) }
		} else if sym == LSS.Symbols.plus {
			LSS.Get(&sym); term(&x, &t)
			if t !== intType { LSS.Mark(44) }
		} else { term(&x, &t)
		}
		while sym == LSS.Symbols.plus || sym == LSS.Symbols.minus {
			if sym == LSS.Symbols.plus {
				if t === intType { tag = LSC.add } else { tag = LSC.or }
			} else { /*sym == minus*/
				if t === intType { tag = LSC.sub } else { tag = LSC.xor }
			}
			LSS.Get(&sym); term(&y, &yt); checkT(yt!.tag, t!.tag)
			x = New(tag, x, y)
		}
	} //expression;
	
    static func condition(_ x: inout Item) {
		var tag: SHORTINT
        var y, z, t: Item?
        expression(&y, &t)
		if t !== intType { LSS.Mark(44) } //;
		if sym >= LSS.Symbols.eql && sym <= LSS.Symbols.geq {
			if sym == LSS.Symbols.eql { tag = LSC.eql
			} else if sym == LSS.Symbols.neq { tag = LSC.neq
			} else if sym == LSS.Symbols.lss { tag = LSC.lss
			} else if sym == LSS.Symbols.geq { tag = LSC.geq
			} else if sym == LSS.Symbols.leq { tag = LSC.leq
			} else { tag = LSC.gtr
			}
			LSS.Get(&sym); expression(&z, &t)
			if t !== intType { LSS.Mark(44) }
			x = New(tag, y, z)
		} else { LSS.Mark(18)
		}
	} //condition;
	
    static func parameter(_ fp: Object, _ par: inout Item?) {
		var ftyp, eltyp, par0, par1: Item
        var x, y, atyp: Item?
        ftyp = fp.a!
		if fp.tag != LSC.in0 && fp.tag != LSC.io { LSS.Mark(36) }
		if sym == LSS.Symbols.lbrak {
			if ftyp.tag == LSC.array { eltyp = ftyp.a! } else { eltyp = bitType }
			LSS.Get(&sym); expression(&y, &atyp); x = New(LSC.next, y, nil); par0 = x!
			while sym == LSS.Symbols.comma {
				LSS.Get(&sym); expression(&y, &atyp); par1 = New(LSC.next, y, nil); par0.b = par1; par0 = par1
				if !(atyp === eltyp || eltyp.tag == LSC.bit && atyp!.tag <= LSC.oc) { LSS.Mark(40) }
			}
			if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
			if fp.tag == LSC.in0 {
				if ftyp.tag != LSC.array { LSS.Mark(40) }
			} else { LSS.Mark(36)
			}
		} else { expression(&x, &atyp)
			if atyp != nil {
				while atyp!.tag == LSC.array && ftyp.tag == LSC.array { atyp = atyp!.a; ftyp = ftyp.a! }
				if !(atyp === ftyp || ftyp.tag == LSC.bit && atyp!.tag <= LSC.oc) { LSS.Mark(40) }
			} else { LSS.Mark(36)
			}
		} //;
		par = New(LSC.next, x, nil)
	} //parameter;

    static func position(_ vtyp: Item, _ x: inout Item?) {
        var z, y, t: Item?
        
		if sym == LSS.Symbols.lbrak {
			LSS.Get(&sym);
			if vtyp.tag == LSC.array {
				if sym != LSS.Symbols.rbrak {
					position(vtyp.a!, &z); y = New(LSC.next, z, nil); x = y
					while sym == LSS.Symbols.semicolon {
						LSS.Get(&sym); position(vtyp.a!, &z); y!.b = New(LSC.next, z, nil); y = y!.b
					}
					if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
				} else { x = nil
				}
			} else { LSS.Mark(32); x = nil
				while sym != LSS.Symbols.rbrak { LSS.Get(&sym) }
			}
		} else { expression(&z, &t)
			if t!.tag != LSC.integer { LSS.Mark(42) }
			y = New(LSC.posass, z, nil); x = y
			while sym == LSS.Symbols.comma {
				LSS.Get(&sym); expression(&z, &t)
				if t!.tag != LSC.integer { LSS.Mark(42) }
				y!.b = New(LSC.posass, z, nil); y = y!.b
			}
		}
	} //position

    static func StatSequence(_ seq: inout Item?) {
		var tag: SHORTINT; var fp, obj: Object?
        var x, y, z, s, ss, xtyp, ytyp, ztyp, ap: Item?

        func IfPart(_ s: inout Item?) {
            var x = Item(t: LSC.bit, v: 0); var y, z: Item?
            condition(&x); y = nil; z = nil
			if sym == LSS.Symbols.then { LSS.Get(&sym) } else { LSS.Mark(22) }
			StatSequence(&y)
			if sym == LSS.Symbols.elsif { LSS.Get(&sym); IfPart(&z); z = New(LSC.next, z, nil)
			} else if sym == LSS.Symbols.else { LSS.Get(&sym); StatSequence(&z)
			} else { z = nil
			}
			s = New(LSC.if0, x, New(LSC.if1, y, z))
		} //IfPart

        ss = nil; tag = LSC.bit
        repeat {
            s = nil; /* obj = guard0; */
			if sym < LSS.Symbols.ident { LSS.Mark(24)
				repeat { LSS.Get(&sym) } while !(sym >= LSS.Symbols.ident)
			}
			if sym == LSS.Symbols.ident {
				x = This(localScope); LSS.Get(&sym); xtyp = x!.a!; y = x; ytyp = xtyp
				selector(&x, &xtyp)
				if sym == LSS.Symbols.becomes {
					if ytyp!.tag == LSC.record { LSS.Mark(46) }
					z = x
					while z!.tag == LSC.asel { z = z!.a }
					if z!.tag == LSC.in0 { LSS.Mark(47) }
					check(xtyp); LSS.Get(&sym); expression(&y, &ytyp); check(ytyp)
					if sym == LSS.Symbols.bar {
						LSS.Get(&sym); expression(&z, &ztyp); check(ztyp); y = New(LSC.tsg, y, z); tag = LSC.tsass
						if xtyp !== tsType { LSS.Mark(49) }
					} else if xtyp === bitType { tag = LSC.assign
					} else if xtyp === tsType { LSS.Mark(48); tag = LSC.assign
					} else if xtyp === ocType { tag = LSC.ocass
					}
					s = New(tag, x, y)
				} else if sym == LSS.Symbols.pos {
					if (x !== y) && (ytyp!.tag == LSC.record) { LSS.Mark(46) }
					LSS.Get(&sym); position(xtyp!, &y); s = New(LSC.posass, x, y)
				} else if sym == LSS.Symbols.lparen {
					LSS.Get(&sym)
					if xtyp!.tag == LSC.record {
						fp = (xtyp!.a!.a as! Object); s = New(LSC.call, x, nil)
						while fp!.tag < LSC.var0 { fp = fp!.next }
						if sym != LSS.Symbols.rparen {
							parameter(fp!, &ap); s!.b = ap; fp = fp!.next
							while sym == LSS.Symbols.comma {
								LSS.Get(&sym); parameter(fp!, &y); ap!.b = y; ap = y; fp = fp!.next
							}
							if fp!.tag == LSC.in0 || fp!.tag == LSC.io { LSS.Mark(35) }
							ap!.b = xtyp!.a
						} else { s!.b = xtyp!.a
						}
						if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					} else { LSS.Mark(37); expression(&x, &xtyp)
						while sym == LSS.Symbols.comma { LSS.Get(&sym); expression(&x, &xtyp) }
					}
				} else { LSS.Mark(21)
				}
			} else if sym == LSS.Symbols.if {
				LSS.Get(&sym); IfPart(&s)
				if sym == LSS.Symbols.end { LSS.Get(&sym) } else { LSS.Mark(26) }
			} else if sym == LSS.Symbols.for {
				LSS.Get(&sym);
				if sym == LSS.Symbols.ident {
					obj = NewObj(localScope, classv: LSC.par); obj!.a = intType; LSS.Get(&sym)
					if sym == LSS.Symbols.becomes { LSS.Get(&sym) } else { LSS.Mark(21) }
					expression(&x, &xtyp)
					if xtyp !== intType { LSS.Mark(39) }
					if sym == LSS.Symbols.to { LSS.Get(&sym) } else { LSS.Mark(23) }
					expression(&y, &ytyp)
					if ytyp !== intType { LSS.Mark(39) }
					if sym == LSS.Symbols.do { LSS.Get(&sym) } else { LSS.Mark(27) }
					z = nil; StatSequence(&z)
					s = New(LSC.for0, obj, New(LSC.for1, x, New(LSC.for2, y, z)));
					Remove(obj!)
					if sym == LSS.Symbols.end { LSS.Get(&sym) } else { LSS.Mark(26) }
				}
			}
			if s != nil { ss = New(LSC.next, s, ss) }
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym)
			} else if sym <= LSS.Symbols.ident || sym == LSS.Symbols.if || sym == LSS.Symbols.for { LSS.Mark(24)
			} else { break
			}
		} while true
		x = nil /*invert list*/
		while ss != nil { y = ss!.b; ss!.b = x; x = ss; ss = y }
		seq = x
	} //StatSequence

	/*---------------------------------------------------*/
	
    static func IdentList(_ classv: SHORTINT, _ first: inout Object?) {
        first = NewObj(localScope, classv: classv); LSS.Get(&sym)
		while sym == LSS.Symbols.comma {
			LSS.Get(&sym);
			if sym == LSS.Symbols.ident { _ = NewObj(localScope, classv: classv); LSS.Get(&sym)
			} else { LSS.Mark(10)
			}
		}
	} //IdentList

    static func TypParam(_ fp: Object?, _ typ: inout Item?) {
		var x, t: Item?
		if fp!.tag != LSC.par { LSS.Mark(36) }
		expression(&x, &t)
		if t !== intType { LSS.Mark(39) }
		typ = New(LSC.next, x, nil)
	} //TypParam

    static func Type(_ x: inout Item?, _ forms: Set<SHORTINT>) {
		var t, y, z: Item?; var obj, fp: Object?
	
		if sym == LSS.Symbols.lbrak {
			LSS.Get(&sym); expression(&z, &t)
			if t !== intType { LSS.Mark(39) }
			if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
			Type(&y, forms); x = New(LSC.array, y, z)
		} else {
			if sym == LSS.Symbols.ident {
				obj = This(globalScope); LSS.Get(&sym)
				if obj!.tag == LSC.type {
					fp = (obj!.a as! Object); x = New(LSC.record, obj, nil)
					if sym == LSS.Symbols.lparen {
						LSS.Get(&sym); TypParam(fp, &z); x!.b = z; fp = fp!.next
						while sym == LSS.Symbols.comma {
							LSS.Get(&sym); TypParam(fp, &y); z!.b = y; z = y; fp = fp!.next
						}
						if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					}
				} else { fp = guard0; LSS.Mark(37)
					if sym == LSS.Symbols.lparen {
						LSS.Get(&sym); expression(&y, &t)
						while sym == LSS.Symbols.comma { LSS.Get(&sym); expression(&y, &t) }
						if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
					}
				}
				if fp!.tag == LSC.par { LSS.Mark(38) }
			} else if sym == LSS.Symbols.bit { LSS.Get(&sym); x = bitType
			} else if sym == LSS.Symbols.ts { LSS.Get(&sym); x = tsType
			} else if sym == LSS.Symbols.oc { LSS.Get(&sym); x = ocType
			} else { LSS.Mark(28); x = bitType
			}
			if !forms.contains(x!.tag) { LSS.Mark(28) }
		}
	} //Type

    static func FormalType(_ x: inout Item?, _ forms: Set<SHORTINT>) {
		var y, z, t: Item?
	
		if sym == LSS.Symbols.lbrak {
			LSS.Get(&sym); expression(&z, &t)
			if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.Mark(16) }
			FormalType(&y, forms); x = New(LSC.array, y, z)
		} else {
			if sym == LSS.Symbols.bit { x = bitType; LSS.Get(&sym)
			} else if sym == LSS.Symbols.ts { x = tsType; LSS.Get(&sym)
			} else if sym == LSS.Symbols.oc { x = ocType; LSS.Get(&sym)
			} else { LSS.Mark(28); x = bitType
			}
			if !forms.contains(x!.tag) { LSS.Mark(28) }
		}
	} //FormalType;

    static func ConstDecl () {
		var obj: Object;
	
		while sym == LSS.Symbols.ident {
			obj = NewObj(localScope, classv: LSC.const); LSS.Get(&sym)
			if sym == LSS.Symbols.becomes || sym == LSS.Symbols.eql { LSS.Get(&sym)
			} else { LSS.Mark(21)
			}
			expression(&obj.b, &obj.a)
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //ConstDecl;

    static func PosDecl () {
		var p: Object; var x, r, t: Item?
	
		while sym == LSS.Symbols.ident {
			/* TBD - need to find out how to pass position to front end */
			p = This(localScope); t = p.a; LSS.Get(&sym); selector(&r, &t)
			if sym == LSS.Symbols.eql { LSS.Get(&sym)
			} else { LSS.Mark(29)
			}
			expression(&x, &t)
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //PosDecl;

    static func ParDecl(_ classv: SHORTINT, _ forms: Set<SHORTINT>) {
        var list: Object?; var obj: Object?; var type: Item?
	
		while sym == LSS.Symbols.ident {
			IdentList(classv, &list);
			if sym == LSS.Symbols.colon { LSS.Get(&sym) } else { LSS.Mark(20) }
			FormalType(&type, forms)
			obj = list
			while obj !== guard0 { obj!.a = type; obj = obj!.next }
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //ParDecl;

    static func VarDecl(_ classv: SHORTINT, _ forms: Set<SHORTINT>) {
        var list: Object?; var obj: Object?; var type: Item?
	
		while sym == LSS.Symbols.ident {
			IdentList(classv, &list)
			if sym == LSS.Symbols.colon { LSS.Get(&sym) } else { LSS.Mark(20) }
			Type(&type, forms)
			obj = list
			while obj !== guard0 { obj!.a = type; obj = obj!.next }
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
		}
	} //VarDecl

    static func TypeDecl() {
		var obj, this: Object?
        LSS.Get(&sym)
		obj = nil;
		if sym == LSS.Symbols.ident {
			this = NewObj(globalScope, classv: LSC.type); LSS.Get(&sym)
			if sym == LSS.Symbols.times { LSS.Get(&sym) }
			localScope.next = guard0 /*start new scope*/
			if sym == LSS.Symbols.lparen {
				LSS.Get(&sym);
				if sym == LSS.Symbols.ident { IdentList(LSC.par, &obj) } //;
				if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.Mark(15) }
				while obj !== guard0 { obj!.a = intType; obj = obj!.next }
			}
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			if sym == LSS.Symbols.const { LSS.Get(&sym); ConstDecl() }
			if sym == LSS.Symbols.in { LSS.Get(&sym); ParDecl(LSC.in0, [LSC.bit]) }
			if sym == LSS.Symbols.inout { LSS.Get(&sym); ParDecl(LSC.io, [LSC.ts, LSC.oc]) }
			if sym == LSS.Symbols.out { LSS.Get(&sym); VarDecl(LSC.out, [LSC.bit]) }
			if sym == LSS.Symbols.var { LSS.Get(&sym); VarDecl(LSC.var0, [LSC.bit, LSC.ts, LSC.oc, LSC.record]) }
			this!.a = localScope.next
			if sym == LSS.Symbols.begin { LSS.Get(&sym); StatSequence(&this!.b) }
			if sym == LSS.Symbols.end { LSS.Get(&sym) } else { LSS.Mark(26) }
			if sym == LSS.Symbols.ident {
				if LSS.id != this!.name { LSS.Mark(4) }
				LSS.Get(&sym)
			} else { LSS.Mark(10)
			}
		}
	} //TypeDecl

    static func Module (_ name: String) {
		var t, clk, rst: Item?
        print("compiling Lola ", terminator: "")
		globalScope.next = guard0; body = nil; clk = nil;
		LSS.Init(name); LSS.Get(&sym);
		if sym == LSS.Symbols.module {
			LSS.Get(&sym);
			if sym == LSS.Symbols.ident {
				globalScope.name = LSS.id; LSS.Get(&sym);
				print(LSS.id)
			} else { LSS.Mark(10)
			} //;
			if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) } //;
			while sym == LSS.Symbols.type {
				TypeDecl()
				if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} //;
			localScope.next = guard0;
			if sym == LSS.Symbols.const { LSS.Get(&sym); ConstDecl() } //;
			if sym == LSS.Symbols.in { LSS.Get(&sym); VarDecl(LSC.in0, [LSC.bit]) } //;
			if sym == LSS.Symbols.inout { LSS.Get(&sym); VarDecl(LSC.io, [LSC.ts, LSC.oc]) } //;
			if sym == LSS.Symbols.out { LSS.Get(&sym); VarDecl(LSC.out, [LSC.bit]) } //;
			if sym == LSS.Symbols.var { LSS.Get(&sym); VarDecl(LSC.var0, [LSC.bit, LSC.ts, LSC.oc, LSC.record]) } //;
			if sym == LSS.Symbols.pos { LSS.Get(&sym); PosDecl() };
			if sym == LSS.Symbols.clock {
				LSS.Get(&sym); expression(&clk, &rst); check(t)
				if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} //;
			if sym == LSS.Symbols.reset {
				LSS.Get(&sym); expression(&rst, &t); check(t)
				if sym == LSS.Symbols.semicolon { LSS.Get(&sym) } else { LSS.Mark(24) }
			} else { rst = nil
			} //;
			if sym == LSS.Symbols.begin { LSS.Get(&sym); StatSequence(&body) } //;
			if sym == LSS.Symbols.end { LSS.Get(&sym) } else { LSS.Mark(26) } //;
			if sym == LSS.Symbols.ident {
				if LSS.id != globalScope.name { LSS.Mark(4) } //;
				LSS.Get(&sym)
			} else { LSS.Mark(10)
			} //;
			if sym != LSS.Symbols.period { LSS.Mark(25) } //;
			if rst != nil { body = New(LSC.next, New(LSC.rstass, rst, nil), body) } //;
			if clk != nil { body = New(LSC.next, New(LSC.clkass, clk, nil), body) }
		} else { LSS.Mark(11)
		} //;
		if LSS.error { globalScope.next = guard0; localScope.next = guard0; body = nil }
	} //Module;

} //LSC.
