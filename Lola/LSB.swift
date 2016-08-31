import Foundation

class LSB {
    
    /* Lola System Base   MG 03.10.15, NW 25.2.95 / 16.4.96 / 21.10.97 */

	let NameLen = 32
    static let black : SHORTINT = 5; static let grey : SHORTINT = 4  /* node values used in loop search */
    
    typealias SHORTINT = Int8
    typealias BOOLEAN = Bool
    typealias INTEGER = Int
		
    /* function codes */
    static let bit : SHORTINT = 0; static let ts : SHORTINT  = 1; static let oc : SHORTINT = 2;
    static let integer : SHORTINT = 3; static let array : SHORTINT = 4; static let record : SHORTINT = 5;
    static let sect : SHORTINT = 6; static let buf : SHORTINT = 7; static let not : SHORTINT = 8;
    static let and : SHORTINT = 9; static let or : SHORTINT = 10; static let xor : SHORTINT = 11; static let mux : SHORTINT = 12;
    static let mux1 : SHORTINT = 13; static let reg : SHORTINT = 14; static let reg1 : SHORTINT = 15;
    static let latch : SHORTINT = 16; static let sr : SHORTINT = 17; static let tsg : SHORTINT = 18; static let link : SHORTINT = 19

    /*class codes*/
    static let Var : SHORTINT = 0; static let In : SHORTINT = 1; static let Out: SHORTINT = 2; static let IO : SHORTINT = 3

	typealias Name = String

	/* fct field: bits 0-4: function code, bits 5-7: class code;
       val field: not used; u,v fields: position data */

    class Signal {
		var x, y: Signal?
        var fct = LSB.bit; var val: SHORTINT = 0; var u: SHORTINT = -1; var v: SHORTINT = 0
    }

    class Variable : Signal {
		var name: Name = ""
		var classv: SHORTINT = LSB.In
		var next, dsc: Variable?
        
        init (name: String) {
            self.name = name
        }
        
        override init () {}
    }

    static let arrayRecordSet = Set<SHORTINT>(arrayLiteral: LSB.array, LSB.record)
	static var org : Variable?
    static let zero = Variable(name: "'0")
    static let one = Variable(name: "'1")
    static let clk = Variable(name: "CK")
	static var reset: Signal?
	static var change: BOOLEAN = false
	static let opcode = "BTONARX!~*+-:,^:$%|,"

	static func Init() {
        org = nil; reset = nil; clk.x = nil
	}
    
    static func OutString(s: String)  { print(s, terminator:"") }

    static func WriteName (v: Variable) {
		var w: Signal?
        w = v.y
        if (w != nil) && (w !== org) { WriteName(w as! Variable); OutString(".") }
		OutString(v.name)
	}

    static func This (org: Variable, _ name: String) -> Variable? {
		var v: Variable?; var i, j: INTEGER
        var id: String
        v = org.dsc; i = 0; id = ""
		repeat { j = 0;
			while (name[i] > " ") && (name[i] != ".") { id.append(name[i]); j += 1; i += 1 }
			while (v != nil) && (v!.name != id) { v = v!.next }
			if name[i] == "." {
				if v != nil && arrayRecordSet.contains(v!.fct) {
                    v = v!.dsc; i += 1
				} else {
                    v = nil; break
				}
			} else { break
			}
		} while true
		return v
	}

    static func New (f: SHORTINT, _ x: Signal?, _ y: Signal?) -> Signal {
		let z = Signal()
        z.fct = f; z.x = x; z.y = y; return z
	}

    static func NewVar (f: SHORTINT, _ val: SHORTINT, _ x: Signal?, _ y: Signal?, _ next: Variable?, _ name: String) -> Variable {
		let v = Variable(name: name)
        v.fct = f; v.val = val; v.x = x; v.y = y; v.u = -1; v.v = -1
		v.next = next; return v
	}

	/*----------------- Simplify --------------------*/

    static func traverse(inout s: Signal?) {
		var z: Signal
		if s != nil {
			if s is Variable {
				if s!.x === zero { s = zero
				} else if s!.x === one { s = one
				}
			} else {
                traverse(&s!.x); traverse(&s!.y)
                switch s!.fct {
                case LSB.not:
                    if s!.y!.fct == LSB.not { s = s!.y!.y
                    } else if s!.y === zero { s = one
                    } else if s!.y === one { s = zero
                    }
                case LSB.or:
                    if s!.x === one { s = one
                    } else if s!.x === zero { s = s!.y
                    } else if s!.y === one { s = one
                    } else if s!.y === zero { s = s!.x
                    }
                case LSB.xor:
                    if s!.x === zero { s = s!.y
                    } else if s!.x === one {
                        if s!.y!.fct == LSB.not { s = s!.y!.y } else { s!.fct = LSB.not; s!.x = nil }
                    } else if s!.y === zero { s = s!.x
                    } else if s!.y === one {
                        if s!.x!.fct == LSB.not { s = s!.x!.y } else { s!.fct = LSB.not; s!.y = s!.x; s!.x = nil }
                    }
                case LSB.and:
                    if s!.x === zero { s = zero
                    } else if s!.x === one { s = s!.y
                    } else if s!.y === zero { s = zero
                    } else if s!.y === one { s = s!.x
                    }
                case LSB.mux:
                    if s!.x === zero { s = s!.y!.x
                    } else if s!.x === one { s = s!.y!.y
                    } else if s!.y!.x === zero { s!.fct = LSB.and; s!.y = s!.y!.y; traverse(&s)
                    } else if s!.y!.x === one { s!.fct = LSB.or; z = s!.y!; s!.y = z.y;
                        z.fct = LSB.not; z.x = nil; z.y = s!.x; s!.x = z; traverse(&s)
                    } else if s!.y!.y === zero { s!.fct = LSB.and; z = s!.y!; s!.y = z.x;
                        z.fct = LSB.not; z.x = nil; z.y = s!.x; s!.x = z; traverse(&s!.x)
                    } else if s!.y!.y === one { s!.fct = LSB.or; s!.y = s!.y!.x
                    }
                case LSB.reg:
                    if s!.x === zero || s!.x === one || s!.y!.x === zero {
                        print(" dead reg")
                    }
                case LSB.latch:
                    if s!.x === zero { print(" dead latch")
                    } else if s!.x === one { s = s!.y
                    }
                case LSB.sr:
                    if s!.x === zero || s!.y === zero { print(" dead SR")
                    }
                case LSB.tsg:
                    if s!.x === zero || s!.x === one {
                        print(" dead tri-state")
                    }
                default: break
                }
			}
		}
	}

	static func simp(v: Variable?) {
        var v = v
		if arrayRecordSet.contains(v!.fct) {
            v = v!.dsc
			while v != nil { simp(v); v = v!.next }
		} else if v!.x !== zero && v!.x !== one {
			if v!.fct == LSB.link { traverse(&v!.x); traverse(&v!.y)
			} else { traverse(&v!.x)
				if v!.x === zero || v!.x === one { change = true }
			}
		}
	}

    static func Simplify (org: Variable) {
		var n: INTEGER;
        n = 0;
        repeat { n += 1; change = false; simp(org) } while change
	}

	/*----------------- Find Loops --------------------*/

	static func Loop(s: Signal?) {
		if s != nil {
			if s is Variable {
				if s!.val == black { s!.val = grey; Loop(s!.x); s!.val = 0
				} else if s!.val == grey {
					WriteName(s as! Variable); print(" in loop")
				}
			} else if s!.fct != LSB.reg {
				Loop(s!.x)
				if s!.fct != LSB.tsg { Loop(s!.y) }
			}
		}
	}

	static func Loops (v: Variable?) {
        var v = v
		if arrayRecordSet.contains(v!.fct) {
            v = v!.dsc
			while v != nil { Loops(v); v = v!.next }
		} else if v!.val == black { Loop(v)
		}
	}

	/*----------------- Show --------------------*/

    static func ShowTree(x: Signal?) {
        var f: SHORTINT
		if x != nil {
			if x is Variable { WriteName(x as! Variable)
			} else {
                f = x!.fct; OutString("(")
				ShowTree(x!.x); OutString("\(opcode[Int(f)])")
                ShowTree(x!.y); OutString(")")
			}
		}
	}

    static func Show (x: Variable?) {
        var x = x
		let typ = x!.fct
		if arrayRecordSet.contains(typ) {
			x = x!.dsc
			while x != nil { Show(x); x = x!.next }
		} else if typ != LSB.integer {
			WriteName(x!)
			if x!.u != -1 {
				OutString(" (\(Int(x!.u) % 0x100))")
			}
			if x!.x != nil { OutString(" = "); ShowTree(x!.x) }
			print("")
		}
	}

	/*----------------- Open --------------------*/

	static func Assign (v: Variable) { org = v }

}
