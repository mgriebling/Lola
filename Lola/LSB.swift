import Foundation

class LSB {   /*Lola System Base   MG 11.8.14, NW 25.2.95 / 16.4.96 / 21.10.97*/
//	IMPORT Out, GC;

	let NameLen = 32;
    let black : SHORTINT = 5; let grey : SHORTINT = 4;  /* node values used in loop search */
    
    typealias SHORTINT = Int8
    typealias BOOLEAN = Bool
    typealias INTEGER = Int
		
    /*function codes*/
    enum Functions : Int8 {case bit = 0, ts = 1, oc = 2, integer = 3, array = 4, record = 5, sect = 6,
		buf = 7, not = 8, and = 9, or = 10, xor = 11, mux = 12, mux1 = 13,
        reg = 14, reg1 = 15, latch = 16, sr = 17, tsg = 18, link = 19}

    /*class codes*/
    let Var : SHORTINT = 0; let In : SHORTINT = 1; let Out: SHORTINT = 2; let IO : SHORTINT = 3;

	typealias Name = String

	/* fct field: bits 0-4: function code, bits 5-7: class code;
       val field: not used; u,v fields: position data */

    class Signal {
		var x, y: Signal?
        var fct: Functions = .bit; var val: SHORTINT = 0; var u: SHORTINT = 0; var v: SHORTINT = 0
    }

    class Variable : Signal {
		var name: Name = ""
		var classv: SHORTINT = 0
		var next, dsc: Variable?
    }

    let arrayRecordSet = Set<Functions>(arrayLiteral: .array, .record)
	var org, zero, one, clk: Variable?
	var reset: Signal?
	var change: BOOLEAN = false
	var opcode: String
    
    init () {
 //       OpenLog()
        zero = Variable(); zero!.name = "'0"; zero!.fct = .bit; zero!.val = 0; zero!.u = -1; zero!.classv = In
        one = Variable(); one!.name = "'1"; one!.fct = .bit; one!.val = 1; one!.u = -1; one!.classv = In
        clk = Variable(); clk!.name = "CK"; clk!.fct = .bit; clk!.u = -1; clk!.classv = In
        opcode = "BTONARX!~*+-:,^:$%|,"
    }

	func Init() {
        org = nil; reset = nil; clk!.x = nil
	}

    func WriteName (v: Variable) {
		var w: Signal?
        w = v.y
        if (w != nil) && (w !== org) { WriteName(w as! Variable); print(".", terminator: "") }
		print(v.name, terminator: "")
	}

    func This (org: Variable, name: String) -> Variable? {
		var v: Variable?; var i, j: INTEGER
        var id: String
        v = org.dsc; i = 0; id = ""
		repeat { j = 0;
			while (name[i] > " ") && (name[i] != ".") { id.append(name[i]); j++; i++ }
			while (v != nil) && (v!.name != id) { v = v!.next }
			if name[i] == "." {
				if v != nil && arrayRecordSet.contains(v!.fct) {
                    v = v!.dsc; i++
				} else {
                    v = nil; break
				}
			} else { break
			}
		} while true
		return v
	}

    func New (f: Functions, x: Signal, y: Signal) -> Signal {
		let z = Signal()
        z.fct = f; z.x = x; z.y = y; return z
	}

    func NewVar (f: Functions, val: SHORTINT, x: Signal, y: Signal, next: Variable, name: String) -> Variable {
		let v = Variable()
        v.fct = f; v.val = val; v.x = x; v.y = y; v.u = -1; v.v = -1;
		v.next = next; v.name = name; return v
	}

	/*----------------- Simplify --------------------*/

    func traverse(inout s: Signal?) {
		var z: Signal;
		if s != nil {
			if s is Variable {
				if s!.x === zero { s = zero
				} else if s!.x === one { s = one
				}
			} else {
                traverse(&s!.x); traverse(&s!.y);
				if s!.fct == .not {
					if s!.y!.fct == .not { s = s!.y!.y
					} else if s!.y === zero { s = one
					} else if s!.y === one { s = zero
					}
				} else if s!.fct == .or {
					if s!.x === one { s = one
					} else if s!.x === zero { s = s!.y
					} else if s!.y === one { s = one
					} else if s!.y === zero { s = s!.x
					}
				} else if s!.fct == .xor {
					if s!.x === zero { s = s!.y
					} else if s!.x === one {
						if s!.y!.fct == .not { s = s!.y!.y } else { s!.fct = .not; s!.x = nil }
					} else if s!.y === zero { s = s!.x
					} else if s!.y === one {
						if s!.x!.fct == .not { s = s!.x!.y } else { s!.fct = .not; s!.y = s!.x; s!.x = nil }
					}
				} else if s!.fct == .and {
					if s!.x === zero { s = zero
					} else if s!.x === one { s = s!.y
					} else if s!.y === zero { s = zero
					} else if s!.y === one { s = s!.x
					}
				} else if s!.fct == .mux {
					if s!.x === zero { s = s!.y!.x
					} else if s!.x === one { s = s!.y!.y
					} else if s!.y!.x === zero { s!.fct = .and; s!.y = s!.y!.y; traverse(&s)
					} else if s!.y!.x === one { s!.fct = .or; z = s!.y!; s!.y = z.y;
						z.fct = .not; z.x = nil; z.y = s!.x; s!.x = z; traverse(&s)
					} else if s!.y!.y === zero { s!.fct = .and; z = s!.y!; s!.y = z.x;
						z.fct = .not; z.x = nil; z.y = s!.x; s!.x = z; traverse(&s!.x)
					} else if s!.y!.y === one { s!.fct = .or; s!.y = s!.y!.x
					}
				} else if s!.fct == .reg {
					if (s!.x === zero) || (s!.x === one) || (s!.y!.x === zero) {
						print(" dead reg")
					}
				} else if s!.fct == .latch {
					if s!.x === zero { print(" dead latch")
					} else if s!.x === one { s = s!.y
					}
				} else if s!.fct == .sr {
					if (s!.x === zero) || (s!.y === zero) { print(" dead SR")
					}
				} else if s!.fct == .tsg {
					if (s!.x === zero) || (s!.x === one) {
						print(" dead tri-state")
					}
				}
			}
		}
	}

	func simp(var v: Variable?) {
		if arrayRecordSet.contains(v!.fct) {
            v = v!.dsc;
			while v != nil { simp(v); v = v!.next }
		} else if (v!.x !== zero) && (v!.x !== one) {
			if v!.fct == .link { traverse(&v!.x); traverse(&v!.y)
			} else { traverse(&v!.x);
				if (v!.x === zero) || (v!.x === one) { change = true }
			}
		}
	}

    func Simplify (org: Variable) {
		var n: INTEGER;
        n = 0;
        repeat { n++; change = false; simp(org) } while change
	}

	/*----------------- Find Loops --------------------*/

	func Loop(s: Signal?) {
		if s != nil {
			if s is Variable {
				if s!.val == black { s!.val = grey; Loop(s!.x); s!.val = 0
				} else if s!.val == grey {
					WriteName(s as! Variable); print(" in loop")
				}
			} else if s!.fct != .reg {
				Loop(s!.x);
				if s!.fct != .tsg { Loop(s!.y) }
			}
		}
	}

	func Loops (var v: Variable?) {
		if arrayRecordSet.contains(v!.fct) {
            v = v!.dsc;
			while v != nil { Loops(v); v = v!.next }
		} else if v!.val == black { Loop(v)
		}
	}

	/*----------------- Show --------------------*/

    func ShowTree(x: Signal?) {
        var f: Functions
		if x != nil {
			if x is Variable { WriteName(x as! Variable)
			} else {
                f = x!.fct; print("(", terminator: "")
				ShowTree(x!.x); print(opcode[Int(f.rawValue)], terminator: "")
                ShowTree(x!.y); print(")", terminator: "")
			}
		}
	}

    func Show (var x: Variable?) {
		let typ = x!.fct
		if arrayRecordSet.contains(typ) {
			x = x!.dsc;
			while x != nil { Show(x); x = x!.next }
		} else if typ != .integer {
			WriteName(x!)
			if x!.u != -1 {
				print(" (\(Int(x!.u) % 0x100))", terminator: "")
			}
			if x!.x != nil { print(" = ", terminator: ""); ShowTree(x!.x) }
			print("")
		}
	}

	/*----------------- Open --------------------*/

	func OpenLog()
		/* var V: MenuViewers.Viewer; X, Y: INTEGER; */
	{
		/* Oberon.AllocateSystemViewer(Oberon.Mouse.X, X, Y);
		V = MenuViewers.New (
				TextFrames.NewMenu("Lola.Log", 
					"System.Close  System.Copy  System.Grow  Edit.Search  Edit.Parcs  Edit.Store "),
				TextFrames.NewText(Log, 0), TextFrames.menuH, X, Y)
		*/
	}

	func ClearLog () {
        /* TextIO.Delete(Log, 0, Log.len) */
	}

	func Assign (v: Variable) { org = v }

}
