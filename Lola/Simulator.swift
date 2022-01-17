import Foundation

class Simulator {

    /* NW 27.12.92 / 25.5.96 / 4.12.97 */

    static let clash : Int = 2
    static let undef : Int = 3                    /* signal values */
	static let BaseTyps = [LSB.bit, LSB.ts]
    static let Struct = [LSB.array, LSB//LSB.record]
    static let Tab : Character = "\t"

	static var rorg: LSB.Signal?  /*register list*/
	static var sym: String = "01+x"
    static let Undef = [Int](repeating: undef, count: 4)
    static let Undef2 = [[Int]](repeating: Undef, count: 4)
	static var and = Undef2
    static var or  = Undef2
    static var xor = Undef2
	static var not = Undef

    static func value(_ s: LSB.Signal?) -> Int {
        var w: Int = 0; var h: Int
	
		if let ns = s {
			if s is LSB.Variable {
                assign(s as! LSB.Variable); w = s!.val
                if let v = s as? LSB.Variable {
                    print("\(v.name) = \(w)")
                }
			} else {
				switch ns.fct {
                case LSB.or:  w = or[Int(value(ns.x))][Int(value(ns.y))]; print(" Or \(value(ns.x)), \(value(ns.y)) = \(w)")
				case LSB.xor: w = xor[Int(value(ns.x))][Int(value(ns.y))]; print(" Xor \(value(ns.x)), \(value(ns.y)) = \(w)")
				case LSB.and: w = and[Int(value(ns.x))][Int(value(ns.y))]; print(" And \(value(ns.x)), \(value(ns.y)) = \(w)")
				case LSB.not: w = not[Int(value(ns.y))]; print(" Not \(value(ns.y)) = \(w)")
				case LSB.mux: h = Int(value(ns.x))
						if h == undef { w = undef
						} else if h == 0 { w = value(ns.y!.x)
						} else { w = value(ns.y!.y)
						}
                    print(" Mux \(value(ns.y!.x)), \(value(ns.y!.y)) = \(w)")
				case LSB.reg: w = ns.val
                    print(" Reg = \(w)")
				case LSB.latch: h = value(ns.x)
						if h == undef {
                            w = undef
						} else if h == 0 {
                            w = ns.val
						} else {
                            w = value(ns.y); ns.val = w
						}
                    print(" Latch = \(w)")
				case LSB.sr: h = value(ns.x); w = value(ns.y)
						if h == undef || w == undef {
                            w = undef
						} else if h == 0 {
							if w == 0 { w = clash } else { w = 1; ns.val = 1 } //
						} else if w == 0 {
                            w = 0; ns.val = 0
						} else {
                            w = ns.val
						} //
                    print(" Sr = \(w)")
                default: break
				}
			}
		} else {
            w = undef
		}
		return w
	} // value

	static func assign(_ v: LSB.Variable) {
		var lnk, tsg: LSB.Signal?
        var w, h: Int
	
        w = 0;
		if v.val == LSB.black {
			v.val = LSB.grey
			if v.fct == LSB.bit {
                w = value(v.x)
			} else if v.fct == LSB.ts {
				lnk = v.x; h = 0; w = undef
				/* at this point v.x should point to a SignalDesc with fct = link, but
				   there may be intervening ts variables due to renaming */
				while lnk != nil && lnk!.fct != LSB.link { lnk = lnk!.x }
				while true {
					if lnk == nil { break }
					tsg = lnk!.x; h = value(tsg!.x)
					if h == 1 {
						w = value(tsg!.y)
						repeat { lnk = lnk!.y } while !(lnk == nil || value(lnk!.x!.x) != 0)
						if lnk != nil { w = clash; break }
					} else if h == 0 {
                        lnk = lnk!.y
					} else {
                        break
					}
				}
			} else if v.fct == LSB.oc {
				lnk = v.x; w = 1
				while lnk != nil && w == 1 { w = value(lnk!.x); lnk = lnk!.y } //
			}
			v.val = w
		} else if v.val == LSB.grey {
			LSB.WriteName(v); print(" in loop")
		}
	} // assign

	static func evaluate(_ v: LSB.Variable?) {
        /*compute new values of variables*/
		if BaseTyps.contains(v!.fct) {
            assign(v!)
		} else if Struct.contains(v!.fct)  {
            var nv = v!.dsc
			while nv != nil { evaluate(nv); nv = nv!.next } //
		} //
	} // evaluate;

	static func initval(_ v: LSB.Variable?) {
		if BaseTyps.contains(v!.fct) {
			if v!.x != nil { v!.val = LSB.black } //
		} else if Struct.contains(v!.fct) {
            var nv = v!.dsc;
			while nv != nil { initval(nv); nv = nv!.next } //
		} //
	} // initval;
    
    static func OutChar(_ c: Character) { print(c, terminator:"") }
    static func OutString(_ s: String)  { print(s, terminator:"") }

	static func listinverse(_ v: LSB.Variable?) {
        if v != nil { listinverse(v!.next); OutChar(sym[Int(v!.val)]) } //
	} // listinverse;

	static func list(_ v: LSB.Variable?) {
		if BaseTyps.contains(v!.fct) {
			if v!.u == 0 { OutChar(sym[Int(v!.val)]); OutChar(Tab) } //
		} else if v!.fct == LSB.record { var nv = v!.dsc
			while nv != nil { list(nv); nv = nv!.next }
		} else if v!.fct == LSB.array {
			if v!.u == 0 { listinverse(v!.dsc); OutChar(Tab) }
			var nv = v!.dsc
			while nv != nil { list(nv); nv = nv!.next } //
		} //
	} // list;

    static func Step (_ step : Int) {
		var i: Int; var r, rg: LSB.Signal?
	
		if LSB.org != nil {
            i = step
			while i > 0 {
				rg = rorg  /* compute new values of register inputs */
				while rg != nil {
					r = rg!.x
					if value(r!.y!.x) == 1 { /* enabled */ r!.y!.val = Int(value(r!.y!.y)) } else { r!.y!.val = r!.val }
					rg = rg!.y
				}
				rg = rorg  /* tick: replace old values of registers by new values */
				while rg != nil { r = rg!.x; r!.val = r!.y!.val; rg = rg!.y }
				initval(LSB.org); evaluate(LSB.org); i -= 1
			}
            list(LSB.org); print("")
		}
	} // Step;

	static func Reset() {
		var rg: LSB.Signal?
	
		if LSB.org != nil {
			rg = rorg
			while rg != nil { rg!.x!.val = 0; rg = rg!.y }
			initval(LSB.org); evaluate(LSB.org);
			print("reset")
            print(""); OutString("loaded "); Label()
            list(LSB.org); print("\n")
		} //
	} // Reset;

    static func start1(_ s: LSB.Signal?) {
		var rg: LSB.Signal;
	
		if s != nil && !(s is LSB.Variable) {
			if s!.fct == LSB.reg { rg = LSB.Signal(); rg.x = s; rg.y = rorg; rorg = rg
			} else { start1(s!.x); start1(s!.y)
			} //
		} //
	} // start1;

    static func start0(_ v: LSB.Variable?) {
        var v = v
		if BaseTyps.contains(v!.fct) { start1(v!.x)
			if v!.x != nil { v!.val = LSB.black } //
		} else if Struct.contains(v!.fct) { v = v!.dsc
			while v != nil { start0(v); v = v!.next }
		}
	} // start0

	static func Start() {
		if LSB.org != nil {
			rorg = nil; start0(LSB.org); Reset()
		} else {
            print("No circuit to simulate")
		}
	} // Start

    static func Set (_ nameValue: String) {
        var v: LSB.Variable?
		var value: Int
		var name: LSB.Name
	
		/* extract the name and value from the nameValue input string */
		let values = nameValue.components(separatedBy: " ")
		if values.count == 1 {
            value = 0; name = nameValue
		} else {
            name = values[0]
			value = Int(values[1])!
		}

		v = LSB.This(LSB.org!, name)
		if v != nil {
            OutString("  "); LSB.WriteName(v!); OutString("=\(value)")
			if v!.fct == LSB.array {
				v = v!.dsc
				if v!.x == nil || v!.fct == LSB.ts {
					while v != nil { v!.val = Int(value % 2); value = value / 2; v = v!.next }
                } else { OutString(" not an input")
				}
			} else if v!.x == nil || v!.fct == LSB.ts { v!.val = Int(value % 2)
			}
		} else { print("No input called '\(name)'")
		}
		print("")
	} // Set

	static func lab(_ v: LSB.Variable?) {
        var v = v
		if v!.u == 0 { LSB.WriteName(v!); OutChar(Tab) }
		if Struct.contains(v!.fct) {
			v = v!.dsc
			while v != nil { lab(v); v = v!.next } //
		}
	} // lab

	static func Label() {
		if LSB.org != nil {
			lab(LSB.org); print("")
		}
	} // Label

	static func clrsel(_ v: LSB.Variable?) {
        var v = v
        v!.u = 1;
		if Struct.contains(v!.fct) {
            v = v!.dsc
			while v != nil { clrsel(v); v = v!.next } //
		}
	} // clrsel

	static func ClearSelect() {
		if LSB.org != nil { clrsel(LSB.org) } //
	} // ClearSelect;

    static func Select (_ name: String) {
		var i: Int; var v: LSB.Variable?
	
		if LSB.org != nil {
			i = 0
            let names = name.components(separatedBy: CharacterSet.whitespaces)
            for name in names where i < 14 {
				v = LSB.This(LSB.org!, name)
				if v != nil { v!.u = 0; i += 1 }
			}
		}
	} // Select

    static func DefOps() {
		or [0][0] = 0; or [0][1] = 1; or [1][0] = 1; or [1][1] = 1
		or [1][2] = 1; or [1][3] = 1; or [2][1] = 1; or [3][1] = 1
		and[0][0] = 0; and[0][1] = 0; and[1][0] = 0; and[1][1] = 1
		and[0][2] = 0; and[0][3] = 0; and[2][0] = 0; and[3][0] = 0
		xor[0][0] = 0; xor[0][1] = 1; xor[1][0] = 1; xor[1][1] = 0
		not[0] = 1; not[1] = 0
	} // DefOps;
    
    static func InLine() -> String {
        var s = ""
        var c : Character
        while true {
            c = InChar()
            if c != "\n" { s.append(c) }
            else { break }
        }
        return s
    }
    
    static func InChar() -> Character {
        let keyboard = FileHandle.standardInput
        let data = keyboard.readData(ofLength: 1)
        var buffer = [CChar](repeating: 0, count: 1)
        (data as NSData).getBytes(&buffer, length: 1)
        return Character(Int(buffer[0]))
    }

	static func DisplayCommands() {	
		print("Simulator commands:")
		print("  s <n>               - Step circuit <n> times")
		print("  l <file>            - Load circuit from <file>.lola")
		print("  v <input> <n>       - Set the value of <input> to <n>")
		print("  d <var1> <var2> ... - Display variables <var1>, <var2>, etc.")
        print("  x                   - Exit simulator"); print("")
	} // DisplayCommands;

	static func RemoveLeadingSpaces(_ s: inout String) {
		/* remove leading spaces in s */
		while s.hasPrefix(" ") { s.remove(at: s.startIndex) } //
	} // RemoveLeadingSpaces

	static func GetString (_ s: inout String) {
		s = InLine(); RemoveLeadingSpaces(&s)
	} // GetString;

	static func GetInt (_ n: inout Int) {
        var numb: LSB.Name = ""
		GetString(&numb);
		if let m = Int(numb) { n = m }
        else { n = 1 }
	} // GetInt;

	static func Interactive() {
        var n: Int = 0
		var cmd: Character = "\0"
		var file: String = ""
		var name: LSB.Name = ""
	
        DefOps()
        print("Simulator  MG, NW 7.10.2015")
		DisplayCommands()
        Start()

		repeat {
            OutString("> "); cmd = InChar();
			switch cmd.lowercase {
				case "s" : GetInt(&n); Label(); Step(n)
				case "l" : GetString(&file); LST.Import(file); Start(); ClearSelect()
				case "d" : GetString(&name); Select(name); Label()
				case "v" : GetString(&file); Set(file)
				case "x" : break /* just exit */
                default: DisplayCommands()
			} //
		} while cmd.lowercase != "x"

/*
		ClearSelect;
		Select("s"); Select("c"); Select("co"); Select("x"); Select("y"); Select("ci");
		Start;
		Set("x", 3); Set("y", 7); Out.Ln;
		Label;
		Step(1)
		*/
	} // Interactive;

} // Simulator.
