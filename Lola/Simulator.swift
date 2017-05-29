import Foundation

class Simulator {

    /* NW 27.12.92 / 25.5.96 / 4.12.97 */
    
    typealias SHORTINT = Int8
    typealias INTEGER  = Int
    typealias LONGINT = Int

    static let clash : SHORTINT = 2
    static let undef : SHORTINT = 3                    /* signal values */
	static let BaseTyps = [LSB.bit, LSB.ts, LSB.oc]
    static let Struct = [LSB.array, LSB.record]
    static let Tab : Character = "\t"

	static var rorg: LSB.Signal?  /*register list*/
	static var sym: String = "01+x"
    static let Undef = [SHORTINT](repeating: undef, count: 4)
    static let Undef2 = [[SHORTINT]](repeating: Undef, count: 4)
	static var and = Undef2
    static var or  = Undef2
    static var xor = Undef2
	static var not = Undef

    static func value(_ s: LSB.Signal?) -> SHORTINT {
        var w: SHORTINT = 0; var h: SHORTINT
	
		if let s = s {
			if s is LSB.Variable {
                assign((s as! LSB.Variable)); w = s.val
			} else {
				switch s.fct {
                case 0: break
				case LSB.or:  w = or[Int(value(s.x))][Int(value(s.y))]
				case LSB.xor: w = xor[Int(value(s.x))][Int(value(s.y))]
				case LSB.and: w = and[Int(value(s.x))][Int(value(s.y))]
				case LSB.not: w = not[Int(value(s.y))]
				case LSB.mux: h = SHORTINT(value(s.x))
						if h == undef { w = undef
						} else if h == 0 { w = value(s.y!.x)
						} else { w = value(s.y!.y)
						}
				case LSB.reg: w = s.val
				case LSB.latch: h = value(s.x)
						if h == undef {
                            w = undef
						} else if h == 0 {
                            w = s.val
						} else {
                            w = value(s.y); s.val = w
						}
				case LSB.sr: h = value(s.x); w = value(s.y)
						if h == undef || w == undef {
                            w = undef
						} else if h == 0 {
							if w == 0 { w = clash } else { w = 1; s.val = 1 } //
						} else if w == 0 {
                            w = 0; s.val = 0
						} else {
                            w = s.val
						} //
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
        var w, h: SHORTINT
	
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
        var v = v
		if BaseTyps.contains(v!.fct) {
            assign(v!)
		} else if Struct.contains(v!.fct)  {
            v = v!.dsc
			while v != nil { evaluate(v); v = v!.next } //
		} //
	} // evaluate;

	static func initval(_ v: LSB.Variable?) {
        var v = v
		if BaseTyps.contains(v!.fct) {
			if v!.x != nil { v!.val = LSB.black } //
		} else if Struct.contains(v!.fct) {
            v = v!.dsc;
			while v != nil { initval(v); v = v!.next } //
		} //
	} // initval;
    
    static func OutChar(_ c: Character) { print(c, terminator:"") }
    static func OutString(_ s: String)  { print(s, terminator:"") }

	static func listinverse(_ v: LSB.Variable?) {
        if v != nil { listinverse(v!.next); OutChar(sym[Int(v!.val)]) } //
	} // listinverse;

	static func list(_ v: LSB.Variable?) {
        var v = v
		if BaseTyps.contains(v!.fct) {
			if v!.u == 0 { OutChar(sym[Int(v!.val)]); OutChar(Tab) } //
		} else if v!.fct == LSB.record { v = v!.dsc
			while v != nil { list(v); v = v!.next } //
		} else if v!.fct == LSB.array {
			if v!.u == 0 { listinverse(v!.dsc); OutChar(Tab) } // ;
			v = v!.dsc
			while v != nil { list(v); v = v!.next } //
		} //
	} // list;

    static func Step (_ step : INTEGER) {
		var i: LONGINT; var r, rg: LSB.Signal?
	
		if LSB.org != nil {
            i = step
			while i > 0 {
				rg = rorg  /* compute new values of register inputs */
				while rg != nil {
					r = rg!.x
					if value(r!.y!.x) == 1 { /* enabled */ r!.y!.val = SHORTINT(value(r!.y!.y)) } else { r!.y!.val = r!.val }
					rg = rg!.y
				}
				rg = rorg  /* tick: replace old values of registers by new values */
				while rg != nil { r = rg!.x; r!.val = r!.y!.val; rg = rg!.y }
				initval(LSB.org); evaluate(LSB.org); i -= 1
				list(LSB.org); print("")
			} //
		} //
	} // Step;

	static func Reset() {
		var rg: LSB.Signal?
	
		if LSB.org != nil {
			rg = rorg
			while rg != nil { rg!.x!.val = 0; rg = rg!.y } // ;
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
		var value: INTEGER
		var name: LSB.Name
	
		/* extract the name and value from the nameValue input string */
		let values = nameValue.components(separatedBy: " ")
		if values.count == 1 {
            value = 0; name = nameValue
		} else {
            name = values[0]
			value = INTEGER(values[1])!
		}

		v = LSB.This(LSB.org!, name)
		if v != nil {
            OutString("  "); LSB.WriteName(v!); OutString("=\(value)")
			if v!.fct == LSB.array {
				v = v!.dsc
				if v!.x == nil || v!.fct == LSB.ts {
					while v != nil { v!.val = SHORTINT(value % 2); value = value / 2; v = v!.next }
                } else { OutString(" not an input")
				}
			} else if v!.x == nil || v!.fct == LSB.ts { v!.val = SHORTINT(value % 2)
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
		var i: LONGINT; var v: LSB.Variable?
	
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

	static func GetInt (_ n: inout INTEGER) {
        var numb: LSB.Name = ""
		GetString(&numb);
		if let m = INTEGER(numb) { n = m }
        else { n = 1 }
	} // GetInt;

	static func Interactive() {
        var n: INTEGER = 0
		var cmd: Character = "\0"
		var file: String = ""
		var name: LSB.Name = ""
	
        DefOps()
        print("Simulator  MG, NW 7.10.2015")
		DisplayCommands()

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
