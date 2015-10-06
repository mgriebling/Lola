class Simulator {

    /* NW 27.12.92 / 25.5.96 / 4.12.97 */
//	IMPORT In, Out, Strings, Conv, LST, LSB;
    
    typealias SHORTINT = Int8
    typealias INTEGER  = Int
    typealias LONGINT = Int

    static let clash : SHORTINT = 2; static let undef : SHORTINT = 3;  /* signal values */
	static let BaseTyps = [LSB.bit, LSB.ts, LSB.oc];
    static let Struct = [LSB.array, LSB.record];
    static let Tab : Character = "\t"

	static var rorg: LSB.Signal?  /*register list*/
	static var sym: String = ""
	static var and = [[SHORTINT]]()
    static var or  = [[SHORTINT]]()
    static var xor = [[SHORTINT]]()
	static var not = [SHORTINT]()

//	PROCEDURE^ assign(v: LSB.Variable);

    static func value(s: LSB.Signal?) -> INTEGER {
		var w, h: SHORTINT;
	
		if let s = s {
			if s is LSB.Variable { assign((s as! LSB.Variable)); w = s.val
			} else {
				switch s.fct {
                case 0: break
				case LSB.or:  w = or[value(s.x)][value(s.y)]
				case LSB.xor: w = xor[value(s.x)][value(s.y)]
				case LSB.and: w = and[value(s.x)][value(s.y)]
				case LSB.not: w = not[value(s.y)]
				case LSB.mux: h = SHORTINT(value(s.x))
						if h == undef { w = undef
						} else if h == 0 { w = SHORTINT(value(s.y!.x))
						} else { w = SHORTINT(value(s.y!.y))
						}
				case LSB.reg: w = s.val
				case LSB.latch: h = SHORTINT(value(s.x))
						if h == undef { w = undef
						} else if h == 0 { w = s.val
						} else { w = SHORTINT(value(s.y)); s.val = w
						}
				case LSB.sr: h = SHORTINT(value(s.x)); w = SHORTINT(value(s.y));
						if (h == undef) || (w == undef) { w = undef
						} else if h == 0 {
							if w == 0 { w = clash } else { w = 1; s.val = 1 } //
						} else if w == 0 { w = 0; s.val = 0
						} else { w = s.val
						} //
				} //
			} //
		} else { w = undef
		} // ;
		return INTEGER(w)
	} // value;

	static func assign(v: LSB.Variable) {
		var lnk, tsg: LSB.Signal?
        var w, h: SHORTINT;
	
        w = 0;
		if v.val == LSB.black {
			v.val = LSB.grey;
			if v.fct == LSB.bit { w = SHORTINT(value(v.x))
			} else if v.fct == LSB.ts {
				lnk = v.x; h = 0; w = SHORTINT(undef)
				/* at this point v.x should point to a SignalDesc with fct = link, but
				there may be intervening ts variables due to renaming */
				while (lnk != nil) && (lnk!.fct != LSB.link) { lnk = lnk!.x } //;
				for ;; {
					if lnk == nil { break } // ;
					tsg = lnk!.x; h = SHORTINT(value(tsg!.x))
					if h == 1 {
						w = SHORTINT(value(tsg!.y))
						repeat { lnk = lnk!.y } while !((lnk == nil) || (value(lnk!.x!.x) != 0))
						if lnk != nil { w = clash; break } //
					} else if h == 0 { lnk = lnk!.y
					} else { break
					} //
				} //
			} else if v.fct == LSB.oc {
				lnk = v.x; w = 1;
				while (lnk != nil) && (w == 1) { w = SHORTINT(value(lnk!.x)); lnk = lnk!.y } //
			} // ;
			v.val = w
		} else if v.val == LSB.grey {
			LSB.WriteName(v); print(" in loop")
		} //
	} // assign;

	static func evaluate(var v: LSB.Variable?) {
        /*compute new values of variables*/
		if BaseTyps.contains(v!.fct) {
            assign(v!)
		} else if Struct.contains(v!.fct)  {
            v = v!.dsc;
			while v != nil { evaluate(v); v = v!.next } //
		} //
	} // evaluate;

	static func initval(var v: LSB.Variable?) {
		if BaseTyps.contains(v!.fct) {
			if v!.x != nil { v!.val = LSB.black } //
		} else if Struct.contains(v!.fct) {
            v = v!.dsc;
			while v != nil { initval(v); v = v!.next } //
		} //
	} // initval;
    
    static func OutChar(c: Character) { print(c, terminator:"") }

	static func listinverse(v: LSB.Variable?) {
        if v != nil { listinverse(v!.next); print(sym[Int(v!.val)], terminator:"") } //
	} // listinverse;

	static func list(var v: LSB.Variable?) {
		if BaseTyps.contains(v!.fct) {
			if v!.u == 0 { OutChar(sym[Int(v!.val)]); OutChar(Tab) } //
		} else if v!.fct == LSB.record { v = v!.dsc;
			while v != nil { list(v); v = v!.next } //
		} else if v!.fct == LSB.array {
			if v!.u == 0 { listinverse(v!.dsc); OutChar(Tab) } // ;
			v = v!.dsc;
			while v != nil { list(v); v = v!.next } //
		} //
	} // list;

    static func Step (step : INTEGER) {
		var i: LONGINT; var r, rg: LSB.Signal?
	
		if LSB.org != nil {
            i = step;
			while i > 0 {
				rg = rorg;  /* compute new values of register inputs */
				while rg != nil {
					r = rg!.x;
					if value(r!.y!.x) == 1 { /* enabled */ r!.y!.val = SHORTINT(value(r!.y!.y)) } else { r!.y!.val = r!.val } // ;
					rg = rg!.y
				} // ;
				rg = rorg;  /* tick: replace old values of registers by new values */
				while rg != nil { r = rg!.x; r!.val = r!.y!.val; rg = rg!.y } // ;
				initval(LSB.org); evaluate(LSB.org); i--
				list(LSB.org); print("")
			} //
		} //
	} // Step;

	static func Reset() {
		var rg: LSB.Signal?
	
		if LSB.org != nil {
			rg = rorg;
			while rg != nil { rg!.x!.val = 0; rg = rg!.y } // ;
			initval(LSB.org); evaluate(LSB.org);
			print("reset")
            print(""); print("loaded ", terminator:""); Label()
            list(LSB.org); print("\n")
		} //
	} // Reset;

    static func start1(s: LSB.Signal?) {
		var rg: LSB.Signal;
	
		if (s != nil) && !(s is LSB.Variable) {
			if s!.fct == LSB.reg { rg = LSB.Signal(); rg.x = s; rg.y = rorg; rorg = rg
			} else { start1(s!.x); start1(s!.y)
			} //
		} //
	} // start1;

    static func start0(var v: LSB.Variable?) {
		if BaseTyps.contains(v!.fct) { start1(v!.x);
			if v!.x != nil { v!.val = LSB.black } //
		} else if Struct.contains(v!.fct) { v = v!.dsc;
			while v != nil { start0(v); v = v!.next } //
		} //
	} // start0;

	static func Start() {
		if LSB.org != nil {
			rorg = nil; start0(LSB.org); Reset()
		} else {
            print("No circuit to simulate")
		} //
	} // Start;

    static func Set (nameValue: String) {
        var v: LSB.Variable?
		var value: INTEGER;
		var name: LSB.Name;
		var pos: INTEGER;
	
		/* extract the name and value from the nameValue input string */
		pos = Strings.Pos(" ", nameValue, 0);
		if pos <= 0 {
            value = 0; COPY(nameValue, name);
		} else {
            Strings.Extract(nameValue, 0, pos, name); Strings.Delete(nameValue, 0, pos+1);
			value = Conv.IntVal(nameValue)
		} //;

		v = LSB.This(LSB.org, name);
		if v != nil {
			Out.String("  "); LSB.WriteName(v); Out.Char("="); Out.Int(value, 1);
			if v.fct == LSB.array {
				v = v.dsc;
				if (v.x == nil) || (v.fct == LSB.ts) {
					while v != nil { v.val = SHORT(value MOD 2); value = value DIV 2; v = v.next } // ;
				} else { Out.String(" not an input")
				} //
			} else if (v.x == nil) || (v.fct == LSB.ts) { v.val = SHORT(value MOD 2)
			} //
		} else { print("No input called '\(name)'");
		} //;
		Out.Ln()
	} // Set;

	static func lab(v: LSB.Variable) {
		if v.u == 0 { LSB.WriteName(v); Out.Char(Tab) } //;
		if v.fct IN Struct {
			v = v.dsc;
			while v != nil { lab(v); v = v.next } //
		} //
	} // lab;

	static func Label() {
		if LSB.org != nil {
			lab(LSB.org); Out.Ln()
		} //
	} // Label;

	static func clrsel(v: LSB.Variable) {
	 v.u = 1;
		if v.fct IN Struct { v = v.dsc;
			while v != nil { clrsel(v); v = v.next } //
		} //
	} // clrsel;

	static func ClearSelect() {
	
		if LSB.org != nil { clrsel(LSB.org) } //
	} // ClearSelect;

    static func Select* (name: String) {
		var i: LONGINT; v: LSB.Variable;
			n: LSB.Name; pos: INTEGER;
	
		if LSB.org != nil {
			i = 0;
			while (Strings.Length(name) > 0) && (i < 14) {
				/* extract the name and value from the nameValue input string */
				pos = Strings.Pos(" ", name, 0);
				if pos <= 0 { COPY(name, n); name = ""
				} else { Strings.Extract(name, 0, pos, n); Strings.Delete(name, 0, pos+1)
				} //;
				v = LSB.This(LSB.org, n);
				if v != nil { v.u = 0; INC(i) } // ;
			} //
		} //
	} // Select;

    static func DefOps() {
		var i, j: INTEGER;
	
		FOR i = 0 TO undef {
			FOR j = 0 TO undef { or[i, j] = undef; and[i, j] = undef; xor[i, j] = undef } // ;
			not[i] = undef
		} // ;
		or [0, 0] = 0; or [0, 1] = 1; or [1, 0] = 1; or [1, 1] = 1;
		or [1, 2] = 1; or [1, 3] = 1; or [2, 1] = 1; or [3, 1] = 1;
		and[0, 0] = 0; and[0, 1] = 0; and[1, 0] = 0; and[1, 1] = 1;
		and[0, 2] = 0; and[0, 3] = 0; and[2, 0] = 0; and[3, 0] = 0;
		xor[0, 0] = 0; xor[0, 1] = 1; xor[1, 0] = 1; xor[1, 1] = 0;
		not[0] = 1; not[1] = 0
	} // DefOps;

	static func DisplayCommands() {	
		Out.String("Simulator commands:"); Out.Ln;
		Out.String("  s <n>               - Step circuit <n> times"); Out.Ln;
		Out.String("  l <file>            - Load circuit from <file>.lola"); Out.Ln;
		Out.String("  v <input> <n>       - Set the value of <input> to <n>"); Out.Ln;
		Out.String("  d <var1> <var2> ... - Display variables <var1>, <var2>, etc."); Out.Ln;
		Out.String("  x                   - Exit simulator"); Out.Ln; Out.Ln;
	} // DisplayCommands;

	static func RemoveLeadingSpaces(var s: String) {
	
		/* remove leading spaces in s */
		while s[0] == " " { Strings.Delete(s, 0, 1) } //
	} // RemoveLeadingSpaces;

	static func GetString (var s: String) {
	
		In.Line(s); RemoveLeadingSpaces(s)
	} // GetString;

	static func GetInt (var n: INTEGER) {
	var numb: LSB.Name;
	
		GetString(numb); n = Conv.IntVal(numb);
		if n == 0 { n = 1 } //
	} // GetInt;

	static func Interactive() {
	var n: INTEGER;
		cmd: CHAR;
		file: String
		name: LSB.Name;
	
		DisplayCommands;

		repeat {
			Out.String("> "); In.Char(cmd);
			switch CAP(cmd) {
				case "S" : GetInt(n); Label; Step(n)
				case "L" : GetString(file); LST.Import(file); Start; ClearSelect
				case "D" : GetString(name); Select(name); Label
				case "V" : GetString(file); Set(file)
				case "X" : /* just exit */
				case } else { GetString(file); DisplayCommands
			} //
		} while !(CAP(cmd) == "X";

/*
		ClearSelect;
		Select("s"); Select("c"); Select("co"); Select("x"); Select("y"); Select("ci");
		Start;
		Set("x", 3); Set("y", 7); Out.Ln;
		Label;
		Step(1)
		*/
	} // Interactive;


	DefOps;
    Out.String("Simulator  MG, NW 17.8.2014"); Out.Ln();
	sym[0] = "0"; sym[1] = "1"; sym[2] = "+"; sym[3] = "x";
	Interactive
} // Simulator.
