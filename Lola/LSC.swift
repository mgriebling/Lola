import Foundation

class LSC {

    /* Lola System Compiler, NW 8.1.95 / 29.10.96 */
    typealias GetItem =  (_ x: inout LSB.Item) -> ()
    typealias GetObject =  (_ x: inout LSB.Object) -> ()
    typealias Object = LSB.Object
    
    static var sym: LSS.Symbols = .null
    static var err: Bool = false
    static var top = LSB.Object()
    static var bot = LSB.root
    static var undef = LSB.Object(tag: 2, name: "", type: LSB.bitType, next: nil)
    static var factor: GetItem = factor0
    static var expression: GetItem! = expression0
    static var Unit: GetObject! = Unit0
    
    static func Err(_ n: Int) {
        LSS.mark("type error"); print(n)
    }
    
    static func Log(_ m: Int) -> Int {
        var n = 1, m = m
        while m > 1 {  m = m / 2; n+=1 }
        return n
    }
    
    static func NewObj(_ _class: Int) -> Object {
        var new: Object!
        var x: Object! = top
        while (x.next !== bot) && (x.next.name != LSS.id) { x = x.next }
        if x.next === bot {
            new = Object(tag: _class, name: LSS.id, type: nil, next: bot)
            x.next = new
        } else { LSS.mark("mult def"); new = x }
        return new
    } //NewObj
    
    /// find object with name = identifier last read
    static func ThisObj(_ id: LSS.Ident) -> Object? {
        var x : Object! = top.next
        while (x !== nil) && (x.name != id) { x = x.next }
        if x === nil { LSS.mark("undef"); x = undef }
        return x
    } //This

	/* -------------------- Parser ---------------------*/
    
    static func CheckTypes(_ x: LSB.Item, _ y:LSB.Item, _ z: LSB.Item) {  /*z.type = result type*/
        let xtyp = x.type; let ytyp = y.type; z.type = xtyp; z.size = x.size; z.val = x.val
        if xtyp === LSB.bitType {
            if ytyp === LSB.integer {  /* b + 0 */ }
            if y.val >= 2 { Err(20); LSS.mark("only 0 or 1") }
            else if ytyp === LSB.string { /* b + {...} */ Err(21) }
            else if ytyp !== LSB.bitType { Err(22) }
        } else if let xtyp = xtyp as? LSB.ArrayType {
            if xtyp.eltyp === LSB.bitType {
                if let ytyp = ytyp as? LSB.ArrayType, xtyp.eltyp === LSB.bitType {
                    if xtyp.size != ytyp.size { Err(33) }  /* x + y */
                } else if ytyp === LSB.integer {   /* w + 5 */
                    if xtyp.size < Log(y.val) { Err(30) }
                } else if ytyp === LSB.string {   /*x + {...} */
                    if xtyp.size != y.size { Err(31) }
                } else { Err(34)
                }
            }
            else if let ytyp = ytyp as? LSB.ArrayType, xtyp.eltyp === ytyp.eltyp {
                if xtyp.size != ytyp.size { Err(40) }
            } else { Err(41) }
        } else if xtyp === LSB.string {
            if ytyp === LSB.bitType {  /* {...} + b */ Err(12)
            } else if let ytyp = ytyp as? LSB.ArrayType, ytyp.eltyp === LSB.bitType {  /* {...} + w */
                if x.size != ytyp.size { Err(13) }
            } else if ytyp === LSB.integer {  /* {...} + 5*/
                if x.size < Log(y.val) { Err(10) }
            } else if ytyp === LSB.string {  /* {...} + {...} */
                if x.size != y.size { Err(11) }
            } else { Err(14) }
        } else if xtyp === LSB.integer {
            if let ytyp = ytyp as? LSB.ArrayType, ytyp.eltyp === LSB.bitType {  /* 5 + w */
                if Log(x.val) > ytyp.size { Err(3); LSS.mark("const too large") }
            } else if ytyp === LSB.bitType { /* 5 + b */
                if x.val >= 2 { Err(2) }
            } else if ytyp === LSB.integer {  /* 5 + 5 */
            } else if ytyp === LSB.string {  /* 5 + {...} */
                if Log(x.val) > y.size { Err(12) }
            } else { Err(4) }
        }
    } // CheckTypes
    
    static func selector(_ x: inout LSB.Item) {
        var y, z: LSB.Item!
        var eltyp: LSB.TType; var len, kind: Int;
        
        while (sym == LSS.Symbols.lbrak) || (sym == LSS.Symbols.period) {
            if sym == LSS.Symbols.lbrak {
                eltyp = (x.type as! LSB.ArrayType).eltyp; LSS.Get(&sym); expression(&y);
                if sym == LSS.Symbols.colon  {
                    /*range*/
                    LSS.Get(&sym); expression(&z);
                    if (y.tag == LSB.lit) && (z.tag == LSB.lit)  {
                        len = y.val - z.val + 1; y = LSB.Item(LSB.range, y, z);
                        x = LSB.Item(LSB.sel, x, y); x.type = LSB.string; x.size = len
                    }
                } else { kind = x.val; x = LSB.Item(LSB.sel, x, y); x.type = eltyp; x.val = kind
                }
                if sym == LSS.Symbols.rbrak { LSS.Get(&sym) } else { LSS.mark("rbrak ?") }
            } else {/*sym == LSS.period*/ LSS.Get(&sym); factor(&y);
                if (y.tag == LSB.lit) && (y.val >= x.type.len)  { LSS.mark("too large") }
                eltyp = (x.type as! LSB.ArrayType).eltyp; kind = x.val;
                x = LSB.Item(LSB.sel, x, y); x.type = eltyp; x.val = kind
            }
        }
    } // } selector;
        
    static func elem(_ x: inout LSB.Item, _ len: inout Int) {
        var y: LSB.Item; var m:Int, n:Int = 0
        expression(&x);
        if (x.type === LSB.integer) || (x.type === LSB.string)  { m = x.size } else {m = x.type.size }
        if sym == LSS.Symbols.repl  {
            LSS.Get(&sym);
            if sym == LSS.Symbols.integer  {
                y = LSB.Item(LSB.lit); n = LSS.val; y.val = n; y.type = LSB.integer; LSS.Get(&sym);
                x = LSB.Item(LSB.repl, x, y)
            }
        } else { n = 1 }
        len = m*n
    } // } elem;

    static func constructor(_ x: inout LSB.Item) {
        var y: LSB.Item!; var n = 0, len = 0
        elem(&x, &len);
        while sym == LSS.Symbols.comma {
            LSS.Get(&sym); elem(&y, &n); len += n; x = LSB.Item(LSB.cons, x, y); x.val = len
        }
        x.size = len; x.type = LSB.string;
        if sym == LSS.Symbols.rbrace { LSS.Get(&sym) } else { LSS.mark("rbrace ?") }
    } //constructor;

    static func factor0(_ x: inout LSB.Item) {
        var y: LSB.Item
        var n, len: Int
        
        if sym == LSS.Symbols.ident  {
            x = ThisObj(LSS.id)!; LSS.Get(&sym);
            if x.tag == LSB._var  { selector(&x)
            } else if x.tag == LSB.const { n = x.b.val; x = LSB.Item(LSB.lit); x.val = n; x.type = LSB.integer
            } else { LSS.mark("bad factor")
            }
        } else if sym == LSS.Symbols.lparen  {
            LSS.Get(&sym); expression(&x)
            if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else {LSS.mark("rparen ?") }
        } else if sym == LSS.Symbols.integer  {
            x=LSB.Item(LSB.lit); x.val = LSS.val; x.type = LSB.integer; LSS.Get(&sym);
            if sym == LSS.Symbols.apo { LSS.Get(&sym);
                if sym == LSS.Symbols.integer {
                    len = LSS.val; LSS.Get(&sym);
                    if len < Log(x.val)  { LSS.mark("value too large") }
                } else { LSS.mark("integer ?"); len = 0
                }
                x.size = len
            } else {len = 0
            }
            x.size = len
        } else if sym == LSS.Symbols.not  {
            LSS.Get(&sym); factor(&x); y = LSB.Item(LSB.not, nil, x); y.type = x.type; y.size = x.size; x = y
        } else if sym == LSS.Symbols.lbrace { LSS.Get(&sym); constructor(&x)
        } else { LSS.mark("bad factor")
        }
    } //factor0;

    static func term(_ x: inout LSB.Item) {
        var y, z: LSB.Item!; var op: Int = 0
        factor(&x);
        while (sym >= LSS.Symbols.times) && (sym <= LSS.Symbols.and) {
            if sym == LSS.Symbols.and  { op = LSB.and
            } else if sym == LSS.Symbols.times  { op = LSB.mul
            } else if sym == LSS.Symbols.div  { op = LSB.div
            }
            LSS.Get(&sym); factor(&y); z = LSB.Item(op, x, y); CheckTypes(x, y, z); x = z
        }
    } //term;
    
    static func SimpleExpression(_ x: inout LSB.Item) {
        var y, z: LSB.Item!; var op: Int = 0
        
        if sym == LSS.Symbols.minus  { LSS.Get(&sym); term(&y);
            if y.tag == LSB.lit  { x = y; x.val = -y.val
            } else { x = LSB.Item(LSB.sub, nil, y); x.type = y.type; x.size = y.size
            }
        } else if sym == LSS.Symbols.plus { LSS.Get(&sym); term(&x);
        } else { term(&x)
        }
        while (sym >= LSS.Symbols.plus) && (sym <= LSS.Symbols.xor) {
            if sym == LSS.Symbols.or  { op = LSB.or
            } else if sym == LSS.Symbols.xor  { op = LSB.xor
            } else if sym == LSS.Symbols.plus  { op = LSB.add
            } else if sym == LSS.Symbols.minus  { op = LSB.sub
            }
            LSS.Get(&sym); term(&y); z = LSB.Item(op, x, y); CheckTypes(x, y, z); x = z
        }
    } //SimpleExpression;

    static func UncondExpression(_ x: inout LSB.Item) {
        var y, z: LSB.Item!; var rel: Int;
        SimpleExpression(&x);
        if (sym >= LSS.Symbols.eql) && (sym <= LSS.Symbols.geq)  {
            if sym == LSS.Symbols.eql  { rel = LSB.eql
            } else if sym == LSS.Symbols.neq  { rel = LSB.neq
            } else if sym == LSS.Symbols.lss  { rel = LSB.lss
            } else if sym == LSS.Symbols.geq  { rel = LSB.geq
            } else if sym == LSS.Symbols.leq  { rel = LSB.leq
            } else { rel = LSB.gtr
            }
            LSS.Get(&sym); SimpleExpression(&y);
            z = LSB.Item(rel, x, y); CheckTypes(x, y, z); z.type = LSB.bitType; x = z
        }
    } //UncondExpression;

    static func expression0(_ x: inout LSB.Item) {
        var y, z, w: LSB.Item!
        UncondExpression(&x);
        if sym == LSS.Symbols.then  {
            if x.type !== LSB.bitType  { LSS.mark("Boolean?") }
            LSS.Get(&sym); expression(&y);
            if sym == LSS.Symbols.colon  {
                LSS.Get(&sym); expression(&z); w = LSB.Item(LSB._else, y, z); CheckTypes(y, z, w);
                x = LSB.Item(LSB.then, x, w); x.type = w.type; x.size = w.size
            } else { LSS.mark("colon ?")
            }
        }
    } //expression0;

    static func CheckAssign(_ x: LSB.Item, _ y:LSB.Item) {
        var xtyp, ytyp: LSB.TType;
        xtyp = x.type; ytyp = y.type;
        if xtyp !== ytyp  {
            if xtyp === LSB.bitType  {
                if (ytyp !== LSB.integer) || (y.val >= 2)  { Err(70); }
            } else if let xtyp = xtyp as? LSB.ArrayType  {
                if xtyp.eltyp === LSB.bitType  {
                    if ytyp is LSB.ArrayType && xtyp.eltyp === LSB.bitType  { /*w = w*/
                        if xtyp.size != ytyp.size  { Err(71) }  /* x + y */
                    } else if ytyp === LSB.integer  {   /* w = 5 */
                        if xtyp.size < Log(y.val)  { Err(72) }
                    } else if ytyp === LSB.string  {   /* w = {...} */
                        if xtyp.size != y.size  { Err(73) }
                    } else { Err(74) }
                } else { Err(74) }
            }
        }
    } //CheckAssign;

    static func Param (_ fpar: LSB.Object, _ apar: inout LSB.Item) {
        var y: LSB.Item!
        expression(&y); apar = LSB.Item(LSB.next, nil, y); CheckAssign(fpar, y)
        if [3, 4].contains(fpar.val)  {  /*OUT or INOUT parameter*/
            if ![3, 7].contains(y.tag)  {  /*actual param is expression?*/ LSS.mark("bad actual param")
            } else if y.b === nil  { y.b = undef
            }
        }
    } //Param;

    static func Statement() {
        var x, y, z, w, apar, npar: LSB.Item!
        var unit: LSB.UnitType; var fpar: LSB.Object;
        
        if sym < LSS.Symbols.ident { LSS.mark("bad factor");
            repeat { LSS.Get(&sym) } while sym < LSS.Symbols.ident
        }
        if sym == LSS.Symbols.ident  {
            x = ThisObj(LSS.id)!; z = x; LSS.Get(&sym); selector(&z);
            if sym == LSS.Symbols.becomes  { LSS.Get(&sym);
                if x.val >= 5  { LSS.mark("assignment to read-only") }
                if (x.b != nil) && !(x.type is LSB.ArrayType)  { LSS.mark("mult assign") }
                expression(&y); CheckAssign(z, y); x.b = y; /*tricky*/
                if z !== x  { x.a = z.b; x.val = 1 /*overwriting clk field x.a */ }
            } else if sym == LSS.Symbols.lparen  { LSS.Get(&sym);  /*unit instantiation*/
                if x.type is LSB.UnitType  {
                    unit = (x.type as! LSB.UnitType); fpar = unit.firstobj;
                    if sym != LSS.Symbols.rparen  {
                        Param(fpar, &apar); x.b = apar; fpar = fpar.next;
                        while sym != LSS.Symbols.rparen {
                            if sym == LSS.Symbols.comma  { LSS.Get(&sym) }
                            Param(fpar, &npar);
                            if fpar.tag >= 3  { fpar = fpar.next; apar.a = npar; apar = npar
                            } else { LSS.mark("too many params")
                            }
                        }
                        if fpar.val >= 3  { LSS.mark("too few params") }
                    }
                    if sym == LSS.Symbols.rparen  { LSS.Get(&sym) } else {LSS.mark("rparen ?") }
                } else { LSS.mark("not a module")
                }
            } else { LSS.mark("bad statement")
            }
        } else if sym == LSS.Symbols.ts  {  /*tri-state*/ LSS.Get(&sym);
            if sym == LSS.Symbols.lparen  { LSS.Get(&sym) } else {LSS.mark("( missing") }
            if sym == LSS.Symbols.ident  {
                x = ThisObj(LSS.id)!; x.b = undef;  /*INOUT parameter*/
                if x.val != 5  { LSS.mark("not INOUT") }
                LSS.Get(&sym);
                if sym == LSS.Symbols.comma  { LSS.Get(&sym) }
                if sym == LSS.Symbols.ident  { y = ThisObj(LSS.id)!; CheckAssign(x, y); y.b = undef }  /*output from gate*/
                LSS.Get(&sym);
                if sym == LSS.Symbols.comma  { LSS.Get(&sym) }
                expression(&z);
                if (z.tag == LSB.lit) && (z.val <= 1)  { z.type = LSB.bitType }
                CheckAssign(x, z); LSS.Get(&sym);
                if sym == LSS.Symbols.comma  { LSS.Get(&sym) }
                expression(&w);  /*control*/
                if w.type !== LSB.bitType  { CheckAssign(x, w) } //;
                w = LSB.Item(LSB.next, z, w); x.b = LSB.Item(LSB.ts, y, w);
                if sym == LSS.Symbols.rparen  { LSS.Get(&sym) } else { LSS.mark(") missing") }
            }
        }
    } //Statement;
    
    static func StatSequence() {
        Statement()
        while sym <= LSS.Symbols.semicolon {
            if sym < LSS.Symbols.semicolon { LSS.mark("semicolon missing?") }
            while sym == LSS.Symbols.semicolon { LSS.Get(&sym) }
            Statement()
        }
        if sym == LSS.Symbols.end { LSS.Get(&sym) } else {LSS.mark("} ?") }
    } //StatSequence;
    
    /*---------------------------------------------------*/
    
    /* for variables and registers,, obj.val has the meaning
        0  register
        1  register with imlicit clock "clk"
        2  variable
        3  output parameter
        4  output parameter with register
        5  inout parameter
        6  input parameter  */
    
    static func ConstDeclaration () {
        var obj: LSB.Object;
        
        if sym == LSS.Symbols.ident  {
            obj = NewObj(LSB.const); LSS.Get(&sym);
            if (sym == LSS.Symbols.becomes) || (sym == LSS.Symbols.eql)  { LSS.Get(&sym) } else {LSS.mark("= ?") }
            expression(&obj.b); obj.type = LSB.integer;
            if sym == LSS.Symbols.semicolon  { LSS.Get(&sym) } else {LSS.mark("semicolon ?") }
        } else {LSS.mark("ident ?")
        }
    } //ConstDeclaration;
    
    static func Type0(_ type: inout LSB.TType) {
        var obj: LSB.Object!; var len: Int;
        var eltyp: LSB.TType!; var arrtyp: LSB.ArrayType;
        len = 1;
        if sym == LSS.Symbols.lbrak  { /*array*/ LSS.Get(&sym);
            if sym == LSS.Symbols.integer  { len = LSS.val; LSS.Get(&sym)
            } else if sym == LSS.Symbols.ident  { obj = ThisObj(LSS.id); len = obj.val
            }
            if sym == LSS.Symbols.rbrak  { LSS.Get(&sym) } else {LSS.mark("rbrak ?") }
            Type0(&eltyp); arrtyp=LSB.ArrayType(len: len, size: eltyp.size * len, type: eltyp)
            type = arrtyp
        } else if sym == LSS.Symbols.ident  {
            obj = ThisObj(LSS.id); LSS.Get(&sym);
            if obj !== nil {
                if obj.tag == LSB.typ  { type = obj.type } else { LSS.mark("not a type"); type = LSB.bitType }
            } else { LSS.mark("type ?")
            }
        } else { type = LSB.bitType; LSS.mark("ident or [")
        }
    } //Type0;
    
    static func TypeDeclaration () {
        var obj: LSB.Object; var utyp: LSB.UnitType;
        
        if sym == LSS.Symbols.ident  {
            obj = NewObj(LSB.typ); LSS.Get(&sym);
            if (sym == LSS.Symbols.becomes) || (sym == LSS.Symbols.eql)  { LSS.Get(&sym) } else {LSS.mark("= ?") }
            if sym == LSS.Symbols.module  {
                LSS.Get(&sym); utyp = LSB.UnitType(); Unit(&utyp.firstobj); obj.type = utyp; obj.type.typobj = obj
            } else { Type0(&obj.type)
            }
            if sym == LSS.Symbols.semicolon  { LSS.Get(&sym) } else {LSS.mark("semicolon ?") }
        } else { LSS.mark("ident ?")
        }
    } //TypeDeclaration;
    
    static func VarList(_ kind: Int, _ clk: LSB.Item!) {
        var first, new, obj: LSB.Object!; var type: LSB.TType!
        obj = nil
        while sym == LSS.Symbols.ident {
            new = NewObj(LSB._var); new.name = LSS.id; new.val = kind; first = new; LSS.Get(&sym);
            if sym == LSS.Symbols.comma  { LSS.Get(&sym) } else if sym == LSS.Symbols.ident  { LSS.mark("comma missing") }
            while sym == LSS.Symbols.ident {
                new = NewObj(LSB._var); new.name = LSS.id; new.val = kind; LSS.Get(&sym);
                if sym == LSS.Symbols.comma  { LSS.Get(&sym) } else if sym == LSS.Symbols.ident  { LSS.mark("comma missing") }
            }
            if sym == LSS.Symbols.colon  {
                LSS.Get(&sym); Type0(&type); obj = first;
                while obj !== bot { obj.type = type; obj.a = clk; obj = obj.next }
            } else { LSS.mark("colon ?")
            }
            if sym == LSS.Symbols.semicolon  { LSS.Get(&sym)
            } else if sym != LSS.Symbols.rparen  { LSS.mark("semicolon or rparen missing")
            }
        }
    } //VarList;
    
    static func ParamList () {
        var kind: Int = 0
        
        if sym == LSS.Symbols.in  { LSS.Get(&sym); kind = 6
        } else if sym == LSS.Symbols.out  { LSS.Get(&sym);
            if sym == LSS.Symbols.reg  { LSS.Get(&sym); kind = 4 } else {kind = 3 }
        } else if sym == LSS.Symbols.inout  { LSS.Get(&sym); kind = 5
        }
        VarList(kind, nil)
    } //ParamList;
    
    static func Traverse(_ x: LSB.Item!) {
        if x !== nil {
            if let x = x as? LSB.Object  {
                if x.tag == LSB._var && x.val >= 2 {  /*not reg*/
                    if x.marked  { /*loop*/
                        print(x.name, " ", terminator: ""); err = true
                    } else if x.b !== nil  { x.marked = true; Traverse(x.b)
                    }
                    x.marked = false
                }
            } else {
                Traverse(x.a); Traverse(x.b)
            }
        }
    } //Traverse;
    
    static func Unit0(_ locals: inout LSB.Object) {
        var obj, oldtop: LSB.Object; var kind: Int; var clock: LSB.Item!
        oldtop = top.next; top.next = LSB.root;  /*top is dummy*/
        if sym == LSS.Symbols.lparen  { LSS.Get(&sym) } else { LSS.mark("lparen ?") }
        while (sym == LSS.Symbols.in) || (sym == LSS.Symbols.out) || (sym == LSS.Symbols.inout) { ParamList() }
        if sym == LSS.Symbols.rparen  { LSS.Get(&sym) } else { LSS.mark("rparen ?") }
        if sym == LSS.Symbols.xor /*arrow*/  { LSS.Get(&sym); locals = top.next
        } else {
            if sym == LSS.Symbols.semicolon  { LSS.Get(&sym) } else {LSS.mark("semicolon ?") }
            if sym == LSS.Symbols.const  { LSS.Get(&sym);
                while sym == LSS.Symbols.ident { ConstDeclaration() }
            }
            if sym == LSS.Symbols.type  { LSS.Get(&sym);
                while sym == LSS.Symbols.ident { TypeDeclaration() }
            }
            while (sym == LSS.Symbols.var) || (sym == LSS.Symbols.reg) {
                if sym == LSS.Symbols.var  { LSS.Get(&sym);
                    while sym == LSS.Symbols.ident { VarList(2, nil) }
                } else {/*reg*/ kind = 0; LSS.Get(&sym);
                    if sym == LSS.Symbols.lparen  { /*clock*/
                        LSS.Get(&sym); expression(&clock);
                        if clock.type !== LSB.bitType { LSS.mark("clock must be bitType") }
                        if let clk = clock as? LSB.Object, clk.name == "clk" { kind = 1; clock = nil }
                        if sym == LSS.Symbols.rparen { LSS.Get(&sym) } else { LSS.mark("rparen ?") }
                    } else { LSS.mark("lparen expected"); clock = undef
                    }
                    while sym == LSS.Symbols.ident { VarList(kind, clock) }
                }
            }
            locals = top.next;
            if sym == LSS.Symbols.begin { LSS.Get(&sym); StatSequence() }
            obj = locals; err = false;  /*find unassigned variables*/
            while obj !== LSB.root {
                if (obj.tag == LSB._var) && (obj.val < 5) {
                    if (obj.b === nil) && (obj.val < 4) {
                       print(obj.name, " ", terminator: ""); err = true
                    } else if obj.b === undef  { obj.b = nil
                    }
                }
                obj = obj.next
            }
            if err { print(" unassigned")
            } else { obj = locals; err = false;  /*find combinatorial loops*/
                while obj !== LSB.root {
                    if obj.tag == LSB._var { obj.marked = true; Traverse(obj.b); obj.marked = false }
                    obj = obj.next
                }
                if err { print("in loop") }
            }
        }
        top.next = oldtop
    } //Unit0;
    
    static public func Module (_ name : String) {
        var root: LSB.Object!; var modname = ""
        print("compiling Lola: ", terminator: ""); LSS.error = false
        bot = LSB.root; top.next = bot; LSS.Init(name); LSS.Get(&sym);
        if sym == LSS.Symbols.module  {
            LSS.Get(&sym);
            if sym == LSS.Symbols.ident  {
                modname = LSS.id; print(LSS.id, terminator: ""); LSS.Get(&sym);
                print()
            } else { LSS.mark("ident ?")
            }
            Unit(&root)
            if sym == LSS.Symbols.ident { LSS.Get(&sym);
                if LSS.id != modname { LSS.mark("no match") }
            }
            if sym != LSS.Symbols.period { LSS.mark("period ?") }
            if !LSS.error { print(); LSB.Register(modname, root)
            } else { print("compilation failed"); LSB.Register("", LSB.root)
            }
        } else { LSS.mark("module ?")
        }
    } //Module;
    
//    static public func Compile() {
//        var beg, end, time: Int
//        var S: Texts.Scanner; T: Texts.Text;
//        Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
//        if S.class == Texts.Char {
//            if S.c == "*" {
//            } else if S.c == "@" {
//                Oberon.GetSelection(T, beg, end, time);
//                if time >= 0 { Module(T, beg) }
//            }
//        } else if S.class == Texts.Name {
//            NEW(T); Texts.Open(T, S.s); Module(T, 0)
//        }
//        Texts.Append(Oberon.Log, W.buf)
//    } // Compile;


} //LSC.
