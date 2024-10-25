A digital design compiler and simulator to produce and test designs based on a Lola circuit description. 
Invented by Niklaus Wirth (inventor of Pascal and many other languages) Lola is similar to other high-level circuit design languages like VHDL and Verilog except much simpler to learn and use.

For example, here's the design of a 4-bit binary counter:

```
MODULE Counter0 (IN clk: BIT; OUT d: [4] BIT);
  REG (clk) R: [4] BIT;
BEGIN
  R := {R.3 ^ R.3 & R.2 & R.1 & R.0, (* R.3 *)
        R.2 ^ R.2 & R.1 & R.0,       (* R.2 *)
        R.1 ^ R.1 & R.0,             (* R.1 *)
        ~R.0};                       (* R.0 *)
  d := R
END Counter0.
```
