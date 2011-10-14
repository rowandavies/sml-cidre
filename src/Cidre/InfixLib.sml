(* Handy top level defintions from http://mlton.org/InfixingOperators *)
structure InfixingLib = struct

  infix  3 <\     fun x <\ f = fn y => f (x, y)     (* Left section      *)
  infix  3 \>     fun f \> y = f y                  (* Left application  *)
  infixr 3 />     fun f /> y = fn x => f (x, y)     (* Right section     *)
  infixr 3 </     fun x </ f = f x                  (* Right application *)

  infix  2 o  (* See motivation below *)
  infix  0 := 

  infix  1 >|     val op>| = op</      (* Left pipe *)
  infixr 1 |<     val op|< = op\>      (* Right pipe *) 

end
