(*General.sml*)

  (* See the top-level at: http://www.standardml.org/Basis/top-level-chapter.html
     and the General structure: http://www.standardml.org/Basis/general.html *)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

structure General : GENERAL = 
  struct (*[ assumesig GENERAL ]*)
  end (*structure General*)

open General


(* Top-level identifiers; Some are here - some are introduced later depending on *)
(* which were required by other structures in the ML Kit.  (Revisit 18oct11) *)
fun op = (x: ''a, y: ''a): bool = prim ("=", "=", (x, y))

fun not true = false
  | not false = true

fun a <> b = not (a = b)


fun print (s:string) : unit = raise Match (* prim("printString", "printString", s) *)

fun implode (chars : char list) : string = prim ("implodeChars", "implodeCharsProfiling", chars)
fun concat (ss : string list) : string = prim ("implodeString", "implodeStringProfiling", ss)
fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))
fun str (c : char) : string = implode [c]
fun size (s:string): int = prim ("sizeString", "sizeString", s)
local fun sub_unsafe (s:string,i:int) : char = prim ("subString", "subString", (s,i))
in fun explode s =
     let fun h j res = if j<0 then res
		       else h (j-1) (sub_unsafe (s, j) :: res)
     in h (size s - 1) []
     end

end
