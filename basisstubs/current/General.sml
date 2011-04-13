(*General.sml*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

structure General : GENERAL = 
  struct (*[ assumesig GENERAL ]*)
(*    type unit = unit
    type exn = exn
    type 'a ref = 'a ref

    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string 
  
    fun exnName (e: exn) : string = prim("exnName", "exnNameProfiling", e)   (* exomorphic by copying *)
    fun exnMessage (e: exn) : string = exnName e 

    datatype 'a option = NONE | SOME of 'a
    exception Option
    fun getOpt (NONE, a) = a
      | getOpt (SOME a, b) = a
    fun isSome NONE = false
      | isSome _ = true
    fun valOf (SOME a) = a
      | valOf _ = raise Option

    datatype order = LESS | EQUAL | GREATER

    fun !(x: 'a ref): 'a = prim ("!", "!", x) 
    fun (x: 'a ref) := (y: 'a): unit = prim (":=", ":=", (x, y)) 
    fun (f o g) x = f(g x)
    fun a before () = a
    fun ignore (a) = ()
*)
  end (*structure General*)

open General


(* Top-level identifiers; Some are here - some are introduced later *)
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
