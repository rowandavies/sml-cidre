(* Modification of red-black.sml to test constructor types and inv. princ.  - Rowan 31jul02 *)


(*General.sml*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

    type unit = unit
    type exn = exn
    type 'a ref = 'a ref

(*
    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string 
*)
(*  
    fun exnName (e: exn) : string = prim("exnName", "exnNameProfiling", e)   (* exomorphic by copying *)
    fun exnMessage (e: exn) : string = exnName e 
*)
    datatype 'a option = NONE | SOME of 'a
(*    exception Option
    fun getOpt (NONE, a) = a
      | getOpt (SOME a, b) = a
    fun isSome NONE = false
      | isSome _ = true
    fun valOf (SOME a) = a
(*       | valOf _ = raise Option *)
*)
    datatype order = LESS | EQUAL | GREATER

    fun !(x: 'a ref): 'a = ! x
    fun (x: 'a ref) := (y: 'a): unit = (x := y)
    fun (f o g) x = f(g x)
    fun a before () = a
    fun ignore (a) = ()


(* Top-level identifiers; Some are here - some are introduced later *)

fun op = (x: ''a, y: ''a): bool =  (x = y)

fun not true = false
  | not false = true

fun a <> b = not (a = b)

fun print (s:string) : unit = ()

(*  
fun implode (chars : char list) : string = ""
fun concat (ss : string list) : string = ""
fun (s : string) ^ (s' : string) : string = ""
fun str (c : char) : string = ""
fun size (s:string): int = 0
fun explode (s:string) : char list = explode s 
*)

(* Red/Black Trees *)

(* Tables *)
(* This provides a common interface to hash tables *)
(* red/black trees and similar data structures *)

(*
datatype 'a option = NONE | SOME of 'a;


signature TABLE =
sig
  type key (* parameter *)
  type 'a entry (* = key * 'a *)

  type 'a Table
  val new : int -> 'a Table		(* includes size hint *)
  val insert : 'a Table -> 'a entry -> unit
  val insertShadow : 'a Table -> 'a entry -> ('a entry) option
  val lookup : 'a Table -> key -> 'a option
  val clear : 'a Table -> unit

  val app : ('a entry -> unit) -> 'a Table -> unit
end;
*)

fun compare (s1:int,s2:int) =
    if s1 = s2 then EQUAL
    else if s1 < s2 then LESS
	 else GREATER;

(*
structure RedBlackTree (* : TABLE *) =
struct
*)
  type key = int
  type 'a entry = key * 'a

  datatype 'a dict =                                     (* general dictionaries *)
    Empty | Black of 'a entry * 'a dict * 'a dict        (* Empty is considered black *)
  | Red of 'a entry * 'a dict * 'a dict

(*[  datasort 'a rbt =  Empty | Black of 'a entry * 'a rbt * 'a rbt   (* red/black trees *)
                   | Red of 'a entry * 'a bt * 'a bt
       and 'a bt = Empty | Black of 'a entry * 'a rbt * 'a rbt     (* black root node *)  ]*)
(*[  datasort 'a red = Red of 'a entry * 'a bt * 'a bt             ]*)   (* red root node *)

(*[  datasort 'a badRoot                    (* invariant possibly violated at the root *)
     = Empty | Black of 'a entry * 'a rbt * 'a rbt | Red of 'a entry * 'a rbt * 'a bt
     | Red of 'a entry * 'a bt * 'a rbt  ]*)  (* MODIFIED HERE *)

(*[  datasort 'a badLeft               (* invariant possibly violated at the left child *)
     = Empty | Black of 'a entry * 'a rbt * 'a rbt | Red of 'a entry * 'a bt * 'a bt
     | Black of 'a entry * 'a badRoot * 'a rbt   ]*)

(*[  datasort 'a badRight              (* invariant possibly violated at the right child *)
     = Empty | Black of 'a entry * 'a rbt * 'a rbt | Red of 'a entry * 'a bt * 'a bt
     | Black of 'a entry * 'a rbt * 'a badRoot  ]*)

  type 'a Table = 'a dict ref

  (* Representation Invariants *)
  (*
     1. The tree is ordered: for every node Red((key1,datum1), left, right) or
        Black ((key1,datum1), left, right), every key in left is less than
        key1 and every key in right is greater than key1.

     2. The children of a red node are black (color invariant).

     3. Every path from the root to a leaf has the same number of
        black nodes, called the black height of the tree.
  *)

(*   local *)

(*  (*[ val  lookup :> 'a rbt -> key -> 'a option ]*)*)
(*  fun lookup (dict:'a dict) key =*)
(*    let*)
(*      fun lk (Empty) = NONE*)
(*	| lk (Red tree) = lk' tree*)
(*        | lk (Black tree) = lk' tree*)
(*      and lk' ((key1, datum1), left, right) =*)
(*	    (case compare(key,key1)*)
(*	       of EQUAL => SOME(datum1)*)
(*	        | LESS => lk left*)
(*		| GREATER => lk right)*)
(*      in*)
(*	lk dict*)
(*      end*)


(*  (*[ val restore_right :> 'a badRight -> 'a rbt ]*)*)
(*  (**)
(*     restore_right (Black(e,l,r)) >=> dict*)
(*     where (1) Black(e,l,r) is ordered,*)
(*           (2) Black(e,l,r) has black height n,*)
(*	   (3) color invariant may be violated at the root of r:*)
(*               one of its children might be red.*)
(*     and dict is a re-balanced red/black tree (satisfying all invariants)*)
(*     and same black height n.*)
(*  *)*)
(*  fun restore_right (Black(e:'a entry, Red lt, Red (rt as (_,Red _,_)))) =*)
(*         Red(e, Black lt, Black rt)	(* re-color *)*)
(*    | restore_right (Black(e, Red lt, Red (rt as (_,_,Red _)))) =*)
(*         Red(e, Black lt, Black rt)	(* re-color *)*)
(*    | restore_right (Black(e, l, Red(re, Red(rle, rll, rlr), rr))) =*)
(*	 (* l is black, deep rotate *)*)
(*	 Black(rle, Red(e, l, rll), Red(re, rlr, rr))*)
(*    | restore_right (Black(e, l, Red(re, rl, rr as Red _))) =*)
(*	 (* l is black, shallow rotate *)*)
(*	 Black(re, Red(e, l, rl), rr)*)
(*    | restore_right dict = dict*)

(*  (*[ val restore_left :> 'a badLeft -> 'a rbt ]*)*)
(*  (* restore_left is like restore_right, except *)*)
(*  (* the color invariant may be violated only at the root of left child *)*)
(*  fun restore_left (Black(e:'a entry, Red (lt as (_,Red _,_)), Red rt)) =*)
(*	 Red(e, Black lt, Black rt)	(* re-color *)*)
(*    | restore_left (Black(e, Red (lt as (_,_,Red _)), Red rt)) =*)
(*	 Red(e, Black lt, Black rt)	(* re-color *)*)
(*    | restore_left (Black(e, Red(le, ll as Red _, lr), r)) =*)
(*	 (* r is black, shallow rotate *)*)
(*	 Black(le, ll, Red(e, lr, r))*)
(*    | restore_left (Black(e, Red(le, ll, Red(lre, lrl, lrr)), r)) =*)
(*	 (* r is black, deep rotate *)*)
(*	 Black(lre, Red(le, ll, lrl), Red(e, lrr, r))*)
(*    | restore_left dict = dict*)

(*  (*[ val insert :> 'a rbt * 'a entry -> 'a rbt ]*)*)
(*  fun 'b insert (dict, entry as (key,datum)) =*)
(*    let*)
(*      (* val ins : 'a dict -> 'a dict  inserts entry *)*)
(*      (* ins (Red _) may violate color invariant at root *)*)
(*      (* ins (Black _) or ins (Empty) will be red/black tree *)*)
(*      (* ins preserves black height *)*)
(*      (*[ val ins1 :> 'b rbt -> 'b badRoot & 'b bt -> 'b rbt  ]*)*)
(*      fun ins1 (Empty) = Red(entry, Empty, Empty)*)
(*	| ins1 (Red(entry1 as (key1, datum1), left, right)) =*)
(*	  (case compare(key,key1)*)
(*	     of EQUAL => Red(entry, left, right)*)
(*	      | LESS => Red(entry1, ins1 left, right)*)
(*	      | GREATER => Red(entry1, left, ins1 right))*)
(*	| ins1 (Black(entry1 as (key1, datum1), left, right)) =*)
(*	  (case compare(key,key1)*)
(*	     of EQUAL => Black(entry, left, right)*)
(*	      | LESS => restore_left (Black(entry1, ins1 left, right))*)
(*	      | GREATER => restore_right (Black(entry1, left, ins1 right)))*)
(*    in                (* the second conjuct is needed for the recursive cases *)*)
(*      case ins1 dict*)
(*	of Red (t as (_, Red _, _)) => Black t (* re-color *)*)
(*	 | Red (t as (_, _, Red _)) => Black t (* re-color *)*)
(*	 | dict => dict                        (* depend on sequential matching *)*)
(*    end*)


(*  (* use non-imperative version? *)*)
(*  (*[ val insertShadow :> 'a rbt * 'a entry -> 'a rbt * 'a entry option ]*)*)
(*  fun 'a insertShadow (dict, entry as (key,datum)) =*)
(*      let val oldEntry = ref NONE (* : 'a entry option ref *)*)
(*          (*[ val  ins :> 'a rbt -> 'a badRoot  & 'a bt -> 'a rbt   ]*)*)
(*          fun ins (Empty) = Red(entry, Empty, Empty)*)
(*	    | ins (Red(entry1 as (key1, datum1), left, right)) =*)
(*	      (case compare(key,key1)*)
(*		 of EQUAL => (oldEntry := SOME(entry1);*)
(*			      Red(entry, left, right))*)
(*	          | LESS => Red(entry1, ins left, right)*)
(*	          | GREATER => Red(entry1, left, ins right))*)
(*	    | ins (Black(entry1 as (key1, datum1), left, right)) =*)
(*	      (case compare(key,key1)*)
(*		 of EQUAL => (oldEntry := SOME(entry1);*)
(*			      Black(entry, left, right))*)
(*	          | LESS => restore_left (Black(entry1, ins left, right))*)
(*	          | GREATER => restore_right (Black(entry1, left, ins right)))*)
(*      in*)
(*	(oldEntry := NONE;*)
(*	 ((case ins dict*)
(*	     of Red (t as (_, Red _, _)) => Black t (* re-color *)*)
(*	      | Red (t as (_, _, Red _)) => Black t (* re-color *)*)
(*	      | dict => dict),*)
(*	  !oldEntry))*)
(*      end*)
  
(*  (*[ val app :> ('a entry -> unit) -> 'a rbt -> unit ]*)*)
(*  fun app (f:'a entry -> unit) dict =*)
(*      let fun ap (Empty) = ()*)
(*	    | ap (Red tree) = ap' tree*)
(*	    | ap (Black tree) = ap' tree*)
(*	  and ap' (entry1, left, right) =*)
(*	      (ap left; f entry1; ap right)*)
(*      in*)
(*	ap dict*)
(*      end*)

(*
  in
    fun new (n:int) = ref (Empty : 'a dict) (* ignore size spec *)
        withsort new :> int -> 'a rbt ref
    val insert = (fn table => fn entry : 'a entry => (table := insert (!table, entry)))
        :> 'a rbt ref -> 'a entry -> unit
    val insertShadow =
        (fn table => fn entry : 'a entry => 
	 let
	   val (dict, oldEntry) = insertShadow (!table, entry)
	 in
	   (table := dict; oldEntry)
	 end)
        :> 'a rbt ref -> 'a entry -> 'a entry option
    val lookup = (fn table : 'a dict ref => fn key => lookup (!table) key)
        :> 'a rbt ref -> int -> 'a  option
    val clear = (fn table : 'a dict ref => (table := Empty))
        :> 'a rbt ref -> unit
    val app = (fn f => fn table : 'a dict ref => app f (!table))
        :> ('a entry -> unit) -> 'a rbt ref -> unit
  end
end;  (* structure RedBlackTree *)
*)