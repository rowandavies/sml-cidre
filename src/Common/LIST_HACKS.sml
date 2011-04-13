(*$LIST_HACKS*)
(* LIST_HACKS provides operations to treat lists as sets. This is temporary,
   since the elaboration phases haven't been ported to use the SML Library's
   sets yet. *)

signature LIST_HACKS =
  sig
    val union: ''a list * ''a list -> ''a list
    val unionEq: ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
    val intersect: ''a list * ''a list -> ''a list
    val minus: ''a list * ''a list -> ''a list
    val eqSet: ''a list * ''a list -> bool
    val reverse: 'a list -> 'a list
    (* add an element to a list of maximal elements wrt a partial order *)
    val addMax: ('a * 'a -> bool) -> 'a * 'a list -> 'a list
    val flatten: 'a list list -> 'a list
    val foldl': ('a * 'a -> 'a) -> 'a list -> 'a
    val foldr': ('a * 'a -> 'a) -> 'a list -> 'a
    val stringSep: string -> string -> string -> ('a -> string) -> 'a list -> string
  end;
