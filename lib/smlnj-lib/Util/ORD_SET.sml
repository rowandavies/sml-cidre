signature ORD_SET = sig
structure Key : ORD_KEY
type item = Key.ord_key
type set
val empty : set
val singleton : item -> set
val add : (set * item) -> set
val add' : (item * set) -> set
val addList : (set * item list) -> set
val delete : (set * item) -> set
val member : (set * item) -> bool
val isEmpty : set -> bool
val equal : (set * set) -> bool
val compare : (set * set) -> order
val isSubset : (set * set) -> bool
val numItems : set -> int
val listItems : set -> item list
val union : (set * set) -> set
val intersection : (set * set) -> set
val difference : (set * set) -> set
val map : (item -> item) -> set -> set
val app : (item -> unit) -> set -> unit
val foldl : ((item * 'b) -> 'b) -> 'b -> set -> 'b
val foldr : ((item * 'b) -> 'b) -> 'b -> set -> 'b
val filter : (item -> bool) -> set -> set
val exists : (item -> bool) -> set -> bool
val find : (item -> bool) -> set -> item option 
end
