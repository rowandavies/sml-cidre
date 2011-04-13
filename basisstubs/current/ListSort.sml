structure ListSort:LIST_SORT =
struct(*[ assumesig LIST_SORT ]*)

    val sort   : ('a * 'a -> order) -> 'a list -> 'a list = fn _ => raise Match
    val sorted : ('a * 'a -> order) -> 'a list -> bool = fn _ => raise Match

  (* 
   [sort ordr xs] sorts the list xs in nondecreasing order, using the
   given ordering.  Uses Richard O'Keefe's smooth applicative merge
   sort.
   
   [sorted ordr xs] checks that the list xs is sorted in nondecreasing
   order, in the given ordering.
   *)

  end
