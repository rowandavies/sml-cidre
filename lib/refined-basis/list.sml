(*[ datasort 'a conslist = :: of 'a * 'a list ]*)

signature LIST = sig

include LIST

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a list -> ('a * 'a list) option ]*)

(*[ val rev: 'a conslist -> 'a conslist
           & 'a list -> 'a list ]*)

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a list * 'a list -> 'a list ]*)
   
(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)

end

structure List:> LIST = struct

open List

(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a list -> ('a * 'a list) option ]*)
fun getItem [] = NONE
  | getItem (x :: xs) = SOME (x, xs)

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a list * 'a list -> 'a list ]*)
fun revAppend ([], ys) = ys 
  | revAppend (x :: xs, ys) = revAppend (xs, x :: ys)

(*[ val rev: 'a conslist -> 'a conslist
           & 'a list -> 'a list ]*)
fun rev xs = revAppend (xs, [])


end

val map = List.map
