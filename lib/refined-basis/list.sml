(*[ datasort 'a conslist = :: of 'a * 'a list ]*)

signature LIST = sig

include LIST

(*[ val null: 'a conslist -> false
            & 'a list -> bool ]*)

(*[ val hd: 'a conslist -> 'a ]*)

(*[ val tl: 'a conslist -> 'a list ]*)

(*[ val last: 'a conslist -> 'a ]*)

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

(*[ val null: 'a conslist -> false
            & 'a list -> bool ]*)
fun null _ = raise Match

(*[ val hd: 'a conslist -> 'a ]*)
fun hd _ = raise Match

(*[ val tl: 'a conslist -> 'a list ]*)
fun tl _ = raise Match

(*[ val last: 'a conslist -> 'a ]*)
fun last _ = raise Match

(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a list -> ('a * 'a list) option ]*)
fun getItem _ = raise Match

(*[ val rev: 'a conslist -> 'a conslist
           & 'a list -> 'a list ]*)
fun rev _ = raise Match

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a list * 'a list -> 'a list ]*)
fun revAppend _ = raise Match   

end

val map = List.map
