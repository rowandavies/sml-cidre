(*[ datasort 'a nillist = nil ]*)
(*[ datasort 'a conslist = :: of 'a * 'a list ]*)

signature LIST = sig

include LIST

(*[ val null: 'a nillist -> true
            & 'a conslist -> false
            & 'a list -> bool ]*)

(*[ val hd: 'a conslist -> 'a ]*)

(*[ val tl: 'a conslist -> 'a ]*)

(*[ val last: 'a conslist -> 'a ]*)

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a nillist -> ('a * 'a list) none
               & 'a list -> ('a * 'a list) option ]*)

(*[ val rev: 'a conslist -> 'a conslist
           & 'a nillist -> 'a nillist
           & 'a list -> 'a list ]*)

(*[ val @: 'a list * 'a conslist -> 'a conslist 
         & 'a conslist * 'a list -> 'a conslist
         & 'a nillist * 'a nillist -> 'a nillist
         & 'a list * 'a list -> 'a list ]*)

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a nillist * 'a nillist -> 'a nillist
                 & 'a list * 'a list -> 'a list ]*)
   
(*[ val app: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a nillist -> 'b nillist
           & ('a -> 'b) -> 'a list -> 'b list ]*)

end

structure List = struct

open List

(*[ ]*)

end

val map = List.map
