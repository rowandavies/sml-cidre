
          
(* Suppose we have a type num, containing zero and one, and refinements for nat and pos.
   We can encode these assumptions via a functor. *)
functor IntroUnsoundRef
         (Num : sig  
                 type num             (*[ sortdef nat < num ]*)
                                      (*[ sortdef pos < nat ]*)
                 val zero : num       (*[ val zero <: nat   ]*)
                 val one : num        (*[ val one  <: pos   ]*)
               end
         ) = 
struct
    open Num

    (*[ x <: (pos ref) & (nat ref) ]*)
    val x = ref one
    val () = (x := zero)

    (*[ y <: pos ]*)
    val y = !x
end
