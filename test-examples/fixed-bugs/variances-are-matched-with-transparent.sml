
structure SS = 
struct
   datatype '+a mixed = MM of '+a | MM2 of '+a
    (*[ datasort '+a mixed3 = MM2 of '+a ]*)
    (*[ datasort '+a mixed2 = MM of '+a ]*)
    (*[ sortdef 'a mixed4 = 'a mixed2 
        val f :> 'a mixed4 -> 'a mixed4 ]*)
   fun f x = x
end :>
sig 
(*  type 'a mixed 
 (*[  sortdef 'a mixed2 <| mixed 
      sortdef 'a mixed3 <| mixed 
      sortdef 'a mixed4 <| mixed2 & mixed3  ]*) 
*)      
  datatype 'a mixed  = MM of 'a | MM2 of 'a
    (*[ datasort '+a mixed2 = MM of '+a ]*)
    (*[ datasort '+a mixed3 = MM2 of '+a ]*)
    (*[ datasort '+a mixed4 = MM of '+a ]*)

    (*[ val f :> 'a mixed2 -> 'a mixed ]*)
    val f : 'a mixed -> 'a mixed
end