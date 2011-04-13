

(* val f :> 'a -> 'a ]*)
fun 'a f x = 
  let  (*[ val y :> 'a -> 'a ]*) 
    val y = x 
  in y end

datatype a = B
(*[ datasort b = B ]*)