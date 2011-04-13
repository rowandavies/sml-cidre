
functor F (type t
           val x: t)  = 
  struct
    type s = unit
    (*[ val y :> s ]*) 
    val y = x 
  end