
functor F (type t
           val x: t)  = 
  struct
    type s = t
    (*[ val y :> s ]*) 
    val y = x 
  end