
functor F (structure A : sig type t end
           structure B : sig type t type s end          
             sharing A = B
             sharing type B.t = B.s
           val x: A.t)  = 
  struct
    (*[ val y :> B.s ]*) 
    val y = x 
  end