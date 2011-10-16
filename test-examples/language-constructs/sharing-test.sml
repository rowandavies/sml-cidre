
functor F (structure A : sig type t end
           structure B : sig type s end
             sharing type A.t = B.s
           val x: A.t)  = 
  struct
    (*[ val y :> B.s ]*) 
    val y = x 
  end