signature S = 
sig
  type t
  type t1 = t * t
  structure s3 : sig
      type t = t
      type t2 = t1
      (*[ sortdef r2 < t ]*)
  end
  val v : t
  val f : s3.t2 -> s3.t2
end



functor F(structure s:S
          structure s2:S where type s3.t = s.s3.t
          sharing s=s2 ) :> sig type t2 
				val result : t2 end = 
struct
   type t2 = s.s3.t2
   (* (*[ val result :> s2.s3.r2 ]*) *)
   val result = s2.f (s.v, s2.v)
end