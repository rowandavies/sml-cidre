
(* This is an attempt to show the unsoundness of the current approach of treating opaque types as
   "all covariant" when all their parameters are covariant.  The attempt failed, and I was 
   reasonably convinced that the approach is sound, but I can't clearly say why.  Roughly, it's
   because for particular sorts S1 and S2, and sort constructor r, 
      R r <= (S1 r) & (S2 r)
   is satisfied exactly if 
        R r <= S1 r   and   R r <= S2 r 
   and this is true exactly if 
          R <= R1     and     R <= R2
   i.e.   R <= R1 & R2

   I guess this only shows that (S1 & S2) r is the greatest R r that is an 
   lower bound of S1 r and S2 r.  But, we might have 
           (S1 & S2) r < (S1 r) & (S2 r)

   To show (S1 r) & (S2 r) <= (S1 & S2) r is the hard part, not the above!
   This is somewhat strange, because the problems are likely to be with
   intersections of functions being strictly less that what can be obtained without
   intersections.

   Roughly, my argument is that expanding both sides, we get

        body_r(S1) & body_r(S2) <= body_r(S1 & S2)

   then, we can push the intersection inwards until we reach a variable, or an arrow.
   The first case is trivial.  In the second case, we have 
      (R1(S1) -> R2(S1)) & (R1(S2) -> R2(S2)) <= R1(S1 & S2) -> R2(S1 & S2)
   with R1 contravariant and R2 covariant.

   Now, R1(S1) <= R1(S1 & S2) and R1(S1) <= R1(S1 & S2) (by contravariance)

   Hmmm.  No time for this now.  Perhaps looking for a counterexample again is a reasonable
   step.  We want a situation where 
             f :> (R1(S1) -> R2(S1)) & (R1(S2) -> R2(S2))
   but not   f :> R1(S1 & S2) -> R2(S1 & S2)
   e.g.      f :> (S1 -> 1) -> 1  &  (S2 -> 1) -> 1
   but not   f :> ((S1 & S2) -> 1) -> 1

   any val f satisfying the first is equivalent to:
      fun f x = e
   with  x :> (S1 -> 1) |- e :> 1
   and   x :> (S2 -> 1) |- e :> 1
   but then, e must only apply x to expressions which can be assigned sort S1 and also
   assigned S2.  Thus with disributivity, we could conclude that in every case, the
   expression could be assigned S1 & S2, thus 
         x :> ((S1 & S2) -> 1) |- e :> 1

   Now, perhaps the lack of distributivity leads to a counter-example?
*)
   
   

structure S = 
struct 

(*
  datatype 'a r = R1 of 'a * bool ref | R2 of 'a * bool ref | R3

  (*[ datasort 'a r12 = R1 of 'a * bool ref | R2 of 'a * bool ref ]*)
  (*[ datasort 'a r13 = R1 of 'a * bool ref | R3 ]*)
*)


  datatype '+a t = T of ('+a -> '+a -> unit) -> '+a

  (*[ val f :> (('a -> 'a -> unit) -> 'a) -> 'a t ]*)
  fun f x = T x

  (*[ val g :> 'a t -> (('a -> 'a -> unit) -> 'a) ]*)
  fun g (T x) = x

  datatype '+a t2 = T2 of ('+a -> unit) -> unit
  datatype '+a t3 = T3 of unit -> '+a

end :>
  sig
     datatype '+a t = T of ('+a -> '+a -> unit) -> '+a

(*      type '+a t *)
     val f : (('a -> 'a -> unit) -> 'a) -> 'a t
     val g : 'a t -> (('a -> 'a -> unit) -> 'a)

     datatype '+a t2 = T2 of ('+a -> unit) -> unit

     datatype '+a t3 = T3 of unit -> '+a
  end

structure Client = 
struct 
  (*[ datasort tt = true and ff = false ]*)  

  (*[ val forF :> (tt -> tt -> unit) -> tt & (ff -> ff -> unit) -> ff ]*)
(*  fun forF s = s true

  val x = S.f forF *)

  (*[ val h :>  (tt ref -> tt ref -> unit) -> tt ref
              & (bool ref -> bool ref -> unit) -> bool ref  ]*)
  fun h g = (g (ref true) (ref true); ref true)

  (* Both of the first two succeed.  But not the second.  And, S.T h is a value, so it
     should succeed.  So, this example doesn't show unsoundness of the rule (yet), but
     but it rejects some examples that it should accept: it doesn't respect the
     intersection rule for values. 

     Thus, ultimately we should declare whether sort constructors are "all covariant"
     or not in signatures.  Which should be the default? *)

  (*[ val y1 :> (tt ref) S.t (* & (bool ref) S.t *) ]*)
  val y1 = (S.T (*[ :>  ((tt ref -> tt ref -> unit) -> tt ref) -> tt ref S.t
                     & ((bool ref -> bool ref -> unit) -> bool ref) -> bool ref S.t ]*)
          ) h

  (*[ val y2 :> (* (tt ref) S.t & *) (bool ref) S.t ]*)
  val y2 = (S.T (*[ :>  ((tt ref -> tt ref -> unit) -> tt ref) -> tt ref S.t
                     & ((bool ref -> bool ref -> unit) -> bool ref) -> bool ref S.t ]*)
          ) h


  (* This example shows that we can't instantiate the sort for S.T in the way that we'd like.
     The constructor should have the sort, since distributivity is valid for constructors.  *)
  (*(*[ val y25 :> (tt ref) S.t & (bool ref) S.t  ]*)
  val y25 = (S.T (*[ :>  ((tt ref -> tt ref -> unit) -> tt ref) -> tt ref S.t
                     & ((bool ref -> bool ref -> unit) -> bool ref) -> bool ref S.t 
                     & ( (   (tt ref -> tt ref -> unit) -> tt ref
		           & (bool ref -> bool ref -> unit) -> bool ref)
                          -> (tt ref S.t & bool ref S.t))
                  ]*)
          ) h
*)

  (* This works, but of course there's no way to intersect the two sorts.  *)
  (*[ val y3 :>  unit -> (tt ref) S.t 
              & unit -> (bool ref) S.t  ]*)
  fun y3 () = (S.T (*[ :>  ((tt ref -> tt ref -> unit) -> tt ref) -> tt ref S.t
                     & ((bool ref -> bool ref -> unit) -> bool ref) -> bool ref S.t ]*)
             ) h

  val y35 = (S.T3 (*[ :> (unit -> tt ref S.t & unit -> bool ref S.t) 
                         -> (tt ref S.t S.t3 & bool ref S.t S.t3) ]*))
                y3

  (*[ val h2 :>  (tt ref -> unit) -> unit
              & (bool ref -> unit) -> unit  ]*)
  fun h2 g = (g (ref true) )

(*
  val y4 = (S.T2 (*[ :> ( (tt ref -> unit) -> unit) -> tt ref S.t2 
                      & ( (bool ref -> unit) -> unit) -> bool ref S.t2
                      & ( (   (tt ref -> unit) -> unit
		            & (bool ref -> unit) -> unit)
                           -> (tt ref S.t2 & bool ref S.t2))
                   ]*)
           ) h2
*)


end