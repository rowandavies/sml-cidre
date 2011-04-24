

(* A library for "computations" with failure and "redoable computations".  *)

functor Comp(structure S : sig type Error end) : COMP where type Error (* in COMP *) 
                                          = S.Error (* functor parameter *)
= struct

  type Error = S.Error

  exception Fail (* Raised when not reporting errors. *)


  (* Results of computations. *)
  type 'a Result = 'a * Error list

  (* Computations, which may fail. *)
  (* When the bool is true errors should be reported, otherwise failures should raise Fail. *)
  type 'a Comp = bool -> 'a Result

  (* val failV : 'a * Error list -> 'a Comp *) 
  fun failV (default, msgs) true = 
      (default, msgs)
    | failV _ false = raise Fail

  (* val failC : 'a Comp * Error list -> 'a Comp *) 
  fun failC (default, msgs) true = 
      let val (res, msgs2) = default true
      in (res, msgs2 @ msgs)
      end
    | failC _ false = raise Fail

  (* val noErr : 'a -> 'a * Error list *)
  fun noErr x = (x, [])

  (* val noErrC : 'a -> 'a Comp *)
  fun noErrC x errflag = (x, [])

  (* Results that can be "backtracked".  (Errors are never reported during a "redo") *)
  datatype 'a Redo = REDO of 'a * ('a Redo) Comp

  (* Computations with backtracking. *)
  type 'a RComp = 'a Redo Comp

  (* failRedo : 'a RComp *)
  fun failRedo false = raise Fail  (* No error reporting during redo *)

  datatype 'a Memo = 
     NOT_MEMOED
   | RETURNED of 'a * Error list
   | FAILED

  (* val newMemo : unit -> 'a Memo ref *)
  fun newMemoCell () = ref NOT_MEMOED


  (* ********** 'a Comp COMBINATORS  ************* *)

  (* val memoIn : 'a Memo ref -> 'a Comp -> 'a Comp *)
  fun memoIn r c errflag =
	case (errflag, !r)
	  of (false, FAILED) => ((*print "HIT FAILED\n";*) raise Fail (* c errflag *))
	   | (false, RETURNED (_, err::errs)) => raise Fail (* c errflag *)
           | (_, RETURNED memoed) => ((*print "HIT RETURNED\n";*)
				      memoed (*; c errflag *))
	   | _ =>              (* NOT_MEMOED or FAILED and errflag is true *)
	     let val res = (c errflag)
			   handle Fail => (r := FAILED; raise Fail)
		 val _ = (r := RETURNED res)
	     in  
		 res
	     end



  fun adderrs ((res, errs1), errs2) = (res, errs1 @ errs2)
  fun errsOf (_, errs1) = errs1
  fun useErrs1 (_, errs1) res = (errs1, res)
  fun extendErrs (res, errs) f = (f res, errs)

  (* "letC" sequences two computations, passing the result of the first
     to the second.  *)
  (* letC : 'a Comp -> ('a -> 'b Comp) -> 'b Comp) *)
  fun letC c1 c2 false = c2 (#1 (c1 false)) false   (* minor optimization *)
    | letC c1 c2 true = 
      (case c1 true of
	   (res, errs) => adderrs (c2 res true, errs))

  (* Sequence a computation with a valuable expression, 
     i.e. apply an ML function to the result of a computation *)
  (* letCV : 'a Comp * ('a -> 'b) -> 'b Comp *)
  fun letCV (c1: 'a Comp) (v2: 'a -> 'b) : 'b Comp = fn (errflag : bool) =>
      (case c1 errflag of
	   (res, errs) => (v2 res, errs))

  (* "ifthenelseC f1 f2 f3 errflag" tries f1, and passes the result to f2
     on success.  If f1 fails and errors are being reported, then it
     tries f3 and combines error messages. *)
  (* ifthenelseC : 'a Comp -> ('a -> 'b Comp) -> ('a -> 'b Comp) -> 'b Comp) *)
  fun ifthenelseC f1 f2 f3 false = f2 (#1 (f1 false)) false   (* minor optimization *)
    | ifthenelseC f1 f2 f3 true = 
      (case f1 true of
	  (res, []) => f2 res true
	| (res, errs) => adderrs (f3 res true, errs) )

  (* Sequenced "or".  Returns first res and errs if both fail.  *)
  fun tryBothC1 f1 f2 false = (f1 false  handle Fail => f2 false)
    | tryBothC1 f1 f2 true =
       (case (f1 true) of 
	  res as (_, []) => res
        | res => f2 false  handle Fail => res)

  (* Sequenced "or".  Returns second res and errs if both fail.  *)
  fun tryBothC2 f1 f2 errflag = 
    (f1 false)
    handle Fail => f2 errflag


  fun mapC fC [] = noErrC []
    | mapC fC (h::t) = letC (fC h) (fn hres => letCV (mapC fC t) (fn tres => hres::tres))

  (* ********** 'a RComp COMBINATORS ************* *)

  (* Memoize an RComp, including the redo continutations. *)
  (* val memoInR : 'a Redo Memo ref -> 'a RComp -> 'a RComp *)
  fun memoInR cell rc errflag = 
    memoIn cell (fn errflag2 => case rc errflag2 
                                  of (REDO (res, redo), errs) => 
 	                             (REDO (res, memoInR (ref NOT_MEMOED) redo), errs))
           errflag

  (* "noRedo" constructs an 'a Redo result with no backtracking. *)
  (* val noRedo : 'a -> 'a Redo *)
  fun noRedo x = REDO (x, failRedo)

  (* val redoToResAndRC : 'a Redo -> 'a * 'aRComp *)
  fun redoToResAndRC (REDO (res, rc)) = (res, rc)

  (* addRedo : 'a RComp -> 'a RComp -> 'a RComp *)
  fun addRedo rc1 rc2 errflag = 
    letCV rc1 (fn REDO (res1, redo1) =>
    REDO (res1, tryBothR2 redo1 rc2)  ) errflag

  (* Sequenced "or".  Returns second res and errs if both fail.  *)
  and tryBothR2 rc1 rc2 errflag =
    tryBothC2 (addRedo rc1 rc2) rc2 errflag

  (* Sequenced "or".  Returns first res and errs if both fail.  *)
  fun tryBothR1 rc1 rc2 errflag =
    tryBothC1 (addRedo rc1 rc2) rc2 errflag

  (* "letR" sequences two redo computations, passing the result of the
     first to the second.   *)
  (* letR : 'a RComp -> ('a -> 'b RComp) -> 'b RComp *)
(* This version was in use for a long time. - Rowan *)
  fun letR rc1 rc2 errflag = 
     letC rc1 (fn REDO (res1, redo1) =>
     tryBothR1 (rc2 res1) (letR redo1 rc2)  ) errflag

(* This version might be slightly more efficient. *)
(*  fun letR rc1 rc2 errflag = 
       ifthenelseC rc1 
		   (fn REDO (res1, redo1) =>
		       tryBothR1 (rc2 res1) (letR redo1 rc2)  )
		   (fn REDO (res1, redo1) => (* failed *)
                       rc2 res1 )  errflag
*)

(* This version might be even faster.  In practice the difference may be small.  *)
(*  fun letR rc1 rc2 false = 
       case rc1 false of REDO ((res, _), _) =>
			 tryBothR1 (rc2, res1) (letR redo1 rc2)
    | letR rc1 rc2 true = 
       (case rc1 true of
	    REDO ((res1, []), redo1) => tryBothR1 (rc2, res1) (letR redo1 rc2) true
	  | REDO ((res1, errs1), redo1)=> adderrs (rc2 res1 true, errs1))
*)       

  (* "letRC" sequences a redo computation rc1 and a computation c2.  It performs
     backtracking on rc1 when c2 fails.  If the rc1 succeeds, but c2 fails
     every time, the errors from the first call to c2 are reported.  *)
  (* letRC : 'a RComp -> ('a -> 'b Comp) -> 'b Comp *)   
  fun letRC rc1 c2 errflag =      
     letC rc1 (fn REDO (res, redo) =>
     tryBothC1 (c2 res) (letRC redo c2)  ) errflag

  (* sequence an RComp and a value, i.e apply a function to the each result of an RComp *)
  (* letRV : 'a RComp -> ('a -> 'b) -> 'b RComp *)
  fun letRV rc1 f2 errflag =
     letCV rc1 (fn REDO (res1, redo1) =>
     REDO (f2 res1, letRV redo1 f2)  ) errflag

  fun redo_to_list (REDO (res, redo)) = 
        (res :: redo_to_list (#1 (redo false)))
        handle Fail => [res]

  (* val tryEach : 'a RComp list -> 'a RComp *)
  fun tryEach [h] = h
    | tryEach (h::t) = tryBothR2 h (tryEach t)

  (* val tryEachV : 'a list -> 'a RComp *)
  fun tryEachV vs = tryEach (map (noErrC o noRedo) vs)

end (* of functor Comp *)
