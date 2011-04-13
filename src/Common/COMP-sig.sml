signature COMP = 
sig
  type Error       (* Errors to be reported *)
  exception Fail   (* Raised when not reporting errors. *)

  type 'a Result = 'a * Error list   (* Results of computations. *)

  (* Computations, which return errors or raise Fail on failure. *)
  (* When the bool is true errors should be returned. *)
  type 'a Comp = bool -> 'a Result

  (* Results that can be "backtracked".  (Errors are never returned during a "redo") *)
  datatype 'a Redo = REDO of 'a * ('a Redo) Comp 
  (* type 'a Redo *)  (* This type could be kept opaque - it is currently not used. *) 

  (* Computations with backtracking. *)
  type 'a RComp = 'a Redo Comp

  datatype 'a Memo =  (* Exposing this type makes its purpose clear. *) 
     NOT_MEMOED
   | RETURNED of 'a Result
   | FAILED

  (* val newMemoCell : unit -> 'a Memo ref *)
  val newMemoCell : unit -> 'a Memo ref


  (* ********** 'a Comp COMBINATORS  ************* *)
  val failV : 'a * Error list -> 'a Comp
  val failC : 'a Comp * Error list -> 'a Comp

  val noErr : 'a -> 'a * Error list
  val noErrC : 'a -> 'a Comp
  val adderrs : 'a Result * Error list -> 'a Result

  (* Memoization. First checks whether memo contains a result. *)
  val memoIn : 'a Memo ref -> 'a Comp -> 'a Comp

  (* "letC" sequences two computations, passing the result of the first
     to the second.  *)
  val letC : 'a Comp -> ('a -> 'b Comp) -> 'b Comp

  (* Sequence a computation with a valuable expression, 
     i.e. apply an ML function to the result of a computation *)
  val letCV : 'a Comp -> ('a -> 'b) -> 'b Comp

  (* Sequenced "or".  Returns first res and errs if both fail.  *)
  val tryBothC1 : 'a Comp -> 'a Comp -> 'a Comp

  (* Sequenced "or".  Returns second res and errs if both fail.  *)
  val tryBothC2 : 'a Comp -> 'a Comp -> 'a Comp

  val mapC : ('a -> 'b Comp) -> 'a list -> 'b list Comp

  (* ********** 'a RComp COMBINATORS ************* *)

  (* Memoize an RComp, including the redo continutations. *)
  val memoInR : 'a Redo Memo ref -> 'a RComp -> 'a RComp

  (* "noRedo" constructs an 'a Redo result with no backtracking. *)
  val noRedo : 'a -> 'a Redo

  (* deconstruct a redo result into a result and a computation for backtracking. *)
  val redoToResAndRC : 'a Redo -> 'a * 'a RComp

  (* Sequenced "or".  Returns second res and errs if both fail.  *)
  val tryBothR1 : 'a RComp -> 'a RComp -> 'a RComp

  (* Sequenced "or".  Returns first res and errs if both fail.  *)
  val tryBothR2 : 'a RComp -> 'a RComp -> 'a RComp

  (* "letR" sequences two redo computations, passing the result of the
     first to the second.   *)
  val letR : 'a RComp -> ('a -> 'b RComp) -> 'b RComp

  (* "letRC" sequences a redo computation rc1 and a computation c2.  It performs
     backtracking on rc1 when c2 fails.  If the rc1 succeeds, but c2 fails
     every time, the errors from the first call to c2 are reported.  *)
  val letRC : 'a RComp -> ('a -> 'b Comp) -> 'b Comp

  (* sequence an RComp and a value, i.e apply a function to the each result of an RComp *)
  val letRV : 'a RComp -> ('a -> 'b) -> 'b RComp

  (* backtrack to list all results (always at least one) *)
  val redo_to_list : 'a Redo -> 'a list

  (* Backtrack over a list of computations *)
  val tryEach : 'a RComp list -> 'a RComp

  (* Backtrack over a non-empty list of values *)  
  val tryEachV : 'a list -> 'a RComp


end (* of signature COMP *)