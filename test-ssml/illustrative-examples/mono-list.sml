
(* This is incomplete, and I think won't work the way I had hoped. *)

    functor mono_list (type elem) = 
    struct
      (*[ assumesig 
	sig
          datatype mlist = nnil | ::: of elem * mlist
          (* type mlist *)
	  (* datatype mlist = nil | :: of elem * mlist *)
	  exception Empty
	  val null : mlist -> bool 
	  val length : mlist -> int 
	  val @ : (mlist * mlist) -> mlist 
	  val hd : mlist -> elem
	  val tl : mlist -> mlist 
	  val last : mlist -> elem
	  val getItem : mlist -> (elem * mlist) option
	  val nth : (mlist * int) -> elem
	  val take : (mlist * int) -> mlist 
	  val drop : (mlist * int) -> mlist 
	  val rev : mlist -> mlist 
(* 	  val concat : mlist oplist -> mlist  *)
	  val revAppend : (mlist * mlist) -> mlist 
	  val app : (elem -> unit) -> mlist -> unit 
	  val map : (elem -> elem) -> mlist -> mlist
	  val mapPartial : (elem -> elem option) -> mlist -> mlist 
	  val find : (elem -> bool) -> mlist -> elem option 
	  val filter : (elem -> bool) -> mlist -> mlist 
	  val partition : (elem -> bool) -> mlist -> (mlist * mlist) 
	  val foldl : ((elem * 'b) -> 'b) -> 'b -> mlist -> 'b
	  val foldr : ((elem * 'b) -> 'b) -> 'b -> mlist -> 'b
	  val exists : (elem -> bool) -> mlist -> bool 
	  val all : (elem -> bool) -> mlist -> bool 
	  val tabulate : (int * (int -> elem)) -> mlist 
	end ]*)
    end	

    datatype 'a operator =
        Atom of 'a
      | Infix of ('a * 'a -> 'a)
      | Prefix of ('a -> 'a)
      | Postfix of ('a -> 'a)

    structure oplist = mono_list(type elem = unit operator)



