(* List Table *)
(* Author: Rowan Davies *)

functor ListTable
  (type key'
   val eq : key' * key' -> bool)
  (* :> TABLE where type key = key' *) =
struct
  type key = key'
  type 'a entry = key * 'a

  type 'a Table = 'a entry list ref

  fun new (n) = ref []


    (* From FinMapEq in the ML Kit.  Tidy this up. - Rowan *)
    fun lookup [] x  = NONE
      | lookup ((x,y)::rest) x' =
	if eq(x,x') then SOME(y) else lookup rest x'

    fun isin (x,[]) = false
      | isin (x,(x',y)::rest) = eq(x,x') orelse isin (x,rest)

    fun ins (x, y, nil) = [(x, y)]
      | ins (x', y', (p as (x, y)) :: rest) = 
	  if eq(x,x') then (x', y') :: rest
	  else p :: ins (x', y', rest)

    fun add (x, y, l) = 
        if isin (x,l) then ins (x,y,l)
        else (x,y)::l


  fun insertPersistant t (k, v) = ref (ins (k, v, !t))

  fun insert t (k, v) = (t := ins(k, v, !t))
  fun insertShadow t (k, v) = 
      let val res = (case lookup (!t) k of
			 SOME x => SOME (k, x)
		       | NONE => NONE)
	  val () = insert t (k, v)
      in
	  res
      end

  fun clear t = t := []
  fun copy t = ref (!t)
  fun app f t = List.app f (!t)
  fun lookup' t x = lookup (!t) x
  val lookup = lookup'

  fun plus (t1, t2) = ref (!t1 @ !t2)

end;  (* functor ListTable *)


