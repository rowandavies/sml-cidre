(*$ListHacks: LIST_HACKS*)
functor ListHacks(): LIST_HACKS =
  struct
    fun member x s = List.exists (fn y => x=y) s
    fun union(set1, set2) =
      set1 @ List.filter (fn x => not(member x set1)) set2

    fun unionEq eq (set1, set2) =
      set1 @ List.filter (fn y => not (List.exists (fn x => eq (x,y)) set1) ) set2

    fun intersect(set1, set2) =
      List.filter (fn x => member x set1) set2

    fun minus(set1, set2) =
      List.filter (fn x => not(member x set2)) set1

    fun eqSet(set1, set2) =
      case (minus(set1, set2), minus(set2, set1))
	of (nil, nil) => true
	 | _ => false

    local
      fun rev [] x = x
	| rev (h::t) x = rev t (h::x)
    in
      fun reverse l = rev l []
    end

    (* Add an element to a list of maximal elements wrt partial order leq *)
    fun addMax leq (x1, xs2) =  
      if (List.exists (fn x2 => leq(x1, x2)) xs2) then 
	xs2
      else 
	x1::(List.filter (fn x2 => not (leq(x2, x1))) xs2)

    fun flatten l = List.foldl (fn (x,acc) => x @ acc) [] l

    fun removeLast [] = raise List.Empty
      | removeLast [x] = (x, [])
      | removeLast (h::t) = let val (x, t2) = removeLast t
                            in (x, h::t2)
                            end

    fun foldl' f [] = raise List.Empty
      | foldl' f (h::t) = List.foldl f h t

    fun foldr' f [] = raise List.Empty
      | foldr' f l  =
        let val (last, front) = removeLast l
        in List.foldr f last front
        end

    fun stringSep first last sep toString l = 
        case l of [] => first ^ last
                | _ => first ^ (foldl' (fn (a, b) => b ^ sep ^ a) (map toString l)) ^ last

  end;
