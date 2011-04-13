(* Algorithms on regular grammars *)
(* Loosely follows Dart & Zobel'91 *)
(* Author: Frank Pfenning *)
(* Sep 2003 *) 
(* Warning: the subset alg is unsound *)
(* See Lu and Cleary'98 *)

datatype sterm = Var of string | Const of string * sterm list
(*[
datasort var = Var of string
datasort con = Const of string * sterm list
]*)

(*[
val stermToString : sterm -> string
and stermsToString : sterm list -> string
]*)
fun stermToString (Var(a)) = a
  | stermToString (Const(f,nil)) = f
  | stermToString (Const(f,ts)) = f ^ "(" ^ stermsToString ts ^ ")"
and stermsToString (nil) = "_"
  | stermsToString (t::nil) = stermToString t
  | stermsToString (t::ts) = stermToString t ^ "," ^ stermsToString ts

datatype rule = Rule of sterm * sterm list
(*[
datasort trule = Rule of var * sterm list  (* result of intersect *)
datasort crule = Rule of var * con list	   (* input to subset *)
]*)

(*[
val expand : var * crule list -> con list
           & var * trule list -> sterm list
]*)
fun expand (Var(a), Rule (Var(b), alts)::rs) =
    if a = b then alts
    else expand (Var(a), rs)
  | expand (Var(a), nil) = raise Match

(*[
val expands : sterm list * crule list -> con list
]*)
fun expands (Var(a)::ss, rs) = expand (Var(a), rs) @ expands (ss, rs)
  | expands (Const(f, ts)::ss, rs) =
      Const(f, ts)::expands (ss, rs)
  | expands (nil, rs) = nil

(*--------------------------------------------------*)
(* Emptiness Test                                   *)
(*--------------------------------------------------*)

(*[
val member : var * var list -> bool
]*)
fun member (Var(a), nil) = false
  | member (Var(a), Var(b)::bs) =
      (a = b) orelse member (Var(a), bs)

(*[
val empty' : trule list * sterm * var list -> bool
and emptys : trule list * sterm list * var list -> bool
and empty1 : trule list * sterm list * var list -> bool
]*)
fun empty' (T, Var(a), E) =
      member(Var(a), E)
      orelse emptys (T, expand (Var(a), T), Var(a)::E)
  | empty' (T, Const (f, ts), E) =
      empty1 (T, ts, E)
and emptys (T, nil, E) = true
  | emptys (T, t::alts, E) =
      empty' (T, t, E) andalso emptys (T, alts, E)
and empty1 (T, nil, E) = false
  | empty1 (T, t::ts, E) =
      empty' (T, t, E) orelse empty1 (T, ts, E);

(*[
val empty : trule list * sterm -> bool
]*)
fun empty (T, t) = empty' (T, t, nil);

(*--------------------------------------------------*)
(* Subset Test                                      *)
(*--------------------------------------------------*)

(*[
val consl : sterm list * sterm list -> sterm list list
]*)
fun consl (nil, ss) = nil
  | consl (t::ts, ss) = (t::ss)::consl(ts, ss)

(*[
val openselect : crule list * con * sterm list list -> sterm list list
]*)
fun openselect (T, Const(f, us), (Const(g, ts)::ss)::sss) =
    if f = g then (ts @ ss)::openselect (T, Const(f, us), sss)
    else openselect (T, Const(f, us), sss)
  | openselect (T, Const(f, us), (Var(a)::ss)::sss) =
      openselect (T, Const(f, us), consl (expand (Var(a), T), ss) @ sss)
  | openselect (T, Const(f, us), nil) = nil
  | openselect (T, Const(f, us), nil::sss) = raise Match (* fix! *)

(*[
val eq : sterm * sterm -> bool
and eqs : sterm list * sterm list -> bool
]*)
fun eq (Var(a), Var(b)) = (a = b)
  | eq (Const(f, ts), Const(g, ss)) =
      (f = g) andalso eqs (ts, ss)
  | eq (_, _) = false
and eqs (nil, nil) = true
  | eqs (t::ts, s::ss) = eq (t, s) andalso eqs (ts, ss)
  | eqs _ = raise Match

(*[
val mem : sterm * sterm list -> bool
]*)
fun mem (t, s::ss) = eq (t, s) orelse mem (t, ss)
  | mem (t, nil) = false

(*[
val subset : sterm list * sterm list -> bool
]*)
fun subset (nil, ss') = true
  | subset (s::ss, ss') =
      mem (s, ss') andalso subset (ss, ss')

(*[
sortdef memo = (var * sterm list) list
val lookup : var * memo -> sterm list option
]*)
fun lookup (Var(a), nil) = NONE
  | lookup (Var(a), (Var(b), ss)::C) =
    if a = b then SOME (ss)
    else lookup (Var(a), C)

(*[
val heads : sterm list list -> sterm list
]*)
fun heads (nil) = nil
  | heads ((t::ts)::sss) = t::heads sss
  | heads _ = raise Match

(*[
val tails : sterm list list -> sterm list list
]*)
fun tails (nil) = nil
  | tails ((t::ts)::sss) = ts::tails sss
  | tails _ = raise Match

(*[
val subs : crule list * sterm list * sterm list * sterm list list * memo -> bool
val subseq : crule list * sterm list * sterm list list * memo -> bool
]*)
fun subs (T, nil, ts, sss, C) = true
  | subs (T, t::alts, ts, sss, C) =
      subseq (T, t::ts, sss, C) andalso subs (T, alts, ts, sss, C)
and subseq (T, ts, nil, C) = (* false *) (* bug in Dart & Zobel? *)
      List.exists (fn t => empty (T, t)) ts
  | subseq (T, nil, sss, C) = true
  | subseq (T, Var(a)::ts, sss, C) =
    (case lookup (Var(a), C)
       of NONE => subs (T, expand (Var(a), T), ts, sss, (Var(a), heads(sss))::C)
        | SOME(ss') => if subset (heads(sss), ss')
	               then subseq (T, ts, tails(sss), C)
                       else subs (T, expand (Var(a), T), ts, sss, (Var(a), heads(sss))::C))
  | subseq (T, Const(f, args)::ts, sss, C) =
       subseq (T, args @ ts, openselect (T, Const(f, ts), sss), C)

(*[ val subtype : crule list * sterm * sterm -> bool ]*)
fun subtype (T, t, s) = subseq (T, [t], [[s]], nil)

(*--------------------------------------------------*)
(* Intersection                                     *)
(*--------------------------------------------------*)

(*[
sortdef intermemo = (sterm * sterm * var) list
val lookup2 : sterm * sterm * intermemo -> var option
]*)
fun lookup2 (t1, t2, nil) = NONE
  | lookup2 (t1, t2, (s1, s2, Var(a))::I) =
    if eq (t1, s1) andalso eq (t2, s2)
      then SOME(Var(a))
    else lookup2 (t1, t2, I)

local
  val counter = ref 0
in

fun reset () = (counter := 0)

(*[ val newvar : unit -> var ]*)
fun newvar () = (counter := !counter+1; Var (Int.toString (!counter)))

end

(*[
val Bottom : var
val BottomRule : crule
]*)
val Bottom = Var "Bottom"
val BottomRule = Rule (Bottom, [])

(*[
val intersectv : trule list * sterm * sterm * intermemo -> sterm * trule list
and intersectv' : trule list * sterm * sterm * intermemo -> sterm * trule list
and intersectvs : trule list * sterm list * sterm list * intermemo
                  -> sterm list * trule list
and cpi : trule list * sterm list * sterm list * sterm list * intermemo
          -> sterm list * trule list
and cpi' : trule list * sterm * sterm list * sterm list * sterm list
             * sterm list * intermemo
           -> sterm list * trule list
]*) 
fun intersectv (T, t1, t2, I) =
    if eq (t1, t2) then (t1, T)
    else (case lookup2 (t1, t2, I)
            of NONE => intersectv' (T, t1, t2, I)
	     | SOME(Var(a)) => (Var(a), T))
and intersectv' (T, Var(a), Var(b), I) =
    let
      val newa = newvar()
      val (alts, T) = cpi (T, expand (Var(a), T), expand (Var(b), T), nil,
			   (Var(a), Var(b), newa)::I)
    in
      (newa, Rule(newa, alts)::T)
    end
  | intersectv' (T, Var(a), t2, I) =
    let
      val newa = newvar()
      val (alts, T) = cpi (T, expand (Var(a), T), [t2], nil,
			   (Var(a), t2, newa)::I)
    in
      (newa, Rule(newa, alts)::T)
    end
  | intersectv' (T, t1, Var(b), I) =
    let
      val newa = newvar ()
      val (alts, T) = cpi (T, [t1], expand (Var(b), T), nil,
			   (t1, Var(b), newa)::I)
    in
      (newa, Rule(newa, alts)::T)
    end
  | intersectv' (T, Const(f, ts), Const(g, ss), I) =
    if f = g
      then let
	     val (us, T) = intersectvs (T, ts, ss, I)
	   in 
	     (Const(f, us), T)
	   end
    else (Bottom, T)
and intersectvs (T, nil, nil, I) = (nil, T)
  | intersectvs (T, t::ts, s::ss, I) =
    let
      val (u, T) = intersectv (T, t, s, I)
      val (us, T) = intersectvs (T, ts, ss, I)
    in
      (u::us, T)
    end
  | intersectvs _ = raise Match
and cpi (T, nil, ss, alts, I) = (alts, T)
  | cpi (T, t::ts, ss, alts, I) =
      cpi' (T, t, ts, ss, ss, alts, I)
and cpi' (T, t, ts, nil, ss, alts, I) =
      cpi (T, ts, ss, alts, I)
  | cpi' (T, t, ts, s::ss', ss, alts, I) =
    let
      val (v, T) = intersectv (T, t, s, I)
      (* optimize here if v = Bottom *)
    in
      cpi' (T, t, ts, ss', ss, v::alts, I)
    end

(*[ val intersect : trule list * sterm * sterm -> sterm * trule list ]*)
fun intersect (T, t1, t2) = intersectv (BottomRule::T, t1, t2, nil)

(*--------------------------------------------------*)
(* Natural Number Refinements                       *) 
(*--------------------------------------------------*)

(*[ val nat : var and even : var and odd : var and pos : var 
    and zero : var and bot' : var and bot : var ]*)
val nat = Var "nat"
val even = Var "even";
val odd = Var "odd";
val pos = Var "pos";
val zero = Var "zero";
val bot' = Var "bot'";
val bot = Var "bot";

(*[ val z : con and s : sterm -> con ]*)
val z = Const ("z", []);
fun s(t) = Const ("s", [t]);

(*[ val rnat : crule and reven : crule and rodd : crule and rpos : crule
    and rbot : crule and rbot' : crule and rzero : crule ]*)
val rnat = Rule (nat, [z, s(nat)]);
val reven = Rule (even, [z, s(odd)]);
val rodd = Rule (odd, [s(even)]);
val rpos = Rule (pos, [s(nat)]);
val rzero = Rule (zero, [z])
val rbot' = Rule (bot', [s(bot)]);
val rbot = Rule (bot, []);

(*[ val Tnat : crule list ]*)
val Tnat = [rzero,rnat,reven,rodd,rpos,rzero,rbot',rbot];

val (* true *) t0 = empty (Tnat, bot);
val (* true *) t1 = empty (Tnat, bot');

val (* true *) t2 = subtype (Tnat, nat, nat);
val (* true *) t3 = subtype (Tnat, odd, pos);
val (* false *) f4 = subtype (Tnat, even, pos);

val (* true *) t5 = subtype (Tnat, bot', zero); (* bug in Dart & Zobel? *)
val (* true *) t6 = subtype (Tnat, bot, zero);

val (evenodd, Tnat') = intersect (Tnat, even, odd);
val (* true *) t7 = empty (Tnat', evenodd);
val (poseven, Tnat') = intersect (Tnat, even, pos);

(*[ val evenpos : var
    val revenpos : crule
]*)
val evenpos = Var "evenpos";
val revenpos = Rule (evenpos, [s(odd)]);
(* next two calls are ill-sorted, because intersect returns
   a trule list, while subtype expects a crule list
*)

(*
val (* true *) t8 = subtype (revenpos::Tnat', evenpos, poseven)
val (* true *) t9 = subtype (revenpos::Tnat', poseven, evenpos)
*)

(*--------------------------------------------------*)
(* Rejection of Tuple Distributivity                *)
(*--------------------------------------------------*)

(*[ val a : con and b : con and c : con
    and f : sterm * sterm -> con ]*)
val a = Const ("a", []);
val b = Const ("b", []);
val c = Const ("c", []);
val d = Const ("d", []);
fun f(s,t) = Const ("f", [s,t]);

(*[ val ac : var and bd : var
    and small : var and large : var ]*)
val ac = Var "ac";
val bd = Var "bd";
val small = Var "small";
val large = Var "large";

(*[ val r1 : crule and r2 : crule and r3 : crule
    and r4 : crule ]*)
val r1 = Rule (ac, [a,c]);
val r2 = Rule (bd, [b,d]);
val r3 = Rule (small, [f(a,b), f(c,d)]);
val r4 = Rule (large, [f(ac, bd)]);

val Ttest = [r1,r2,r3,r4]

val (* true *) tt1 = subtype (Ttest, small, small);
val (* true *) tt2 = subtype (Ttest, small, large);
val (* false *) ff3 = subtype (Ttest, large, small);
val (* true *) tt4 = subtype (Ttest, large, large);
