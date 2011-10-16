
(*General.sml*)

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

    type unit = unit
    type exn = exn
    type 'a ref = 'a ref

(*
    exception Bind = Bind
    exception Match = Match
    exception Subscript
    exception Size
    exception Overflow = Overflow
    exception Domain
    exception Div = Div
    exception Chr
    exception Fail of string 
*)
(*  
    fun exnName (e: exn) : string = prim("exnName", "exnNameProfiling", e)   (* exomorphic by copying *)
    fun exnMessage (e: exn) : string = exnName e 
*)
    datatype 'a option = NONE | SOME of 'a
(*    exception Option
    fun getOpt (NONE, a) = a
      | getOpt (SOME a, b) = a
    fun isSome NONE = false
      | isSome _ = true
    fun valOf (SOME a) = a
(*       | valOf _ = raise Option *)
*)
    datatype order = LESS | EQUAL | GREATER

    fun !(x: 'a ref): 'a = ! x
    fun (x: 'a ref) := (y: 'a): unit = (x := y)
    fun (f o g) x = f(g x)
    fun a before () = a
    fun ignore (a) = ()


(* Top-level identifiers; Some are here - some are introduced later *)

fun op = (x: ''a, y: ''a): bool =  (x = y)

fun not true = false
  | not false = true

fun a <> b = not (a = b)

fun print (s:string) : unit = ()

fun implode (chars : char list) : string = ""
fun concat (ss : string list) : string = ""
fun (s : string) ^ (s' : string) : string = ""
fun str (c : char) : string = ""
fun size (s:string): int = 0
fun explode (s:string) : char list = explode s 





(* Twelf             Copyright (c) 1997 by Carnegie Mellon University *)
(* Author: Frank Pfenning      <fp+@cs.cmu.edu>                       *)
(*         Carsten Schuermann  <carsten+@cs.cmu.edu>                  *)

datatype 'a option = NONE | SOME of 'a;

(* fake arrays for sort checking *)
type 'a array = 'a list;
fun array (size, entry) = [entry];
fun update (a, i, entry) = entry::a;
fun sub (x::a, i) = x;

fun intToString (n:int) = "an integer";

(* Internal syntax                                                    *)

(*
functor IntSyn (structure Global : GLOBAL) : INTSYN =
struct
*)

  type cid = int			(* Constant identifier        *)
  type name = string			(* Variable name              *)
    
  datatype Depend =                     (* Dependency information     *)
    No                                  (* P ::= No                   *)
  | Maybe                               (*     | Maybe                *)

  (* Expressions *)
  (* We use much more general datatypes than required, to avoid a lot of code
     duplication for basic operations such as substitution.  We use
     refinements to restrict to LF expressions.  *)

  datatype Uni =			(* Universes:                 *)
    Kind				(* L ::= Kind                 *)
  | Type				(*     | Type                 *)

(*[
  datasort UniKind = Kind
  datasort UniType = Type
]*)

  datatype Exp =			(* Expressions:               *)
    Uni   of Uni			(* U ::= L                    *)
  | Pi    of (Dec * Depend) * Exp       (*     | Pi (D, P). V         *)
  | Root  of Con * Spine		(*     | C @ S                *)
  | Redex of Exp * Spine		(*     | U @ S                *)
  | Lam   of Dec * Exp			(*     | lam D. U             *)
  | EVar  of Exp option ref * Exp	(*     | X<I> : V             *)
  | EClo  of Exp * Sub			(*     | U[s]                 *)
    
  and Con =				(* Constructors:              *)
    BVar  of int			(* C ::= k                    *)
  | Const of cid			(*     | c                    *)
  | Defn  of cid			(*     | d                    *)
  | FVar  of name * Exp * Sub		(*     | F[s]                 *)
    
  and Spine =				(* Spines:                    *)
    Nil					(* S ::= Nil                  *)
  | App   of Exp * Spine		(*     | U ; S                *)
  | SClo  of Spine * Sub		(*     | S[s]                 *)

  and Sub =				(* Explicit substitutions:    *)
    Shift of int			(* s ::= ^n                   *)
  | Dot   of Head * Sub                 (*     | H.s                  *)

  and Head =				(* Heads:                     *)
    Idx of int				(* H ::= k                    *)
  | Exp of Exp * Exp			(*     | (U:V)                *)

  and Dec =				(* Declarations:              *)
    Dec of name option * Exp		(* D ::= x:V                  *)
    
  and Ctx =				(* Contexts                   *)
    Null				(* G ::= .                    *)
  | Decl of Ctx * Dec			(*     | G, D                 *)

  (* The following refinements restrict the above to LF *)
(*[
  datasort ExpKind =
    Uni of UniType
  | Pi of (DecObj * Depend) * ExpKind
  and ExpFam =
    Pi of (DecObj * Depend) * ExpFam
  | Root of ConFam * SpineFam
  | Redex of ExpFam * SpineFam
  | Lam of DecObj * ExpFam
  | EVar of ExpFam option ref * ExpKind
  | EClo of ExpFam * SubObj
  and ExpObj =
    Root of ConObj * SpineObj
  | Redex of ExpObj * SpineObj
  | Lam of DecObj * ExpObj
  | EVar of ExpObj option ref * ExpFam
  | EClo of ExpObj * SubObj

  and ConFam =
    Const of cid
  | Defn of cid
  and ConObj =
    BVar of int
  | Const of cid
  | Defn of cid
  | FVar of name * ExpFam * SubObj

  and SpineFam =
    Nil
  | App of ExpObj * SpineFam
  | SClo of SpineFam * SubObj
  and SpineObj =
    Nil
  | App of ExpObj * SpineObj
  | SClo of SpineObj * SubObj
  
  and SubObj =
    Shift of int
  | Dot of HeadObj * SubObj
  and HeadObj =
    Idx of int
  | Exp of ExpObj * ExpFam
  and DecObj =
    Dec of name option * ExpFam

  and CtxFam =
    Null
  | Decl of CtxFam * DecObj

]*)

(*
  type root = Con * Spine               (* R = C @ S                  *)
  type eclo = Exp * Sub   		(* Us = U[s]                  *)

  (* The global LF signature *)

  exception Error of string             (* raised if out of space     *)
  type imp = int			(* number implicit arguments  *)

  datatype Entry =			(* Signature Entry            *)
    ConstDec of name * imp		(* a : K : kind  or           *)
                * Exp * Uni	        (* c : A : type               *)
  | ConstDef of name * imp		(* a = A : K : kind  or       *)
                * Exp * Exp * Uni	(* d = M : A : type           *)

  fun entryName (ConstDec (name, _, _, _)) = name
    | entryName (ConstDef (name, _, _, _, _)) = name

  local
    val maxCid = 1023 (* Global.maxCid *)
    val sgnArray = array (maxCid+1, ConstDec("", 0, Uni (Kind), Kind))
      : Entry array
    val sgnSize  = ref(0)

  in
    (* Invariants *)
    (* All definitions are strict in all their arguments *)
    (* Definitions are stored in weak head-normal form *)
    (* if Const(cid) is a term, then sgnArray(cid) = ConstDec _ *)
    (* if Defn(cid) is a term, then sgnArray(cid) = ConstDef _ *)

    fun sgnReset () = (sgnSize := 0)
    fun sgnsize () = (!sgnSize)

    fun sgnAdd (entry) = 
        let
	  val cid = !sgnSize
	in
	  if cid > maxCid
	    then raise Error ("Global signature size "
			      ^ intToString (maxCid+1)
			      ^ " exceeded")
	  else (update (sgnArray, cid, entry) ;
		sgnSize := cid + 1;
		cid)
	end

    (* 0 <= cid < !sgnSize *)
    fun sgnLookup (cid) = sub (sgnArray, cid)
  end


  val sgnSize = sgnsize

  fun constDefn (d) =
      (case sgnLookup (d)
	 of ConstDef(_, _, U,_, _) => U)

  fun constType (c) =
      (case sgnLookup (c)
	 of ConstDec (_, _, V, _) => V
          | ConstDef (_, _, _, V, _) => V)

  fun constImp (c) =
      (case sgnLookup(c)
	 of ConstDec (_,i,_,_) => i
          | ConstDef (_,i,_,_,_) => i)

  fun constUni (c) =
      (case sgnLookup(c)
	 of ConstDec (_,_,_,L) => L
          | ConstDef (_,_,_,_,L) => L)

  (* Contexts *)

  (* ctxLookup (G, k) = x:V
     Invariant: 
     If      |G| >= k, where |G| is size of G,
     then    G |- k : V  and  G |- V : L
  *)
  fun ctxLookup (G, k) =
      let (* ctxLookup' (G'', k') = x:V
	     where G |- ^(k-k') : G'', 1 <= k' <= k
           *)
	fun ctxLookup' (Decl (G', Dec (x, V')), 1) = Dec (x, EClo (V', Shift (k)))
	  | ctxLookup' (Decl (G', _), k') = ctxLookup' (G', k'-1)
	 (* ctxLookup' (Null, k')  should not occur by invariant *)
      in
	ctxLookup' (G, k)

      end

  (* ctxLength G = |G|, the number of declarations in G *)
  fun ctxLength G =
      let 
	fun ctxLength' (Null, n) = n
	  | ctxLength' (Decl(G, _), n)= ctxLength' (G, n+1)
      in
	ctxLength' (G, 0)
      end

  (* Explicit Substitutions *)

  (* id = ^0 
  
     Invariant:
     G |- id : G        id is patsub
  *)
  val id : SubObj = Shift(0)

  (* shift = ^1
  
     Invariant:
     G, V |- ^ : G       ^ is patsub
  *)
  val shift : SubObj = Shift(1)

  (* bvarSub (n, s) = H'
   
     Invariant: 
     If    G |- s : G'    G' |- n : V
     then  H' = Hn         if  s = H1 .. Hn .. ^k
       or  H' = ^(n+k)     if  s = H1 .. Hm ^k   and m<n
     and   G |- H' : V [s]
  *)
  (*[ val bvarSub : int * SubObj -> HeadObj ]*)
  (*[ val headSub : HeadObj * SubObj -> HeadObj ]*)
  fun bvarSub (1, Dot(H, s)) = H
    | bvarSub (n, Dot(H, s)) = bvarSub (n-1, s)
    | bvarSub (n, Shift(k))  = Idx (n+k)
  (* headSub (H, s) = H'

     Invariant:
     If   G |- s : G'     G' |- H : V
     then H' = H [s]
     and  G |- H' : V [s]
  *)
  and headSub (Idx (n), s) = bvarSub (n, s)
    | headSub (Exp (U, V), s) = Exp(EClo (U, s),V)

  (* decSub (x:V, s) = D'

     Invariant:
     If   G  |- s : G'    G' |- V : L
     then D' = x:V[s]
     and  G  |- V[s] : L
  *)
  (*[ val decSub : DecObj * SubObj -> DecObj ]*)
  fun decSub (Dec (x, V), s) = Dec (x, EClo (V, s))

  (* comp (s1, s2) = s'

     Invariant:
     If   G'  |- s1 : G 
     and  G'' |- s2 : G'
     then s'  = s1 o s2
     and  G'' |- s1 o s2 : G

     If  s1, s2 patsub
     then s' patsub
   *)
  (*[ val comp : SubObj * SubObj -> SubObj ]*)
  fun comp (Shift (0), s) = s
    | comp (Shift (n), Dot (H, s)) = comp (Shift (n-1), s)
    | comp (Shift (n), Shift (m)) = Shift (n+m)
    | comp (Dot (H, s), s') = Dot (headSub (H, s'), comp (s, s'))

  (* dot1 (s) = s'

     Invariant:
     If   G |- s : G'
     then s' = 1. (s o ^)
     and  for all V s.t.  G' |- V : L
          G, V[s] |- s' : G', V 

     If s patsub then s' patsub
   *)
  (* may be optimized by unfolding definitions *)
  (* further optimization: dot1 (id) = 1.^1 = id *)

  (*[ val dot1 : SubObj -> SubObj  ]*)
  fun dot1 s = Dot (Idx(1), comp(s, shift))

  (* Constraint equations *)

  datatype Eqn =			(* Equations                  *)
      Eqn of Exp * Exp			(* Eqn ::= (U1 == U2)         *)

  (* EVar related functions *)

  (* newEVar (V) = X, X new

     If   G |- V : L      (for some level L)
     then G |- X : V      
  *)    
  (*[ val newEVar : (ExpFam -> ExpObj) & (ExpKind -> ExpFam)  ]*)
  fun newEVar (V) = EVar(ref NONE, V)

  (* newTypeVar () = X, X new

     where G |- X : type
  *)
  (*[ val newTypeVar : unit -> ExpFam  ]*)
  fun newTypeVar () = EVar(ref NONE, Uni(Type))

(*
end;  (* functor IntSyn *)
*)

(* whnf *)

    (* appendSpine ((S1, s1), (S2, s2)) = S' 

       Invariant:
       If    G |- s1 : G1   G1 |- S1 : V1' > V1 
       and   G |- s2 : G2   G2 |- S2 : V2  > V2'
       and   G |- V1 [s1] == V2 [s2]
       then  G |- S' : V1' [s1] > V2' [s2]   
    *)
    (*[ val appendSpine : (SpineFam * SubObj) * (SpineFam * SubObj) -> SpineFam
                          & (SpineObj * SubObj) * (SpineObj * SubObj) -> SpineObj  ]*)
    fun appendSpine ((Nil, s1), Ss2) = SClo Ss2
      | appendSpine ((App (U1, S1), s1), Ss2) =
          App (EClo (U1, s1), appendSpine ((S1, s1), Ss2))
      | appendSpine ((SClo (S1, s1'), s1), Ss2) =
	  appendSpine ((S1, comp(s1', s1)), Ss2)

    (* whnfRedex ((U, s1), (S, s2)) = (U', s')

       Invariant:
       If    G |- s1 : G1   G1 |- U : V1,   (U,s1) whnf 
	     G |- s2 : G2   G2 |- S : V2 > W2
	     G |- V1 [s1] == V2 [s2] == V : L
       then  G |- s' : G',  G' |- U' : W'
       and   G |- W'[s'] == W2[s2] == W : L
       and   G |- U'[s'] == (U[s1] @ S[s2]) : W
       and   (U',s') whnf

       Effects: EVars may be lowered to base type.
    *)
    (*[ val whnfRedex : (ExpFam * SubObj) * (SpineFam * SubObj) -> (ExpFam * SubObj)
                        & (ExpObj * SubObj) * (SpineObj * SubObj) -> (ExpObj * SubObj)  ]*)
    (*[ val lower : ExpFam -> unit
                    & ExpObj -> unit  ]*)
    (*[ val lower' : ExpFam option ref * (ExpKind * SubObj) -> unit
                     & ExpObj option ref * (ExpFam * SubObj) -> unit  ]*)
    (*[ val whnfRoot : ((ConFam * SpineFam) * SubObj) -> ExpFam * SubObj
                       & ((ConObj * SpineObj) * SubObj) -> ExpObj * SubObj  ]*)
    (*[ val whnf : ExpFam * SubObj -> ExpFam * SubObj
                   & ExpObj * SubObj -> ExpObj * SubObj  ]*)
    fun whnfRedex (Us, (SClo (S, s2'), s2)) =
          whnfRedex (Us, (S, comp (s2', s2)))
      | whnfRedex (Us as (Root R, s1), (Nil, s2)) = Us
      | whnfRedex ((Root (C1, S1), s1), (S2, s2)) =
	  (* S2 = App _, only possible if term is not eta-expanded *)
	  (Root (C1, appendSpine ((S1, s1), (S2, s2))), id)
      | whnfRedex ((Lam (Dec(_,V2), U1), s1), (App (U2, S), s2)) =
	  whnfRedex (whnf (U1, Dot (headSub (Exp (U2, V2), s2), s1)), (S, s2)) 
	  (* whnfRedex (whnf (U1, Dot (headSub (Exp (U2, V2), s2), s1)), (S, s2)) *)
      | whnfRedex (Us as (Lam _, s1), _) = Us  (* S2[s2] = Nil *)
      | whnfRedex (Us as (EVar _, s1), (Nil, s2)) = Us
      | whnfRedex (Us as (X as EVar _, s1), Ss2) = 
	  (* Ss2 must be App, since prior cases do not apply *)
	  (* lower X results in redex, optimize by unfolding call to whnfRedex *)
	  (lower X; whnfRedex (whnf Us, Ss2))
      (* Uni and Pi can arise after instantiation of EVar X : K *)
      | whnfRedex (Us as (Uni _, s1), _) = Us	(* S2[s2] = Nil *)
      | whnfRedex (Us as (Pi _, s1), _) = Us	(* S2[s2] = Nil *)
      (* Other cases impossible since (U,s1) whnf *)

    (* lower (X) = (), effect instantiates X

       Invariant:
       If    G |- X : (Pi x:V''.V')
       then  X := Lam x:V''. X'  where  G, x:V'' |- X' : V'

       Effects: X is instantiated
    *)
    (* possible optimization: lower all the way to base type in one step *)
    and lower (EVar (r, V)) = lower' (r, whnf (V, id))
    and lower' (r, (Pi ((D', _), V'), s')) = 
          (r := SOME (Lam (decSub (D', s'), newEVar (EClo (V', dot1 s')))))
        (* no other cases possible by well-typing invariant *)

    (* whnfRoot ((C, S), s) = (U', s')

       Invariant:
       If    G |- s : G1      G1 |- C : V
			      G1 |- S : V > W
       then  G |- s' : G'     G' |- U' : W'
       and   G |- W [s] = W' [s'] : L

       Effects: EVars may be instantiated when lowered
    *)
    and whnfRoot ((BVar(k), S), s)   =
        (case bvarSub (k, s)
	   of Idx(k) => (Root (BVar(k), SClo (S, s)), id)
	    | Exp(U,V) => whnfRedex (whnf (U, id), (S, s)))
      | whnfRoot ((FVar(name,V,s'), S), s) =
	 (Root (FVar(name, V, comp(s', s)), SClo (S, s)), id)
      | whnfRoot ((C, S), s) =
	 (Root (C, SClo (S, s)), id)

    (* whnf (U, s) = (U', s')

       Invariant:
       If    G |- s : G'    G' |- U : V
       then  G |- s': G''   G''|- U' : V'
       and   G |- V [s] == V' [s'] == V'' : L  
       and   G |- U [s] == U' [s'] : V'' 
       and   (U', s') whnf

       whnf = (L, s) | (Pi DP. U, s)
            | (Root(n,S), id) | (Root(c,S), id) | (Root(d,S), id) | (Root(F[s'], S), id)
            | (Lam D. U, s) | (X, s) where X is uninstantiated
    *)
    (*
       Possible optimization :
         Define whnf of Root as (Root (n , S [s]), id)
	 Fails currently because appendSpine does not necessairly return a closure  -- cs
	 Advantage: in unify, abstract... the spine needn't be treated under id, but under s
    *)
    and whnf (Us as (Uni _, s)) = Us
      | whnf (Us as (Pi _, s)) = Us
      | whnf (Root R, s) = whnfRoot (R, s)
      | whnf (Redex (U, S), s) = whnfRedex (whnf (U, s), (S, s))
      | whnf (Us as (Lam _, s)) = Us
(*      | whnf (EVar (ref (SOME U), _), s) = whnf (U, s) *)
(*      | whnf (Us as (EVar _, s)) = Us *)
      (* --------------------------------------------------------- *)
      (* next two avoid calls to whnf (V, id), where V is type of X *)
      | whnf (Us as (EVar (r, Root _), s)) = Us 
      | whnf (Us as (EVar (r, Uni _), s)) = Us 
      | whnf (Us as (X as EVar (r, V), s)) = 
          (case whnf (V, id) of
	     (Root _, _) => Us
	   | (Uni _, _) => Us
	   | (Pi _, _) => (lower X; whnf Us))
      (* --------------------------------------------------------- *)

      | whnf (EClo (U, s'), s) = whnf (U, comp (s', s)) 
*)