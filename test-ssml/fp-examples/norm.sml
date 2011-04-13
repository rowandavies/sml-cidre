(*
  norm.sml
  Author: Frank Pfenning
  Some normal-form invariants on lambda-calculus terms
  in deBruijn representation with explicit substitutions
*)

datatype term =
  Var of int
| Lam of term
| App of term * term;

datatype subst =
  Dot of term * subst
| Shift of int

(*[
(* Weak head-normal terms *)
datasort whnf = Lam of term | Var of int | App of head * term
     and head = Var of int | App of head * term

(* Normal terms *)
datasort norm = Lam of norm | Var of int | App of elim * norm
     and elim = Var of int | App of elim * norm
]*)

(*[
val subst <: term * subst -> term
and comp <: subst * subst -> subst
]*)
fun subst (Var(1), Dot(e, s)) = e
  | subst (Var(n), Dot(e, s)) = (* n > 1 *) 
      subst (Var(n-1), s)
  | subst (Var(n), Shift(k)) = Var(n+k)
  | subst (Lam(e), s) = Lam (subst (e, Dot(Var(1), comp(s, Shift(1)))))
  | subst (App(e1, e2), s) =
      App (subst (e1, s), subst (e2, s))
and comp (Shift(0), s') = s'
  | comp (Shift(n), Dot (e, s')) = comp (Shift (n-1), s')
  | comp (Shift(n), Shift(k)) = Shift(n+k)
  | comp (Dot(e, s), s') = Dot (subst (e, s'), comp (s, s'))

(*[
val whnf <: term -> whnf
and apply <: whnf * term -> whnf
]*)
fun whnf (Var(n)) = Var(n)
  | whnf (Lam(e)) = Lam(e)
  | whnf (App(e1,e2)) = apply (whnf e1, e2)
and apply (Var(n), e2) = App(Var(n), e2)
  | apply (Lam(e), e2) = whnf (subst (e, Dot(e2, Shift(0))))
  | apply (App(e11, e12), e2) = App(App(e11, e12), e2)

(*[
val norm <: term -> norm
and appnorm <: norm * term -> norm
]*)
fun norm (Var(n)) = Var(n)
  | norm (Lam(e)) = Lam(norm e)
  | norm (App(e1,e2)) = appnorm (norm e1, e2)
and appnorm (Var(n), e2) = App(Var(n), norm e2)
  | appnorm (Lam(e), e2) = norm (subst (e, Dot(e2, Shift(0))))
  | appnorm (App(e11, e12), e2) = App(App(e11, e12), norm e2)

(*[ val K <: norm ]*)
val K = Lam (Lam (Var 2))  (* \x.\y.x *)
(*[ val S <: norm ]*)
val S = Lam (Lam (Lam (App (App (Var 3, Var 1), App (Var 2, Var 1)))))
                           (* \x.\y.\z. x z (y z) *)
(*[ val I <: norm ]*)
val I = Lam (Var 1)        (* \x. x *)

(*[ val ex1 <: norm ]*)
val ex1 = norm (App (I, I))  (* \x. x *)
(*[ val ex2 <: norm ]*)
val ex2 = norm (App (App (S, K), K)) (* \x. x *)
