(*
   Heap-based abstract machine for MinML
   Distinguished small and large values
   Treats Int, Bool, T1 -> T2, T list, let, and recursion

   Based on similar material for Assignment 8, 15-312, Fall 2004
   Author: Frank Pfenning
*)

datatype Var =
    ValVar of string
  | ExpVar of string
(*[
datasort VVar = ValVar of string  (* value variables *)
datasort EVar = ExpVar of string  (* expression variables *)
]*)

datatype Exp =
    Var of Var
  | Int of int | Eq of Exp * Exp | Leq of Exp * Exp | Succ of Exp | Pred of Exp
  | True | False | If of Exp * Exp * Exp
  | Fn of Var * Exp | Apply of Exp * Exp
  | Nil | Cons of Exp * Exp | Case of Exp * Exp * (Var * Var * Exp)
  | Let of Exp * (Var * Exp)
  | Rec of Var * Exp
(* override default sort Exp *)
(*[
datasort Exp =
    Var of Var
  | Int of int | Eq of Exp * Exp | Leq of Exp * Exp | Succ of Exp | Pred of Exp
  | True | False | If of Exp * Exp * Exp
  | Fn of VVar * Exp | Apply of Exp * Exp
  | Nil | Cons of Exp * Exp | Case of Exp * Exp * (VVar * VVar * Exp)
  | Let of Exp * (VVar * Exp)
  | Rec of EVar * Exp
]*)

datatype Val =
    Int_ of int
  | True_ | False_
  | Nil_  | Cons_ of Val * Val
  | Closure of Env * Exp
  | Thunk of Env * Exp
  | Pointer of int
and Bind = Bind of Var * Val
withtype Env = Bind list
(*[
datasort SVal =				(* small values *)
    Int_ of int
  | True_ | False_
  | Nil_
  | Pointer of int
datasort PVal = Pointer of int		(* pointer values *)
datasort LVal =				(* large values *)
    Cons_ of SVal * SVal
  | Closure of SEnv * Exp
  | Thunk of SEnv * Exp
and SBind =
    Bind of VVar * SVal
  | Bind of EVar * PVal
withsort SEnv = SBind list
]*)

datatype Frame =
    Eq1 of Exp | Eq2 of Val | Leq1 of Exp | Leq2 of Val | Succ1 | Pred1
  | If1 of Exp * Exp
  | App1 of Exp | App2 of Val
  | Cons1 of Exp | Cons2 of Val | Case1 of Exp * (Var * Var * Exp)
  | Let1 of Var * Exp
(*[
datasort SFrame =
    Eq1 of Exp | Eq2 of SVal | Leq1 of Exp | Leq2 of SVal | Succ1 | Pred1
  | If1 of Exp * Exp
  | App1 of Exp | App2 of SVal
  | Cons1 of Exp | Cons2 of SVal | Case1 of Exp * (VVar * VVar * Exp)
  | Let1 of VVar * Exp
]*)
 
datatype Cont =
    Frame of Frame
  | Env of Env
(*[
datasort SCont =
    Frame of SFrame
  | Env of SEnv
]*)

type Stack = Cont list
(*[
sortdef SStack = SCont list
]*)

exception RunTimeError of string

exception Lookup

(*[ val lookupEnv : SEnv -> VVar -> SVal option & SEnv -> EVar -> PVal option ]*)
(* First version fails for lack of equality tracking and/or splitting? *)
(*
fun lookupEnv (Bind(y,v)::eta) x =
      if (x = y) then SOME(v) else lookupEnv eta x
  | lookupEnv (nil) x = NONE
*)
fun lookupEnv (Bind(ValVar(y),v)::eta) (ValVar(x)) =
      if (x = y) then SOME(v) else lookupEnv eta (ValVar(x))
  | lookupEnv (Bind(ExpVar(w),v)::eta) (ExpVar(u)) =
      if (w = u) then SOME(v) else lookupEnv eta (ExpVar(u))
  | lookupEnv (b::eta) x = lookupEnv eta x
  | lookupEnv (nil) x = NONE

(*[ val lookupStack : SStack -> VVar -> SVal
                    & SStack -> EVar -> PVal ]*)
fun lookupStack (Frame _::k) x = lookupStack k x
  | lookupStack (Env(eta)::k) x =
    (case lookupEnv eta x
       of SOME(v) => v
        | NONE => lookupStack k x)
  | lookupStack k x = raise RunTimeError("Free variable")

(*[ val clo : SStack * Var list * Exp * SEnv -> SEnv ]*)
(* first two cases are duplicated due to a lack of splitting(?) *)
fun clo (k, g, Var(x as ValVar _), eta) =
    if List.exists (fn y => x = y) g then eta
    else (case lookupEnv eta x
            of SOME _ => eta (* already collected *)
	     | NONE => Bind(x,lookupStack k x)::eta)
  | clo (k, g, Var(u as ExpVar _), eta) =
    if List.exists (fn w => u = w) g then eta
    else (case lookupEnv eta u
            of SOME _ => eta (* already collected *)
             | NONE => Bind(u,lookupStack k u)::eta)
  | clo (k, g, Int(n), eta) = eta
  | clo (k, g, Eq(e1,e2), eta) = clo (k, g, e1, clo (k, g, e2, eta))
  | clo (k, g, Leq(e1,e2), eta) = clo (k, g, e1, clo (k, g, e2, eta))
  | clo (k, g, Pred(e1), eta) = clo (k, g, e1, eta)
  | clo (k, g, Succ(e1), eta) = clo (k, g, e1, eta)
  | clo (k, g, True, eta) = eta
  | clo (k, g, False, eta) = eta
  | clo (k, g, If(e1,e2,e3), eta) = 
      clo (k, g, e1, clo (k, g, e2, clo (k, g, e3, eta)))
  | clo (k, g, Fn(x,e), eta) = clo (k, x::g, e, eta)
  | clo (k, g, Apply(e1,e2), eta) = clo (k, g, e1, clo (k, g, e2, eta))
  | clo (k, g, Nil, eta) = eta
  | clo (k, g, Cons(e1,e2), eta) = clo (k, g, e1, clo (k, g, e2, eta))
  | clo (k, g, Case(e1,e2,(x,y,e3)), eta) =
      clo (k, g, e1, clo (k, g, e2, clo (k, y::x::g, e3, eta)))
  | clo (k, g, Let(e1,(x,e2)), eta) = clo (k, g, e1, clo (k, x::g, e2, eta))
  | clo (k, g, Rec(u,e), eta) = clo (k, u::g, e, eta)

(*[ val closure : SStack * Exp -> LVal ]*)
fun closure (k, e) = Closure(clo (k, nil, e, nil), e)
(*[ val thunk : SStack * Exp -> LVal ]*)
fun thunk (k, e) = Thunk(clo (k, nil, e, nil), e)

signature HEAP =
sig
  exception HeapSizeExceeded
  val alloc : Stack * Val -> Val
  (*[ val alloc : SStack * LVal -> PVal ]*)
  val read : Val -> Val
  (*[ val read : PVal -> LVal ]*)
end (* signature HEAP *)

structure Heap :> HEAP =
struct
  (* this version performs no garbage collection *)
  exception HeapSizeExceeded
  val heap_size = 4096
  (*[ val heap : LVal Array.array ]*)
  (* initialize with dummy value *)
  val heap = Array.array (heap_size,Closure(nil,Int(0)))

  (*[ val next : int ref ]*)
  val next = ref 0

  (*[ val alloc : SStack * LVal -> PVal ]*)
  fun alloc (k,w) =
      let
	val l = !next
	val _ = if l >= heap_size then raise HeapSizeExceeded else ()
	val _ = next := l+1
	val _ = Array.update(heap, l, w)
      in Pointer(l) end

  (*[ val read : PVal -> LVal ]*)
  fun read (Pointer(l)) = Array.sub(heap, l)
end (* structure Heap *)

(*[
val eval : SStack -> Exp -> SVal
and return : SStack -> SVal -> SVal
]*)
fun eval k (Var(ValVar(x))) = return k (lookupStack k (ValVar(x)))
  | eval k (Var(ExpVar(u))) =
    (case Heap.read (lookupStack k (ExpVar(u)))
       of Thunk(eta,e) => eval (Env(eta)::k) e
        | _ => raise RunTimeError("Exp variable not bound to thunk"))
  | eval k (Int(n)) = return k (Int_(n))
  | eval k (Eq(e1,e2)) = eval (Frame(Eq1(e2))::k) e1
  | eval k (Leq(e1,e2)) = eval (Frame(Leq1(e2))::k) e1
  | eval k (Succ(e1)) = eval (Frame(Succ1)::k) e1
  | eval k (Pred(e1)) = eval (Frame(Pred1)::k) e1
  | eval k (True) = return k True_
  | eval k (False) = return k False_
  | eval k (If(e1,e2,e3)) = eval (Frame(If1(e2,e3))::k) e1
  | eval k (Fn(x,e)) = return k (Heap.alloc (k,closure(k, Fn(x,e))))
  | eval k (Apply(e1,e2)) = eval (Frame(App1(e2))::k) e1
  | eval k (Nil) = return k Nil_
  | eval k (Cons(e1,e2)) = eval (Frame(Cons1(e2))::k) e1
  | eval k (Case(e1,e2,(x,y,e3))) = eval (Frame(Case1(e2, (x,y,e3)))::k) e1
  | eval k (Let(e1,(x,e2))) = eval (Frame(Let1(x,e2))::k) e1
  | eval k (Rec(u,e)) = eval (Env(Bind(u,Heap.alloc(k,thunk(k,Rec(u,e))))::nil)::k) e

and return nil v = v
  | return (Env(eta)::k) v = return k v
  | return (Frame(Eq1(e2))::k) v1 = eval (Frame(Eq2(v1))::k) e2
  | return (Frame(Eq2(Int_(n1)))::k) (Int_(n2)) =
      return k (if n1 = n2 then True_ else False_)
  | return (Frame(Leq1(e2))::k) v1 = eval (Frame(Leq2(v1))::k) e2
  | return (Frame(Leq2(Int_(n1)))::k) (Int_(n2)) =
      return k (if n1 <= n2 then True_ else False_)
  | return (Frame(Succ1)::k) (Int_(n)) = return k (Int_(n+1))
  | return (Frame(Pred1)::k) (Int_(n)) = return k (Int_(n-1))
  | return (Frame(If1(e2,e3))::k) True_ = eval k e2
  | return (Frame(If1(e2,e3))::k) False_ = eval k e3
  | return (Frame(App1(e2))::k) v1 = eval (Frame(App2(v1))::k) e2
  | return (Frame(App2(Pointer(l)))::k) v2 =
    (case Heap.read(Pointer(l))
       of Closure(eta,Fn(x,e1')) => eval (Env(Bind(x,v2)::eta)::k) e1'
        | _ => raise RunTimeError("Dynamic type error: apply"))
  | return (Frame(Cons1(e2))::k) v1 = eval (Frame(Cons2(v1))::k) e2
  | return (Frame(Cons2(v1))::k) v2 = return k (Heap.alloc(k, Cons_(v1,v2)))
  | return (Frame(Case1(e2,(x,y,e3)))::k) (Nil_) = eval k e2
  | return (Frame(Case1(e2,(x,y,e3)))::k) (Pointer(l)) =
    (case Heap.read (Pointer(l))
       of Cons_(v1,v2) => eval (Env(Bind(x,v1)::Bind(y,v2)::nil)::k) e3
        | _ => raise RunTimeError("Dynamic type error: case"))
  | return (Frame(Let1(x,e2))::k) v =
      eval (Env(Bind(x,v)::nil)::k) e2
  | return k v = raise RunTimeError("Dynamic type error of unknown form")

(* Printing *)
fun toStringVal (Int_(n)) = Int.toString(n)
  | toStringVal (True_) = "True"
  | toStringVal (False_) = "False"
  | toStringVal (Nil_) = "Nil"
  | toStringVal (Cons_(v1,v2)) = "Cons(" ^ toStringVal v1 ^ "," ^ toStringVal v2 ^ ")"
  | toStringVal (Closure _) = "-"
  | toStringVal (Thunk _) = "%"
  | toStringVal (Pointer(l)) = "^" ^ toStringVal (Heap.read (Pointer(l)))

(* Examples *)
(*
Compiler.Control.Print.printDepth := 100;
*)
val x_ = ValVar("x");
val x = Var(x_);
val y_ = ValVar("y");
val y = Var(y_);
val h_ = ValVar("h");
val h = Var(h_);
val t_ = ValVar("t");
val t = Var(t_);
val u_ = ExpVar("u");
val u = Var(u_);
val s_ = ValVar("s");
val s = Var(s_);
val l_ = ValVar("l");
val l = Var(l_);

val id = Fn(x_,x);
val r1 = eval nil (Apply(id,id));

val downfrom = Rec(u_,Fn(x_,If(Eq(x,Int(0)),Nil,Cons(x,Apply(u,Pred(x))))));
val r2 = eval nil (Apply(downfrom, Int(4)));

val revapp = Rec(u_,Fn(x_,Fn(y_,Case(x,y,(h_,t_,Apply(Apply(u,t),Cons(h,y)))))));
val l123 = Cons(Int(1),Cons(Int(2),Cons(Int(3),Nil)));
val r3 = eval nil (Apply(Apply(revapp,l123),l123));

val append = Rec(u_,Fn(x_,Fn(y_,Case(x,y,(h_,t_,Cons(h,Apply(Apply(u,t),y)))))));
val r4 = eval nil (Apply(Apply(append,l123),l123));

val l41253 = Cons(Int(4),Cons(Int(1),Cons(Int(2),Cons(Int(5),Cons(Int(3),Nil)))));
val split = Rec(u_,Fn(x_,Fn(y_,Fn(s_,Fn(l_,Case(y,Cons(s,Cons(l,Nil)),
(h_,t_,If(Leq(h,x),Apply(Apply(Apply(Apply(u,x),t),Cons(h,s)),l),
                     Apply(Apply(Apply(Apply(u,x),t),s),Cons(h,l))))))))));
val r5 = eval nil (Apply (Apply (Apply (Apply (split, Int(3)), l41253), Nil), Nil));

val hd = Fn(x_,Case(x,Nil,(h_,t_,h)));
val tl = Fn(x_,Case(x,Nil,(h_,t_,t)));

val qsort = Rec(u_,Fn(x_,Case(x,Nil,(h_,t_,
              Let(Apply(Apply(Apply(Apply(split,h),t),Nil),Nil),(s_,
                  Apply(Apply(append,Apply(u,Apply(hd,s))),
                        Cons(h,Apply(u,Apply(hd,Apply(tl,s)))))))))));
val r6 = eval nil (Apply (qsort, l41253));

(* the following program incorrectly uses a value variable y for recursion *)
(* this yields a strange error message on r6 *)
(*
val qsort = Rec(y_,Fn(x_,Case(x,Nil,(h_,t_,
              Let(Apply(Apply(Apply(Apply(split,h),t),Nil),Nil),(s_,
                  Apply(Apply(append,Apply(y,Apply(hd,s))),
                        Cons(h,Apply(y,Apply(hd,Apply(tl,s)))))))))));
val r6 = eval nil (Apply (qsort, l41253));
*)

val r7 = eval nil (Apply (Int(3), Int(2)))
         handle RunTimeError(msg) => (print (msg ^ "\n"); Int_(0));

val h'_ = ValVar("h'");
val h' = Var(h'_);
val t'_ = ValVar("t'");
val t' = Var(t'_);

val init = Rec(u_,Fn(x_,Case(x,Nil,(h_,t_,
            Cons(Cons(h,Nil),Apply(u,t))))));
val r8 = eval nil (Apply(init,l41253));
val merge = Rec(u_,Fn(x_,Fn(y_,
              Case(x,y,(h_,t_,Case(y,x,(h'_,t'_,
                If(Leq(h,h'),
                   Cons(h,Apply(Apply(u,t),y)),
                   Cons(h',Apply(Apply(u,x),t'))))))))));
val r9 = eval nil (Apply(Apply(merge,Cons(Int(1),Cons(Int(3),Cons(Int(5),Nil)))),
                         Cons(Int(2),Cons(Int(4),Nil))));
val r10 = eval nil (Apply(Apply(merge,Cons(Int(2),Cons(Int(4),Nil))),
                         Cons(Int(1),Cons(Int(3),Cons(Int(5),Nil)))));
val msort = Rec(u_,Fn(x_,Fn(y_,
              Case(x,Case(y,Nil,(h'_,t'_,Apply(Apply(u,y),Nil))),(h_,t_,
               Case(t,Case(y,h,(h'_,t'_,Apply(Apply(u,Cons(h,y)),Nil))),(h'_,t'_,
                Apply(Apply(u,t'),Cons(Apply(Apply(merge,h),h'),y)))))))))
val mergesort = Fn(x_,Apply(Apply(msort,Apply(init,x)),Nil))

val r11 = eval nil (Apply (mergesort, Nil));
val r12 = eval nil (Apply (mergesort, Cons(Int(1),Nil)));
val r13 = eval nil (Apply (mergesort, Cons(Int(2),Cons(Int(1),Nil))));
val r14 = eval nil (Apply (mergesort, Cons(Int(3),Cons(Int(2),Cons(Int(1),Nil)))));
val r15 = eval nil (Apply(mergesort,l41253));
