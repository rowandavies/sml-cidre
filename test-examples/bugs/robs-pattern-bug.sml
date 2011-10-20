
(*[ datasort 'a none = NONE ]*)
(*[ datasort 'a some = SOME of 'a ]*)

(*[ datasort true = true ]*)
(*[ datasort false = false ]*)


(*[ datasort 'a conslist = :: of 'a * 'a list ]*)

signature LIST = sig

include LIST

(*[ val null: 'a conslist -> false
            & 'a list -> bool ]*)

(*[ val hd: 'a conslist -> 'a ]*)

(*[ val tl: 'a conslist -> 'a list ]*)

(*[ val last: 'a conslist -> 'a ]*)

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a list -> ('a * 'a list) option ]*)

(*[ val rev: 'a conslist -> 'a conslist
           & 'a list -> 'a list ]*)

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a list * 'a list -> 'a list ]*)
   
(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)

end

structure List:> LIST = struct

open List

(*[ val null: 'a conslist -> false
            & 'a list -> bool ]*)
fun null _ = raise Match

(*[ val hd: 'a conslist -> 'a ]*)
fun hd (x :: xs) = x

(*[ val tl: 'a conslist -> 'a list ]*)
fun tl (x :: xs) = xs

(*[ val last: 'a conslist -> 'a ]*)
fun last [ x ] = raise Empty
  | last (x :: y :: xs) = last (y :: xs)

(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs

(*[ val getItem: 'a conslist -> ('a * 'a list) some
               & 'a list -> ('a * 'a list) option ]*)
fun getItem [] = NONE
  | getItem (x :: xs) = SOME (x, xs)

(*[ val revAppend: 'a list * 'a conslist -> 'a conslist 
                 & 'a conslist * 'a list -> 'a conslist
                 & 'a list * 'a list -> 'a list ]*)
fun revAppend ([], ys) = ys 
  | revAppend (x :: xs, ys) = revAppend (xs, x :: ys)

(*[ val rev: 'a conslist -> 'a conslist
           & 'a list -> 'a list ]*)
fun rev xs = revAppend (xs, [])


end

val map = List.map


signature SYMBOL = sig 
  type symbol
  val fromValue : string -> symbol
  val toValue : symbol -> string
  val eq : symbol * symbol -> bool
end
structure Symbol = struct (*[ assumesig SYMBOL ]*) end


signature SETX = sig
  type set
  type t
  val empty : set
  val union : set -> set -> set
  val remove : set -> t -> set
  val member : set -> t -> bool
  val singleton : t -> set
end where type t=Symbol.symbol

structure SetX = struct (*[ assumesig SETX ]*) end

signature DICTX = sig
  type 'a dict
  type t
  val empty : 'a dict
(*  val union : set -> set -> set
  val remove : set -> t -> set
  val member : set -> t -> bool
  val singleton : t -> set
*)

end

structure DictX = struct (*[ assumesig DICTX ]*) end

signature POS = sig
  type t
end

structure Pos = struct (*[ assumesig POS ]*) end




(* syntax.sml *)

(* Abstract syntax tree for L10 *)
(* Robert J. Simmons *)

structure Type = 
struct
   val t = Symbol.fromValue "t"
   val nat = Symbol.fromValue "nat"
   val string = Symbol.fromValue "string"
   val world = Symbol.fromValue "world" (* Pseudo-type for world names *)
   val rel = Symbol.fromValue "rel" (* Pseudo-type for predicates *)
end

structure Mode = 
struct
   datatype t = Input | Output | Ignore
   fun toString Input = "+"
     | toString Output = "-"
     | toString Ignore = "_"
end

structure Term = 
struct
   datatype t = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Root of Symbol.symbol * t list
    | Var of Symbol.symbol option * Symbol.symbol option
    | Mode of Mode.t * Symbol.symbol option
(*[
   datasort term = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Var of Symbol.symbol option * Symbol.symbol none
    | Root of Symbol.symbol * term conslist

   datasort term_t = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Var of Symbol.symbol option * Symbol.symbol some
    | Root of Symbol.symbol * term_t conslist

   datasort ground = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Root of Symbol.symbol * ground conslist
  
   datasort shape = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Var of Symbol.symbol none * Symbol.symbol none
    | Root of Symbol.symbol * shape conslist

   datasort moded = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Mode of Mode.t * Symbol.symbol none
    | Root of Symbol.symbol * moded conslist

   datasort moded_t = 
      SymConst of Symbol.symbol 
    | NatConst of IntInf.int
    | StrConst of string
    | Mode of Mode.t * Symbol.symbol some
    | Root of Symbol.symbol * moded_t conslist
]*)

   fun eq (term1, term2) = 
      case (term1, term2) of
         (SymConst c1, SymConst c2) => Symbol.eq (c1, c2)
       | (NatConst n1, NatConst n2) => n1 = n2
       | (StrConst s1, StrConst s2) => s1 = s2
       | (Var (NONE, _), (Var (NONE, _))) => true
       | (Var (SOME v1, _), Var (SOME v2, _)) => Symbol.eq (v1, v2)
       | (Mode (m1, _), Mode (m2, _)) => m1 = m2
       | (Root (f1, terms1), Root (f2, terms2)) => 
         Symbol.eq(f1, f2)
         andalso List.all eq (ListPair.zip (terms1, terms2))
       | (_, _) => false

   fun fv term =
      case term of 
         Var (SOME x, _) => SetX.singleton x
       | Root (_, terms) => fvs terms
       | _ => SetX.empty

   and fvs terms = 
      List.foldr (fn (t, set) => SetX.union (fv t) set) SetX.empty terms

   fun toString term = 
      case term of 
         SymConst c => Symbol.toValue c
       | NatConst i => IntInf.toString i
       | StrConst s => "\"" ^ s ^ "\""
       | Var (NONE, _) => "_"
       | Var (SOME x, _) => Symbol.toValue x
       | Mode (m, _) => Mode.toString m
       | Root (f, terms) => 
         if Symbol.eq (f, Symbol.fromValue "_plus") andalso length terms = 2
         then ("(" ^ toString (hd terms) 
               ^ " + " ^ toString (hd (tl terms)) ^ ")")
         else ("(" 
               ^ Symbol.toValue f 
               ^ String.concat (map (fn term => " " ^ toString term) terms)
               ^ ")")

   (*[ sortdef subst = term DictX.dict ]*)
  
   (* Total substitution *)
   (*[ val subst: subst * term -> term option ]*)
   (*[ val substs: subst * term list -> term list option
                 & subst * term conslist -> term conslist option ]*)
   fun subst (map, term) =
      case term of 
         SymConst c => SOME (SymConst c)
       | NatConst n => SOME (NatConst n)
       | StrConst s => SOME (StrConst s)
       | Var (NONE, _) => NONE
       | Var (SOME x, _) => DictX.find map x
       | Root (f, terms) =>  
         (case substs (map, terms) of
             NONE => NONE
           | SOME terms => SOME (Root (f, terms)))
   and substs (map, []) = SOME []
     | substs (map, term :: terms) = 
       case (subst (map, term), substs (map, terms)) of 
          (SOME term, SOME terms) => SOME (term :: terms) 
        | _ => NONE

   (* Partial substition *)
   (*[ val sub: (term * Symbol.symbol) -> term -> term ]*)
   fun sub (term', x) term = 
      case term of
         SymConst c => SymConst c
       | NatConst n => NatConst n
       | StrConst s => StrConst s
       | Var (NONE, ty) => Var (NONE, ty)
       | Var (SOME y, ty) => 
         if Symbol.eq (x, y) then term' else Var (SOME y, ty)
       | Root (f, terms) => 
         Root (f, map (sub (term', x)) terms)

   (*[ val hasUscore: term -> bool ]*)
   fun hasUscore term = 
      case term of 
         Var (NONE, _) => true  
       | Root (_, terms) => List.exists hasUscore terms
       | _ => false
end

structure Atom = struct
   type t = Symbol.symbol * Term.t list
   (*[ sortdef world = Symbol.symbol * Term.term list ]*)
   (*[ sortdef world_t = Symbol.symbol * Term.term_t list ]*)
   (*[ sortdef prop = Symbol.symbol * Term.term list ]*) 
   (*[ sortdef prop_t = Symbol.symbol * Term.term_t list ]*) 
   (*[ sortdef ground_world = Symbol.symbol * Term.ground list ]*) 
   (*[ sortdef ground_prop = Symbol.symbol * Term.ground list ]*) 
   (*[ sortdef moded = Symbol.symbol * Term.moded list ]*) 
   (*[ sortdef moded_t = Symbol.symbol * Term.moded_t list ]*) 

   (*[ val fv: (world & prop) -> SetX.set ]*)
   fun fv (_, terms) = Term.fvs terms

   (*[ val fvs: (world & prop) list -> SetX.set ]*)
   fun fvs atoms = 
      List.foldr (fn (atom, set) => SetX.union (fv atom) set) SetX.empty atoms

   (*[ val hasUscore: (world & prop) -> bool ]*)
   fun hasUscore (_, terms) = List.exists Term.hasUscore terms

   (*[ val eq: (t * t) -> bool ]*)
   fun eq ((a1, terms1), (a2, terms2)) =
     Symbol.eq (a1, a2) andalso List.all Term.eq (ListPair.zip (terms1, terms2))

   (*[ val toString': bool -> t -> string ]*) 
   (*[ val toString: t -> string ]*) 
   fun toString' parens (w, []) = Symbol.toValue w
     | toString' parens (w, terms) =
       (if parens then "(" else "")
       ^ Symbol.toValue w 
       ^ String.concat (map (fn term => " " ^ Term.toString term) terms)
       ^ (if parens then ")" else "")
   val toString = toString' false
end

structure Pat = struct
   datatype t =
      Atom of Atom.t
    | Exists of Symbol.symbol * t
    | Conj of t * t (* Should be implemented, difficult - rjs Oct 12 2011 *)
    | One (* Could be implemented without much trouble *)
(*[
   datasort pat = 
      Atom of Atom.prop
    | Exists of Symbol.symbol * pat

   datasort pat_t = 
      Atom of Atom.prop_t
    | Exists of Symbol.symbol * pat_t
]*)

   (*[ val fv: pat -> SetX.set ]*)
   fun fv pat = 
      case pat of 
         Atom atomic => Atom.fv atomic
       | Exists (x, pat) => 
         let val vars = fv pat 
         in if SetX.member vars x then SetX.remove vars x else vars end

   (*[ val fvs: pat list -> SetX.set ]*)
   fun fvs pats = 
      foldl (fn (x, y) => SetX.union x y) SetX.empty (map fv pats)

   fun toString pat = 
      case pat of
         Atom atm => Atom.toString' false atm
       | Exists (x, pat0) => 
         "(Ex " ^ Symbol.toValue x ^ ". " ^ toString pat0 ^ ")"
       | Conj (pat1, pat2) => toString pat1 ^ ", " ^ toString pat2
       | One => "1"
end

structure Binrel = struct
   datatype t = Eq | Neq | Gt | Geq 

   fun toString Eq = "=="
     | toString Neq = "!="
     | toString Gt = ">"
     | toString Geq = ">="
end

structure Prem = struct
   datatype t = 
      Normal of Pat.t
    | Negated of Pat.t
    | Binrel of Binrel.t * Term.t * Term.t * Symbol.symbol option
(*[ 
   datasort prem = 
      Normal of Pat.pat
    | Negated of Pat.pat
    | Binrel of Binrel.t * Term.term * Term.term * Symbol.symbol none

   datasort prem_t = 
      Normal of Pat.pat_t
    | Negated of Pat.pat_t
    | Binrel of Binrel.t * Term.term_t * Term.term_t * Symbol.symbol some
]*)

   (*[ val fv: prem -> SetX.set ]*)
   fun fv prem = 
      case prem of 
         Normal pat => Pat.fv pat
       | Negated pat => Pat.fv pat
       | Binrel (_, term1, term2, _) => 
           SetX.union (Term.fv term1) (Term.fv term2)

   fun toString prem =  
      case prem of 
         Normal pat => Pat.toString pat
       | Negated pat => "not (" ^ Pat.toString pat ^ ")"
       | Binrel (br, term1, term2, _) => 
         Term.toString term1 ^ " "
         ^ Binrel.toString br ^ " "
         ^ Term.toString term2
end

structure Rule = struct
   type t = (Pos.t * Prem.t) list * (Pos.t * Atom.t) list   
   (*[ sortdef rule = 
          (Pos.t * Prem.prem) list * (Pos.t * Atom.prop) list ]*)
   (*[ sortdef rule_t = 
          (Pos.t * Prem.prem_t) list * (Pos.t * Atom.prop_t) list ]*)

   (*[ val fv: rule -> SetX.set ]*)
   fun fv ((prems, concs): t) =
      SetX.union 
         (List.foldl (fn (x, y) => SetX.union x y)
            SetX.empty (map (Prem.fv o #2) prems))
         (List.foldl (fn (x, y) => SetX.union x y) 
            SetX.empty (map (Atom.fv o #2) concs))   
end

structure Class = 
struct
   datatype t = 
      Base of Symbol.symbol 
    | Rel of Pos.t * Atom.t
    | World
    | Type
    | Builtin
    | Extensible
    | Arrow of Symbol.symbol * t
    | Pi of Symbol.symbol * Symbol.symbol option * t
(*[
   datasort world = 
      World
    | Arrow of Symbol.symbol * world

   datasort typ =
      Base of Symbol.symbol
    | Arrow of Symbol.symbol * typ

   datasort rel = 
      Rel of Pos.t * Atom.world
    | Arrow of Symbol.symbol * rel
    | Pi of Symbol.symbol * Symbol.symbol option * rel

   datasort rel_t = 
      Rel of Pos.t * Atom.world_t
    | Arrow of Symbol.symbol * rel_t
    | Pi of Symbol.symbol * Symbol.symbol some * rel_t

   datasort knd = 
      Type
    | Builtin
    | Extensible
]*)
   
   fun arrows class = 
      case class of 
         Arrow (_, class) => 1+arrows class
       | Pi (_, _, class) => 1+arrows class
       | _ => 0
 
   (*[ val relToTyp: rel_t -> typ ]*)
   fun relToTyp class = 
      case class of
         Rel _ => Base Type.rel
       | Arrow (t, class) => Arrow (t, relToTyp class)
       | Pi (x, SOME t, class) => Arrow (t, relToTyp class)

   (*[ val worldToTyp: world -> typ ]*)
   fun worldToTyp class = 
      case class of 
         World => Base Type.world
       | Arrow (t, class) => Arrow (t, worldToTyp class)

   (*[ val base: typ -> Symbol.symbol ]*)
   fun base class = 
      case class of 
          Arrow (_, class) => base class
        | Base t => t

   (*[ val rel: rel -> Atom.world & rel_t -> Atom.world_t]*)
   fun rel class = 
      case class of
         Arrow (_, class) => rel class
       | Pi (_, _, class) => rel class
       | Rel (_, world) => world

   fun toString typ = 
      case typ of 
         Base t => Symbol.toValue t
       | Rel (_, world) => "rel @ " ^ Atom.toString' false world
       | World => "world"
       | Type => "type"
       | Builtin => "builtin"
       | Extensible => "extensible"
       | Arrow (t, typ) => Symbol.toValue t ^ " -> " ^ toString typ
       | Pi (x, NONE, typ) => 
         "{" ^ Symbol.toValue x ^ "} " ^ toString typ
       | Pi (x, SOME t, typ) => 
         "{" ^ Symbol.toValue x ^ ": " ^ Symbol.toValue t ^ "} " ^ toString typ
end

structure Decl = struct
   (*[ sortdef db = 
          Symbol.symbol 
          * (Pos.t * Atom.ground_prop) list 
          * (Pos.t * Atom.ground_world) ]*)

   (*[ sortdef depend = 
          (Pos.t * Atom.world) * (Pos.t * Atom.world) list ]*)

   (*[ sortdef depend_t = 
          (Pos.t * Atom.world_t) * (Pos.t * Atom.world_t) list ]*)

   datatype t = 
      World of Pos.t * Symbol.symbol * Class.t
    | Const of Pos.t * Symbol.symbol * Class.t
    | Rel of Pos.t * Symbol.symbol * Class.t
    | Type of Pos.t * Symbol.symbol * Class.t
    | DB of Pos.t * (Symbol.symbol * (Pos.t * Atom.t) list * (Pos.t * Atom.t))
    | Depend of Pos.t * ((Pos.t * Atom.t) * (Pos.t * Atom.t) list)
    | Rule of Pos.t * Rule.t
    | Query of Pos.t * Symbol.symbol * Atom.t
(*[
   datasort decl = 
      World of Pos.t * Symbol.symbol * Class.world
    | Const of Pos.t * Symbol.symbol * Class.typ
    | Rel of Pos.t * Symbol.symbol * Class.rel
    | Type of Pos.t * Symbol.symbol * Class.knd
    | DB of Pos.t * db
    | Depend of Pos.t * depend
    | Rule of Pos.t * Rule.rule
    | Query of Pos.t * Symbol.symbol * Atom.moded

   datasort decl_t = 
      World of Pos.t * Symbol.symbol * Class.world
    | Const of Pos.t * Symbol.symbol * Class.typ
    | Rel of Pos.t * Symbol.symbol * Class.rel_t
    | Type of Pos.t * Symbol.symbol * Class.knd
    | DB of Pos.t * db
    | Depend of Pos.t * depend_t
    | Rule of Pos.t * Rule.rule_t
    | Query of Pos.t * Symbol.symbol * Atom.moded_t

   datasort class = 
      World of Pos.t * Symbol.symbol * Class.world
    | Const of Pos.t * Symbol.symbol * Class.typ
    | Rel of Pos.t * Symbol.symbol * Class.rel
    | Type of Pos.t * Symbol.symbol * Class.knd
]*)

(*[ val print: decl_t -> unit ]*)
val print = 
 fn World (_, w, class) => 
       print (Symbol.toValue w ^ ": " ^ Class.toString class ^ "\n")
  | Const (_, c, class) => 
       print (Symbol.toValue c ^ ": " ^ Class.toString class ^ "\n")
  | Rel (_, a, class) => 
       print (Symbol.toValue a ^ ": " ^ Class.toString class ^ "\n")
  | Type (_, t, class) => 
       print (Symbol.toValue t ^ ": " ^ Class.toString class ^ "\n")
  | DB (_, (db, _, _)) => 
       print ("{-# DATABASE " ^ Symbol.toValue db ^ " ... #-}")
  | Depend (_, (conc, prems)) => 
       ( print (Atom.toString (#2 conc) ^ " <- ")
       ; print (String.concatWith ", " (map (Atom.toString o #2) prems))
       ; print "\n")
  | Rule (_, (prems, concs)) =>
       ( print (String.concatWith ", " (map (Prem.toString o #2) prems))
       ; print " -> "
       ; print (String.concatWith ", " (map (Atom.toString o #2) concs))
       ; print "\n")
  | Query (_, qry, mode) =>
       ( print ("{-# QUERY " ^ Symbol.toValue qry ^ " ")
       ; print (Atom.toString mode ^ " #-}\n"))
end


(* bug.sml *)

(*[ val check: Decl.decl -> Decl.decl_t ]*)
fun check decl = 
   case decl of 
      Decl.Const (pos, c, class) => 
      let 
         (*[ val typ: Class.typ ]*)
         val typ = tc_closed_class pos class
         (*[ val knd: Class.knd ]*)
         val knd = Tab.lookup Tab.types (Class.base class)
      in
         ( tc_namespace pos "Constant" c
         ; case knd of
              Class.Type => ()
            | Class.Extensible => 
              (case typ of Class.Base _ => ()
                | _ => raise TypeError (pos, "Extensible type `" 
                                             ^ Symbol.toValue (Class.base typ) 
                                             ^ "` cannot have a complex\
                                             \ constructor; only `" 
                                             ^ Symbol.toValue c ^ ": "
                                             ^ Symbol.toValue (Class.base typ) 
                                             ^ "` is allowed."))
            | Class.Builtin => 
                 raise TypeError (pos, "Built-in type `"
                                       ^ Symbol.toValue (Class.base typ) 
                                       ^ "` cannot be given new constants.")
         ; Decl.Const (pos, c, typ))
      end
    | _ => raise Match
