
(*[ datasort 'a conslist = :: of 'a * 'a list ]*)

(*[ val map: ('a -> 'b) -> 'a conslist -> 'b conslist
           & ('a -> 'b) -> 'a list -> 'b list ]*)
fun map f [] = []
  | map f (x :: xs) = f x :: map f xs


(*[ datasort 'a none = NONE ]*)
(*[ datasort 'a some = SOME of 'a ]*)


structure Symbol =
struct 
   type symbol = string
end 

structure Mode =
struct 
   type t = unit
end 

structure Type :> sig type t val nat : t end = 
struct
   type t = Symbol.symbol

   val nat : t = "nat"

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
]*)
end


datatype t = 
   Split of {t: Type.t, covered: t list ref}
 | Unsplit of Type.t


(*[ val singleton: Term.term_t -> t ]*)
(*[ val singletonList: Term.term_t list -> t list ]*)
fun singleton term = Unsplit Type.nat

and singletonList terms = 
  (map (*[ <: (Term.term_t -> t) -> Term.term_t conslist -> t conslist
            & (Term.term_t -> t) -> Term.term_t list -> t list ]*))
      singleton terms
