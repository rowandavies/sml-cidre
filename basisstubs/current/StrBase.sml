(* Strbase -- internal auxiliaries for String and Substring 
   1995-04-13, 1995-11-06  *)

structure StrBase:STR_BASE =
struct(*[ assumesig STR_BASE ]*)
type substring = string * int * int
    val dropl     : (char -> bool) -> substring -> substring = fn _ => raise Match
    val dropr     : (char -> bool) -> substring -> substring = fn _ => raise Match
    val takel     : (char -> bool) -> substring -> substring = fn _ => raise Match
    val taker     : (char -> bool) -> substring -> substring = fn _ => raise Match
    val splitl    : (char -> bool) -> substring -> substring * substring = fn _ => raise Match
    val splitr    : (char -> bool) -> substring -> substring * substring = fn _ => raise Match
	
    val translate : (char -> string) -> substring -> string = fn _ => raise Match
      
    val tokens    : (char -> bool) -> substring -> substring list = fn _ => raise Match
    val fields    : (char -> bool) -> substring -> substring list = fn _ => raise Match
      
    val foldl     : (char * 'a -> 'a) -> 'a -> substring -> 'a = fn _ => raise Match
    val fromMLescape : ('a -> (char * 'a) option) -> ('a -> (char * 'a) option) = fn _ => raise Match
    val toMLescape   : char -> string = fn _ => raise Match
    val fromCescape  : ('a -> (char * 'a) option) -> ('a -> (char * 'a) option) = fn _ => raise Match
    val toCescape    : char -> string = fn _ => raise Match
    val fromCString  : string -> string option  = fn _ => raise Match

    val explode   : string -> char list = fn _ => raise Match
  end

