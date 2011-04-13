structure Substring:SUBSTRING =
struct(*[ assumesig SUBSTRING ]*)
    structure String : STRING = String
type substring = unit
    val base : substring -> (String.string * int * int)  = fn _ => raise Match
    val string : substring -> String.string  = fn _ => raise Match
    val extract : (String.string * int * int option) -> substring  = fn _ => raise Match
    val substring : (String.string * int * int) -> substring  = fn _ => raise Match
    val all : String.string -> substring  = fn _ => raise Match
    val isEmpty : substring -> bool  = fn _ => raise Match
    val getc : substring -> (String.Char.char * substring) option  = fn _ => raise Match
    val first : substring -> String.Char.char option  = fn _ => raise Match
    val triml : int -> substring -> substring  = fn _ => raise Match
    val trimr : int -> substring -> substring  = fn _ => raise Match
    val slice : (substring * int * int option) -> substring  = fn _ => raise Match
    val sub : (substring * int) -> char  = fn _ => raise Match
    val size : substring -> int  = fn _ => raise Match
    val concat : substring list -> String.string  = fn _ => raise Match
    val explode : substring -> String.Char.char list  = fn _ => raise Match
    val isPrefix : String.string -> substring -> bool  = fn _ => raise Match
    val compare : (substring * substring) -> order  = fn _ => raise Match
    val collate : ((String.Char.char * String.Char.char) -> order) -> (substring * substring) -> order  = fn _ => raise Match
    val splitl : (String.Char.char -> bool) -> substring -> (substring * substring)  = fn _ => raise Match
    val splitr : (String.Char.char -> bool) -> substring -> (substring * substring)  = fn _ => raise Match
    val splitAt : (substring * int) -> (substring * substring)  = fn _ => raise Match
    val dropl : (String.Char.char -> bool) -> substring -> substring = fn _ => raise Match
    val dropr : (String.Char.char -> bool) -> substring -> substring = fn _ => raise Match
    val takel : (String.Char.char -> bool) -> substring -> substring = fn _ => raise Match
    val taker : (String.Char.char -> bool) -> substring -> substring = fn _ => raise Match
    val position : String.string -> substring -> (substring * substring)  = fn _ => raise Match
    val translate : (String.Char.char -> String.string) -> substring -> String.string  = fn _ => raise Match
    val tokens : (String.Char.char -> bool) -> substring -> substring list  = fn _ => raise Match
    val fields : (String.Char.char -> bool) -> substring -> substring list  = fn _ => raise Match
    val foldl : ((String.Char.char * 'a) -> 'a) -> 'a -> substring -> 'a  = fn _ => raise Match
    val foldr : ((String.Char.char * 'a) -> 'a) -> 'a -> substring -> 'a  = fn _ => raise Match
    val app : (String.Char.char -> unit) -> substring -> unit  = fn _ => raise Match
  end

