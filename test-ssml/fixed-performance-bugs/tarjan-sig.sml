
signature STACK =
sig
  type 'a stack

(*[
  sortdef 'a valid < stack
  sortdef 'a nonempty < stack
]*)

  val push : 'a * 'a stack -> 'a stack
  val pop : 'a stack -> 'a * 'a stack

(*[
  val push : 'a * 'a valid -> ('a valid & 'a nonempty)
  val pop : ('a valid & 'a nonempty) -> 'a * 'a valid
]*)

end
