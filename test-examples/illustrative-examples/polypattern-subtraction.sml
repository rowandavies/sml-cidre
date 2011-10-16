
(*[ datasort tt = true and  ff = false ]*)

fun myfun [] = false
  | myfun (true :: tail) = true
  | myfun (false :: tail) = false
