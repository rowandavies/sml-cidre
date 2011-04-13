
structure Mine = struct
  (*[ datasort 'a null = List.nil ]*)

  val x = List.nil
  val y (*[:> 'a null ]*) = List.nil
end