datatype foo = BAR | BAZ
(*[ datasort bar = BAR ]*)

(*[ val check: unit -> unit ]*)
fun check () =
  (( (case (raise Match (*[ <: bar ]*)) of
             BAR => ()) )
  ; ())

