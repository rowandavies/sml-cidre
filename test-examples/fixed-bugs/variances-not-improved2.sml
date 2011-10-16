
datatype 'a c123 = C1 | C2 | C3

(*[ datasort 'a c12 = C1 | C2 ]*)
(*[ datasort 'a c23 = C2 | C3 ]*)

(*[ datasort '-a c2 = C2 ]*)

datatype 'a d = D of 'a c123

(*[ datasort 'a d12 = D of 'a c12 ]*)

(*[ datasort 'a c1 = C1 ]*)
(*[ datasort 'a d12' = D of 'a c1 | D of 'a c2 ]*)


