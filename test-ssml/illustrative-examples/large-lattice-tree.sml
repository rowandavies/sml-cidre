
  datatype dt =                                     
       EM | BL of dt * dt        
          | RD of dt * dt

(*
  (*[ datasort s = EM ]*)

  (*[ datasort sb =   BL of s  * s ]*)
  (*[ datasort sr =   RD of s  * s ]*)

  (*[ datasort sbrr = BL of sr * sr ]*)
  (*[ datasort sbrb = BL of sr * sb ]*)
  (*[ datasort sbbr = BL of sb * sr ]*)
  (*[ datasort sbbb = BL of sb * sb ]*)

  (*[ datasort srrr = RD of sr * sr ]*)
  (*[ datasort srrb = RD of sr * sb ]*)
  (*[ datasort srbr = RD of sb * sr ]*)
  (*[ datasort srbb = RD of sb * sb ]*)
  

  (*[ datasort c0 = BL of s * s | RD of s * s ]*)
  (*[ datasort c1 = BL of sb * sb | BL of sb * sr | BL of sr * sb | BL of sr * sr ]*)
  (*[ datasort n = BL of c * c | RD of c * c ]*)
*)

(* Focus first on EL and BL - since those are closest to nat *)

(*
  (*[ datasort c0 = EM ]*)
  (*[ datasort c1 = BL of c0 * c0 ]*)
  (*[ datasort c2 = BL of c1 * c1 ]*)
  (*[ datasort c3 = BL of c2 * c2 ]*)
  (*[ datasort c4 = BL of c3 * c3 ]*)
  (*[ datasort c5 = BL of c4 * c4 ]*)
  (*[ datasort c6 = BL of c5 * c5 ]*)
  (*[ datasort c7 = BL of c6 * c6 ]*)
*)

(*
  (*[ datasort s0 = EM ]*)
  (*[ datasort s1 = EM | BL of s0 * s0 ]*)
  (*[ datasort s2 = EM | BL of s1 * s1 ]*)
  (*[ datasort s3 = EM | BL of s2 * s2 ]*)
  (*[ datasort s4 = EM | BL of s3 * s3 ]*)

(*[
      datasort s5 = EM | BL of s4 * s4
      datasort s6 = EM | BL of s5 * s5
      datasort s7 = EM | BL of s6 * s6

      datasort n0 = BL of s7 * s7 
      datasort n1 = EM | BL of n0 * n0
      datasort n2 = EM | BL of n1 * n1
      datasort n3 = EM | BL of n2 * n2
      datasort n4 = EM | BL of n2 * n3
      datasort n5 = EM | BL of n2 * n4
      datasort n6 = EM | BL of n2 * n5 ]*)
*)

(*[
(*      datasort nn =      BL of dt * dt

      datasort nl = EM | BL of dt * nn
      datasort nr = EM | BL of nn * dt

      datasort nll = EM | BL of nl * dt
      datasort nlr = EM | BL of nr * dt
      datasort nrl = EM | BL of dt * nl
      datasort nrr = EM | BL of dt * nr


      datasort nlll = EM | BL of nll * dt
      datasort nllr = EM | BL of nlr * dt
      datasort nlrl = EM | BL of nrl * dt  (* Up to here we have 1024 elements... *)
*)
(*
      datasort nlrr = EM | BL of nrr * dt

      datasort nrll = EM | BL of dt * nll
      datasort nrlr = EM | BL of dt * nlr
      datasort nrrl = EM | BL of dt * nrl
      datasort nrrr = EM | BL of dt * nrr
*)

(*  (*Trees with a certain number of nodes*)
      datasort sz0 = EM
      datasort sz1 = BL of sz0 * sz0
      datasort sz2 = BL of sz0 * sz1 | BL of sz1 * sz0
      datasort sz3 = BL of sz0 * sz2 | BL of sz1 * sz1 | BL of sz2 * sz0
      datasort sz4 = BL of sz0 * sz3 | BL of sz1 * sz2 | BL of sz2 * sz1 | BL of sz3 * sz0
      datasort sz4 = BL of sz0 * sz4 | BL of sz1 * sz3 | BL of sz2 * sz2 | BL of sz3 * sz1  
                   | BL of sz4 * sz0
*)


      (* Trees without a certain number of nodes *)
      datasort ns0 = BL of dt * dt
      datasort ns1 = EM | BL of ns0 * dt | BL of dt * ns0
      datasort ns2 = EM | BL of ns1 * dt | BL of dt * ns1 
      datasort ns3 = EM | BL of ns2 * dt | BL of dt * ns2
      datasort ns4 = EM | BL of ns3 * dt | BL of dt * ns3
      datasort ns5 = EM | BL of ns4 * dt | BL of dt * ns4
      datasort ns6 = EM | BL of ns5 * dt | BL of dt * ns5
      datasort ns7 = EM | BL of ns6 * dt | BL of dt * ns6
      datasort ns8 = EM | BL of ns7 * dt | BL of dt * ns7
    

]*)
    
