
structure S = struct
   datatype t = C1 | C2
   (*[ datasort s = C1 ]*)
end

structure S2 = struct

  (* The following doesn't work, because
     the sorts for values in S doesn't change.  *)

  (*[ datasort s2 = S.C2 ]*)

  val y (*[:> s2 ]*) = S.C2
end
 

structure S3 = struct

  (* Replicating the datatype to here fixes the problem. *)
    
  datatype t = datatype S.t
  (*[ datasort s2 = C2 ]*)

  val y (*[:> s2 ]*) = C2

end
