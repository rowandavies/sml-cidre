signature S = sig

   datatype 'a d = D
   (*[ datasort '+a d = D ]*)
(*   (*[ sortdef '+a d |: d ]*)
   (*[ sortdef '+a srt |: d ]*)
*)
end


signature S2 = sig

   type 'a d
   (*[ sortdef '+a d2 < d ]*)
   (*[ subsort d < d2 ]*)
   (*[ sortdef '+a srt2 < d ]*)

end
