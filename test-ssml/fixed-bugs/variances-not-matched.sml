
structure SS = 
struct
   datatype 'a mixed = MM of 'a -> 'a | MM2 of 'a
    (*[ datasort 'a mixed2 = MM2 of 'a ]*)
end :> sig type 'a mixed (*[ sortdef '+a mixed2 <| mixed ]*) end