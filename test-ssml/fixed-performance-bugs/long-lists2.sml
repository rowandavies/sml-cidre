

structure LongList = 
struct
  (*[ datasort 'a ev_l = nil | :: of 'a * 'a od_l
           and 'a od_l =       :: of 'a * 'a ev_l  ]*)

   val ll = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]
(*   val ll = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] *)

end 