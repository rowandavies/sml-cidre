
(*[ datasort 'a some = SOME of 'a
         and 'a none = NONE       ]*)

structure NoNeed = 
struct

  datatype myOption = datatype option

  (*[ sortdef 'a mySome = 'a some 
          and 'a myNone = 'a none ]*)

end