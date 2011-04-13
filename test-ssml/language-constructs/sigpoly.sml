 signature S = sig  
            val f : 'a -> 'a
        (*[ val f :> 'a -> 'a ]*)
        end;

  structure A = 
  struct

        fun f x = x
        
  end : S

  