 signature S = sig  
            datatype 'a nat = z | s of 'a nat
        end;

  structure A = 
  struct

        datatype 'a nat = z | s of 'a nat
  end :> S

  