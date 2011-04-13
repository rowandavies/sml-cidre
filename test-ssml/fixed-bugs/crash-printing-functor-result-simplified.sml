
functor F () = 
struct
  datatype D = C
end

structure S = F ()

structure S2 = struct type t = S.D end

structure S3 = F ()

structure S4 = struct type t = S3.D val c = S3.C  end

