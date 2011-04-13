
signature S = sig datatype t = C of unit end


functor F (structure s : S) =
struct
   open s
   local   fun f (s.C x) = x
   in
   val x = ()
   end
end