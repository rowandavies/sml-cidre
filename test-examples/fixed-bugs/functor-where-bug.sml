functor RefDec(
                structure OG: sig 
		                type info
                                datatype datbind = DATBIND of info
		              end   where type info = unit
               ) =
struct
    val x = OG.DATBIND
end;

