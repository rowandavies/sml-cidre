
signature S2 = sig

   type 'a d
   (*[ sortdef '+a d2 < d ]*)
   (*[ subsort d < d2 ]*)
   (*[ sortdef '+a srt2 < d ]*)

   (*[ abc <: 'a srt2 ]*)
   val abc :  'a d

end

structure ST = struct (*[ assumesig S2 ]*) end


type 'a x = 'a ST.d
type 'a x = 'a ST.d2         (* reports a type.  I"m guessing because the sort equals the mlTy *)
(*type 'a x = 'a ST.str2*)   (* fails with "unbound type constructor ST.str2." *)
                             (* Argh!   str2 <> srt2  !  Oh dear, a typo...  *)

         (* Turns out this is a feature rather than a bug.  Basically the elaborator needs to 
            interpret the types associated with sort annotations, and it seems reasonable not
            to allow an unrelated type and sort with the same name. So, types are always generated
            with the same name as types. *)

(*[ x <: 'a ST.srt2 ]*)
val x : 'a ST.srt2 = ST.abc
