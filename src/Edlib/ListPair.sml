(*$ListPair: LIST_PAIR *)

structure EdListPair: LIST_PAIR =

(* PAIRS OF LISTS

Created by:     Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:           22 Sep 1989

Maintenance:    Author

RCS LOG

$Log: ListPair.sml,v $
Revision 1.2  2004/10/10 14:56:04  rowan_sorts
In the process of fixing bugs so that the implementation can check
itself.  Also added some small extensions to enable this: if the only
errors found for a declaration are unmatched cases, these are
considered to be only warnings.  Also, added "and type" to sharing
specs (like SML/NJ), and allowed module sharing specs in the case that
some types are transparent (although, no sharing is implied though for
transparent types, unlike SML/NJ, so you need to use "where type" for
those).

Revision 1.1.1.1  1999/04/06 16:46:13  rowan
Imported sources

Revision 1.2  1998/11/11 18:59:15  mael
tuning

Revision 1.1  1998/01/22 17:01:16  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.4  91/01/25  20:17:26  20:17:26  db (Dave Berry)
Changed signature names to all upper case.
Amended tag declarations to match above change.

Revision 1.3  91/01/25  15:44:02  db
Used functions defined in CoreUtils structure where appropriate.

Revision 1.2  91/01/24  17:21:28  17:21:28  db (Dave Berry)
Removed version value.

Revision 1.1  90/12/20  14:58:56  14:58:56  db (Dave Berry)
Initial revision


*)

struct

  exception Zip
  fun zip ([], []) = []
  |   zip (x::xs, y::ys) = (x,y) :: zip (xs, ys)
  |   zip (_, _) = raise Zip

  fun unzip []           = ([] ,[])
  |   unzip ((x,y)::xys) =
        let val (xs, ys) = unzip xys
	in (x :: xs, y :: ys)
	end

  local
     fun from1  ([], l2)    = l2
     |   from1  (x::xs, l2) = x :: from2 (xs, l2)
     and from2  (l1, [])    = l1
     |   from2  (l1, y::ys) = y :: from1 (l1, ys)
  in
     val interleave = from1
  end

  fun unravel []  = ([] ,[])
  |   unravel [x] = ([x],[])
  |   unravel (x::y::xys) =
         let val (xs, ys) = unravel xys in
            (x::xs, y::ys)
         end

  fun merge p (l, []) = l
  |   merge p ([], l) = l
  |   merge p (l1 as (x::xs), l2 as (y::ys)) =
         if p x y then x :: merge p (xs, l2)
                      else y :: merge p (l1, ys)
end
