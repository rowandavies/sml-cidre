(*$ListSort : LIST_SORT List ListPair General *)

structure ListSort: LIST_SORT =

(* FUNCTIONS FOR SORTING AND PERMUTING LISTS

Created by:	Dave Berry, LFCS, University of Edinburgh
		db@lfcs.ed.ac.uk
Date:	        6 Feb 1991

Maintenance:	Author 


DESCRIPTION

   A straightforward implementation; not always the most efficient.


NOTES

   These functions were priginally in the main List structure.


RCS LOG

$Log: ListSort.sml,v $
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

Revision 1.1  1998/01/22 17:01:17  mael
I have ported the ML Kit to SML/NJ 110.0.2. Use CM.make() to build the system.
Parts of the Edinburgh Library are still used; they are located in the Edlib
directory.

Revision 1.1  91/02/11  20:10:55  20:10:55  db (Dave Berry)
Initial revision


*)

struct


(* MANIPULATORS *)

  fun sort p [] = []
  |   sort p [x] = [x]
  |   sort p [x1,x2] = if p x1 x2 then [x1, x2] else [x2, x1]
  |   sort p l =
        let val (l1, l2) = Edlib.ListPair.unravel l
         in Edlib.ListPair.merge p (sort p l1, sort p l2)
        end

  local
    fun plug a [] = [[a]]
    |   plug a (l as x::xs) =
        (a::l) :: (map (fn l => x :: l) (plug a xs))
  in
    fun perms [] = [[]]
    |   perms (x::xs) =
	  Edlib.List.foldR (EdlibGeneral.curry op @) [] (map (plug x) (perms xs))
  end

end
