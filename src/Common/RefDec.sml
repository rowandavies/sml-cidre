functor RefDec( 

               (* Lab needed for the equality attribute. *)
                structure Lab: LAB
                structure Ident: IDENT
                structure TyVar: TYVAR
                structure TyCon: TYCON
                structure TyName: TYNAME
                      where type tycon = TyCon.tycon

                structure SortName: SORTNAME
                      where type TyName  = TyName.TyName
                      where type Variance = TyVar.Variance
                      where type sortcon = TyCon.tycon

                structure FinMap : FINMAP
                structure SortedFinMap : SORTED_FINMAP
                structure FinMapEq : FINMAPEQ
                structure Report: REPORT
                structure PP: PRETTYPRINT
                      where type Report = Report.Report

(*                  sharing type StatObject.StringTree
                               = MapDecEtoR.IG.StringTree
                               = RefInfo.ElabInfo.StringTree
                               = RefObject.StringTree
                               = MapDecEtoR.OG.StringTree
                               = RefinedEnvironments.StringTree
                               = PP.StringTree 
*)



                structure StatObject: STATOBJECT
                      where type TyName = TyName.TyName
                      where type ExplicitTyVar  = TyVar.SyntaxTyVar
                      where type lab   = Lab.lab

                structure RefObject: REFOBJECT
                      where type ExplicitTyVar.Variance = TyVar.Variance
                      where type SortName = SortName.SortName                    
                      where type scon  = StatObject.scon
                      where type lab   = Lab.lab

                      where type TyVar = StatObject.TyVar
                      where type TypeFcn = StatObject.TypeFcn
                      where type TyName = StatObject.TyName
                      where type Type = StatObject.Type
                      where type ('a, 'b) sortedFinMap = ('a, 'b) SortedFinMap.map


                structure RefinedEnvironments: REFINED_ENVIRONMENTS
                      where type TypeFcn = StatObject.TypeFcn
                      where type TyName = StatObject.TyName
                      where type Type = StatObject.Type

                      where type SortName = SortName.SortName
                      where type SortFcn = RefObject.SortFcn
                      where type SortScheme = RefObject.SortScheme
                      where type SortVar = RefObject.SortVar
                      where type Sort = RefObject.Sort

                      where type id = Ident.id
                      where type longid = Ident.longid
                      where type tycon = TyCon.tycon
                      where type longtycon = TyCon.longtycon
                      where type Report = Report.Report

                      where type ExplicitTyVar = TyVar.SyntaxTyVar

                structure RefineErrorInfo : REFINE_ERROR_INFO
                     where type Sort  = RefObject.Sort
                     where type SortScheme  = RefObject.SortScheme
                     where type longsortcon = TyCon.longtycon
                     where type longid = Ident.longid
                     where type Type = StatObject.Type
                     (* where type Error = Comp.Error *)

                structure Comp: COMP 
                     where type Error = RefineErrorInfo.Error

                structure RefInfo : REF_INFO  
                     where type Comp.Error = RefineErrorInfo.Error
                     where type 'a Comp.Redo = 'a Comp.Redo
                     where type 'a Comp.Memo = 'a Comp.Memo

                     (* where Comp = Comp *)

                     where type REnv.VarEnv = RefinedEnvironments.VarEnv
                     where type REnv.TyNameEnv = RefinedEnvironments.TyNameEnv
                     where type REnv.Env = RefinedEnvironments.Env
                     where type REnv.Sort = RefObject.Sort 

                     where type ElabInfo.TypeInfo.longid = Ident.longid
                     where type ElabInfo.TypeInfo.Type    = StatObject.Type
                     where type ElabInfo.TypeInfo.TyName.TyName  = StatObject.TyName
                     where type ElabInfo.TypeInfo.TyVar   = StatObject.TyVar
                     where type ElabInfo.TypeInfo.ExplicitTyVarEnv 
                                = RefinedEnvironments.ExplicitTyVarEnv
                     where type ElabInfo.ParseInfo.SourceInfo = RefineErrorInfo.SourceInfo

                structure MapDecEtoR: MAP_DEC_INFO 
                    where type OG.info = RefInfo.RefInfo
                    where type IG.info = RefInfo.ElabInfo.ElabInfo

                    where type IG.lab = Lab.lab
                    where type IG.id = Ident.id
                    where type IG.longid = Ident.longid
                    where type IG.tyvar = TyVar.SyntaxTyVar
                    where type IG.tycon = TyCon.tycon
                    where type IG.longtycon = TyCon.longtycon
                    where type IG.scon = StatObject.scon
                    where type IG.longstrid = RefinedEnvironments.longstrid


(*                    where type IG.TyVar.SyntaxTyVar = RefinedEnvironments.ExplicitTyVar    (* Implied by MAP_DEC_INFO *)

                    where type OG.TyVar.tyvar = RefinedEnvironments.ExplicitTyVar    (* Implied by MAP_DEC_INFO *)
*)
                    
                structure MapDecRtoE: MAP_DEC_INFO 
                      where type IG.dec = MapDecEtoR.OG.dec
                      where type OG.dec = MapDecEtoR.IG.dec
                      where type IG.longstrid = MapDecEtoR.OG.longstrid
                      (* where type OG.longstrid = MapDecEtoR.IG.longstrid *)
                      where type IG.info = MapDecEtoR.OG.info
                      where type OG.info = MapDecEtoR.IG.info
                  



                structure Flags: FLAGS
                structure Crash: CRASH
               ) : REFDEC =
let
in    
  struct

    (*** Local abbrev's.  SML/NJ error messages were painful with them in the "let" above. ***)    
    structure ElabInfo = RefInfo.ElabInfo
    structure TypeInfo = ElabInfo.TypeInfo
          and SourceInfo = ElabInfo.ParseInfo.SourceInfo
          and RO = RefObject 
          and REI = RefineErrorInfo
          and Type = StatObject.Type
    type id = Ident.id
    type longid = Ident.longid
    type SourceInfo = REI.SourceInfo
    open Comp (* computations *)
    type Error = SourceInfo * REI.ErrorInfo

    structure IG = MapDecEtoR.IG  (* AST without memotables in info nodes *)
    structure RG = MapDecEtoR.OG  (* AST with memotables in info nodes *)

    structure Comp = Comp
    structure Env = RefinedEnvironments
    type Env = Env.Env
    type Context = Env.Context
    type TyNameEnv = Env.TyNameEnv
    type Sort = RefObject.Sort
    type 'a Comp = 'a Comp.Comp
    type 'a RComp = 'a Comp.RComp

    type PostElabDec  = IG.dec
     and PostElabTy = IG.ty
     and PostElabDatBind = IG.datbind

    type TyGoals = Env.TyGoals

    structure ListHacks =  (* Copied here for inlining, I guess - from ML Kit v1.5 *)
      struct
        fun member x [] = false
          | member x (y::ys) = x=y orelse member x ys
        fun union(set1, set2) =
          set1 @ List.filter (fn x => not(member x set1)) set2
        fun intersect(set1, set2) =
          List.filter (fn x => member x set1) set2
        fun minus(set1, set2) =
          List.filter (fn x => not(member x set2)) set1
       local
         fun rev [] x = x
           | rev (h::t) x = rev t (h::x)
       in
         fun reverse l = rev l []
       end
       fun addMax leq (x1, xs2) =  (* add an element to a list of maximal elements wrt leq *)
         if (List.exists (fn x2 => leq(x1, x2)) xs2) then 
           xs2
         else 
           x1::(List.filter (fn x2 => not (leq(x2, x1))) xs2)
       fun flatten l = List.foldl (fn (x,acc) => x @ acc) [] l
    end

    fun removeLast [] = raise List.Empty
      | removeLast [x] = (x, [])
      | removeLast (h::t) = let val (x, t2) = removeLast t
                            in (x, h::t2)
                            end

    fun foldr' f [] = raise List.Empty
      | foldr' f l  =
        let val (last, front) = removeLast l
        in List.foldr f last front
        end
    
    fun foldl' f [] = raise List.Empty
      | foldl' f (h::t) = List.foldl f h t

    fun default f (SOME x) = x
      | default f NONE = f()


    (* Raise an exception for debugging.  *)
    fun impossible s = Crash.impossible ("RefDec." ^ s)

    (* Printing of debugging messages *)
    val debug_indent = ref 0
    fun nspaces 0 = ""
      | nspaces n = " " ^ (nspaces (n-1))

    fun add_indent n =  if !Flags.DEBUG_REFDEC then (debug_indent := !debug_indent + n)
                        else ()

    fun pr_indent str =  TextIO.output(TextIO.stdErr,(nspaces (!debug_indent)) ^ str ^"\n")
    fun out_debug str = if !Flags.DEBUG_REFDEC then 
                           pr_indent (str())
                        else ()

    fun pr (msg : string, t : StringTree.t) : unit =
          Report.print' (Report.decorate (msg, PP.reportStringTree t)) TextIO.stdErr

    fun pr_debug (msg, t) = if !Flags.DEBUG_REFDEC then pr (nspaces (!debug_indent) ^ msg, t () )
                            else ()

    fun lines_pp st = PP.lines_minipage (PP.format (100, st))

    val memoizeOn = ref true
    val () = Flags.add_flag ("memoizeOn", "Memoize results during sort checking?", memoizeOn)


    (* For debugging purposes.  *)
    val assert = Env.assert

    (* For use with assert. *)
    fun eqTypes str ty1 ty2 () =  (* Use strings, since Type.eq is tied to unification. *)
        let val flag = Flags.lookup_flag_entry "print_type_name_stamps"
            val keep = !flag
            val () = flag := true
            val str1 = PP.flatten1 (Type.layout ty1)
            val str2 = PP.flatten1 (Type.layout ty2)
            val () = flag := false
        in
          if str1 = str2 (* Type.eq(ty1, ty2) *) then NONE
          else (Flags.lookup_flag_entry "print_type_name_stamps" := true;
                SOME [str ^ "types not equal: \n" ^ PP.flatten1 (Type.layout ty1) ^
                          "\nis not equal to: \n" ^ PP.flatten1 (Type.layout ty2)])
        end

    (* For use with assert. *)
    fun compatSorts str srt1 srt2 () = eqTypes str (RO.tyOfSort srt1) (RO.tyOfSort srt2) ()
  
    val to_ElabInfo = RefInfo.to_ElabInfo

    (* Added this while trying to track down a bug while sort checking this file. *)
    (*[ val getPostElabTypeInfo :> ElabInfo.ElabInfo * RefInfo.RefDecMemo ref 
                                    -> ElabInfo.TypeInfo option ]*)
    fun getPostElabTypeInfo i = ElabInfo.to_TypeInfo (to_ElabInfo i)

    (* Get SourceInfo from AST annotations *)
    (* val to_SourceInfo : RG.info -> SourceInfo *)
    fun to_SourceInfo (i: RG.info) = 
       ElabInfo.ParseInfo.to_SourceInfo (ElabInfo.to_ParseInfo (#1 i))

    (* Fail with error details.  *)
    (* val error : 'a * RG.info * REI.ErrorInfo -> 'a Comp *)
    fun error (x, i, e) errflag =
       Comp.failV (x, [(to_SourceInfo i, e)] : Error list) errflag

    fun sortvar_to_sv sortvar =
      RO.SVofTV
        ((StatObject.TyVar.from_ExplicitTyVar sortvar) : RO.TyVar)

    val bogus_sort = RO.bogusSort
    val bogus_sortsch = RO.Sort_in_SortScheme bogus_sort

    (* convert TyName to default SortName - needed for subSort, etc. *)
    (*[ val TNtoSN : Env.Context -> TyName.TyName -> SortName.SortName ]*)
    fun TNtoSN C tyname = Env.mlSN_of_TyStr (case Env.lookupT(Env.T_of_C C, tyname)
                                               of SOME tystr => tystr
                                                | NONE => Crash.impossible "RefDec.TNtoSN")

    val TVtoSort = RO.mkSortSortVar o RO.SVofTV

    fun calc_variance (TyVar.COVARIANT, covar, contra) = (covar, contra)
      | calc_variance (TyVar.CONTRAVARIANT, covar, contra) = (contra, covar)
      | calc_variance (TyVar.IGNORED, covar, contra) = (true, true)
      | calc_variance (TyVar.MIXED, covar, contra) = 
        (covar andalso contra, covar andalso contra)

    (*[ covariant_sortname :> Env.Context -> SortName.SortName -> bool ]*)
    fun covariant_sortname C sortname =
        case Env.Lookup_tyname (C, SortName.tyname sortname)
           of SOME tystr => Env.covariant_of_TyStr tystr
            | NONE => Crash.impossible "covariant_sortname"

    fun findMLSort(C, i) = (* find the ML Sort corresponding to the info type *)
      let                  (* Could memoize in i *)
        val Type = 
          case (getPostElabTypeInfo i)
            of SOME (TypeInfo.EXP_INFO {Type}) => Type
             | SOME (TypeInfo.PLAINvalbind_INFO{tyvars, Type, Uenv}) => Type   
             | SOME (TypeInfo.VAR_PAT_INFO {Type, ...}) => Type
             | SOME (TypeInfo.MATCH_INFO {Type, ...}) => Type
             | _ => Crash.impossible "RefDec.findMLSort(2)"
          val _ = pr_debug ("findMLSort: ", fn () => StatObject.Type.layout Type)
          val res = RO.MLSortOfTy (TNtoSN C) Type
          val () = assert (eqTypes "findMLSort: " (RO.tyOfSort res) Type)
          val _ = pr_debug ("findMLSort: ", fn () => RO.layoutSort res)
      in
          res
      end

    exception TooMuch
    val howMuchIsTooMuch = ref 50000

    val () = Flags.add_int_to_menu (["Extensions"], "maxInstantiation", 
                                    "Maximum size of instances before using default instead?", howMuchIsTooMuch)

    (* Instance of a sort-scheme for given types with all conjuncts. May throw TooMuch. *)
    fun full_instance (C, sscheme, ty_instances) =
      let
        val howMuch = ref 0
        fun oneMore() = (howMuch := !howMuch + 1; if !howMuch > (!howMuchIsTooMuch) then (raise TooMuch) else ()) 
        val sort_instanceLists = map (Env.allRefinements C) ty_instances
        fun mkconj (srt1, srt2) = (oneMore();
                                   RO.mkSortConj (Env.conjSortNameT' (Env.T_of_C C)) (srt1, srt2) )

        (* conjoin all instantiations of sscheme - accumulator reverses order *)
        fun conjoin_product ([], acc) = RO.instance(sscheme, acc)
          | conjoin_product (h::t, acc) = 
                (foldl' (fn (x,b) => mkconj (x, b))
                        (map (fn x => conjoin_product (t, x::acc)) h) )

        val _ = pr_debug ("full_instance: ", fn () => RO.layoutSortScheme sscheme)
      in
        conjoin_product(ListHacks.reverse sort_instanceLists, [])
      end

    fun full_or_specified_inst (C, sscheme, srt_instances, ty_instances_opt) =
        let
           val ty_instances = case ty_instances_opt of SOME x => x | NONE => map RO.tyOfSort srt_instances
           val unique = not (List.exists (Env.hasMultRefments C) ty_instances)
        in
                    (full_instance (C, sscheme, ty_instances), false)
                    handle TooMuch => (RO.instance (sscheme, srt_instances), true)  (* true => warning *)

(*         if unique orelse List.all (RO.covariant_sort (covariant_sortname C)) srt_instances then
                    (full_instance (C, sscheme, ty_instances), false)
           else     (* Perhaps issue a warning.  Maybe disable this with a flag.  *)
                (RO.instance (sscheme, srt_instances), true)  (* true => warning *)
*)
        end

    fun full_or_default_inst (C, sscheme, ty_instances) =
        full_or_specified_inst(C, sscheme,  map (RO.MLSortOfTy (TNtoSN C)) ty_instances, SOME ty_instances)


    val dec_toRG = MapDecEtoR.map_dec_info RefInfo.from_ElabInfo 
    val ty_toIG = MapDecRtoE.map_ty_info to_ElabInfo   (* tygoals use IG (avoids cycle) *)
    val ty_toRG = MapDecEtoR.map_ty_info RefInfo.from_ElabInfo

    (* A local type to keep track of residual sort information in patterns *)
    datatype PatSort = SORTps of RO.Sort
                       (* Constr. with original sorts, sortsch, instances, and optional argument. *)
                     | VALCONSps of Ident.longid * (RO.Sort list * RO.SortScheme * RO.Sort list) 
                                    * PatSort option
                   (*  | REFps of RO.Sort * PatSort     (* ref requires special treatment *) *)
                     | ADDFIELDps of RG.lab * PatSort * PatSort
                     | UNITps
                     | UNIONps of PatSort * PatSort
                     | EMPTYps

    (* These implement some basic, but critical optimizations.  *)
    fun mkVALCONSps(_, _, SOME EMPTYps) = EMPTYps
      | mkVALCONSps x = VALCONSps x
(*    fun mkREFps (srt, EMPTYps) = EMPTYps  (* empty implies unreachable case, even for ref *)
      | mkREFps x = x *)
    fun mkADDFIELDps(_, EMPTYps, _) = EMPTYps
      | mkADDFIELDps(_, _, EMPTYps) = EMPTYps
      | mkADDFIELDps x = ADDFIELDps x
    fun mkUNIONps(EMPTYps, ps) = ps
      | mkUNIONps(ps, EMPTYps) = ps
      | mkUNIONps x = UNIONps x

    fun mkSORTps C srt = if RO.emptySort (Env.conjSortNameC C) srt then EMPTYps
                         else SORTps srt


(*  Use these to see the dramatic effect of the above optimization. *)
(*  Somehow this causes the rbt example to fail with the weaker rule for product subtraction. *)
(*  There must be a bug somewhere, but I can't find it.  Most likely it only occurs when there
    are extra EMPTYps around... (hopefully) *)
(*    fun mkVALCONSps x = VALCONSps x
    fun mkADDFIELDps x = ADDFIELDps x
    fun mkUNIONps x = UNIONps x
*)

    fun pr_PatSort (SORTps srt) = "SORTps(" ^ (RO.pr_Sort srt) ^ ")"
      | pr_PatSort (VALCONSps (longid, _, NONE)) = Ident.pr_longid longid
      | pr_PatSort (VALCONSps (longid, _, SOME ps)) = 
        (Ident.pr_longid longid) ^ "(" ^ pr_PatSort ps ^ ")"
(*      | pr_PatSort (REFps (srt, ps)) =
          REF ^ "(" ^ RO.pr_Sort srt ^ ", " ^  pr_PatSort ps ^ ")" *)
      | pr_PatSort (ADDFIELDps(lab, ps1, ps2)) = 
        "FIELD(" ^ Lab.pr_Lab lab ^ "=" ^ (pr_PatSort ps1) ^ ", " ^ (pr_PatSort ps2) ^ ")"
      | pr_PatSort UNITps = "UNIT"
      | pr_PatSort (UNIONps(ps1, ps2)) = 
        "UNION(" ^ (pr_PatSort ps1) ^ ", " ^ (pr_PatSort ps2) ^ ")"
      | pr_PatSort EMPTYps = "EMPTY"    

    fun isEmptyPS C (SORTps srt) = RO.emptySort (Env.conjSortNameC C) srt
      | isEmptyPS C (VALCONSps (longid, _, NONE)) = false
      | isEmptyPS C (VALCONSps (longid, _, SOME ps)) = isEmptyPS C ps
(*      | isEmptyPS C (REFps (_, ps)) = isEmptyPS C ps *)
      | isEmptyPS C (ADDFIELDps (lab, ps1, ps2)) = isEmptyPS C ps1 orelse isEmptyPS C ps2
      | isEmptyPS C UNITps = false
      | isEmptyPS C (UNIONps (ps1, ps2)) = isEmptyPS C ps1 andalso isEmptyPS C ps2
      | isEmptyPS C EMPTYps = true

    (* Used for performance testing *)
    fun sizePS (SORTps _) = 1
      | sizePS (VALCONSps (_, _, NONE)) = 1
      | sizePS (VALCONSps (_, _, SOME ps)) = 1 + sizePS ps
(*      | sizePS (REFps (_, ps)) = 1 + sizePS ps *)
      | sizePS (ADDFIELDps (lab, ps1, ps2)) = 1 + sizePS ps1 + sizePS ps2
      | sizePS UNITps = 1
      | sizePS (UNIONps (ps1, ps2)) =  1 + sizePS ps1 + sizePS ps2
      | sizePS EMPTYps = 1

    val largestPS = ref 0

    (***** patSorts, to support ref_pat *****)
    fun patSort_to_Sorts (C, SORTps srt) : RO.Sort list * bool =   (* bool => warn *)
        if (RO.emptySort (Env.conjSortNameC C) srt)
           andalso not (RO.isBogusSort srt) 
        then ([],false) else ([srt],false)
      | patSort_to_Sorts (C, ps as VALCONSps (longid, (srts, ssch, instances), patsort_opt)) =
        if longid = Ident.idToLongId Ident.id_REF then (* Unsound without this.  *)
           if isEmptyPS C ps then ([],false) else (srts,false)
        else
          let  (* The default sort won't work here, we need the original instances instead *)
            val (srt1, warn1) = full_or_specified_inst (*full_instance*) (C, ssch, instances, NONE)
          in
           (case patsort_opt
              of NONE => ([srt1], warn1)
               | SOME patsort =>
                 let val (p2srts, warn2) = (patSort_to_Sorts (C, patsort)) in
                 (List.foldr (fn (srt2,srts) =>
                                case RO.applySort (Env.conjSortNameC C) (srt1, srt2)
                                  of NONE => Crash.impossible ("RefDec.patSort_to_Sorts(2): \n" ^
                                             "applying:  "^  RO.pr_Sort srt1 ^ 
                                             "\n    to:  " ^ RO.pr_Sort srt2 ^
                                             "\npatSort: " ^ (pr_PatSort patsort))
                                   | SOME srt3 => 
                                     RO.addSortToMaximals (Env.conjSortNameC C) (srt3, srts))
                            [] p2srts
                  , warn1 orelse warn2)
                 end)
          end

(*      | patSort_to_Sorts (C, REFps (srt, ps)) = 
          if emptyPS ps (* orelse RO.emptySort (Env.conjSortNameC C) srt *) then
              [] (* Unreachable case *)
          else
              srt *)

      | patSort_to_Sorts (C, ADDFIELDps(lab1, patsort1, patsort2)) =
        let
          val (srts1, warn1) = patSort_to_Sorts (C, patsort1)
          val (srts2, warn2) = patSort_to_Sorts (C, patsort2)
          fun addsrts1 (srt, acc) = 
            case (RO.unSortRecSort srt) 
              of SOME recsrt => 
                 (map (fn srt1 => RO.mkSortRecSort(RO.addField(lab1, srt1) recsrt)) srts1)
                 @ acc
               | NONE => Crash.impossible "RefDec.patSort_to_Sorts(3)"
        in
          (List.foldl addsrts1 [] srts2, warn1 orelse warn2)
        end
      | patSort_to_Sorts (C, UNITps) = ([RO.SortUnit], false)
      | patSort_to_Sorts (C, UNIONps (patsort1, patsort2)) = 
        let val (p2sorts1, warn1) =  patSort_to_Sorts (C, patsort1)
            val (p2sorts2, warn2) =  patSort_to_Sorts (C, patsort2)
        in 
          (List.foldr (fn (srt1,srts2) =>
                          RO.addSortToMaximals (Env.conjSortNameC C) (srt1, srts2))
                      p2sorts2
                      p2sorts1
          , warn1 orelse warn2)
        end
      | patSort_to_Sorts (C, EMPTYps) = ([], false)

    (* Divide a patsort on a constructor to get residual and (optional) argument patsorts,
       plus a sort containing the pattern sort (used for references).
       Argument patsort will be "SOME (empty)" if no match, even if no argument.  *)
    and dividePatsort (C : Env.Context, ps : PatSort, longid : longid) 
          : PatSort * PatSort option * Sort list * Sort list =
     case ps    
     of SORTps srt =>  (* ref is not allowed - it should be handled differently *)
        (case Env.Lookup_longid (C, longid)
         of NONE => impossible "dividePatSort(-1)"
          | SOME (Env.LONGVAR _) => impossible "dividePatsort(0)" 
          | SOME (Env.LONGEXCON srt') =>
            let
              val (out_patsort_opt, out_srt) = 
                  case (RO.unSortArrow srt') 
                   of NONE => (NONE, srt')
                    | SOME (srt1, srt2) => (SOME (mkSORTps C srt1), srt2)
              in
                (ps, out_patsort_opt, [out_srt],[])  (* ps should always be SortExn *)
              end
           | SOME (Env.LONGCON con_srtsch) =>  (* Expand "srt" using the INV. PRINC. in "RC".  *)
             case (RO.unSortCons (RO.firstConjunct srt))   (* Many for non-covariant datasorts. *)
               of NONE => (EMPTYps, SOME EMPTYps, [], [])   (* What about a BogusSort?  *)
                | SOME (srts_1st, sortname) =>  (* All conjuncts have the same sortname, *)
                   let                          (* but possibly different sort arguments.       *)
                     val srts_list =   (* List of the sort arguments in each conjunct. *)
                       map (fn cnjct => case RO.unSortCons cnjct
                                          of SOME (srts, _) => srts
                                           | NONE => Crash.impossible "RefDec.dividePatsort(0.5)")
                           (RO.list_Conjuncts srt)  (* Covariant sortnames will have one conjnct *)
                     val (path, con) = Ident.decompose longid                     
                     val SOME tystr = Env.lookupT (Env.T_of_C C, SortName.tyname sortname)
                     val SOME RC = Env.lookupR(Env.R_of_TyStr tystr, sortname) 
                     val CE = Env.CE_of_TyStr tystr
                     val match_sfs_opt = case Env.lookupRC(RC, con) (* sortfcns that match "con" *)
                                           of NONE => SOME []
                                            | SOME sfs_opt => sfs_opt
                     val mkSortConj = RO.mkSortConj (Env.conjSortNameT' (Env.T_of_C C))
                     fun srtsToConj cnj_srts = foldl' mkSortConj cnj_srts
                     (* Apply sf to each in srts_list, then conjoin.  This substitutes "sf" *)
                     fun conjoin_apply sf =                    (*  for "sortname" in "srt". *) 
                           srtsToConj (map (fn srts' => RO.applySortFcn(sf, srts')) srts_list)
                     fun sfsopt_to_psopt NONE = NONE
                       | sfsopt_to_psopt (SOME sfs) = 
                         SOME (List.foldl (fn (sf, acc_ps) =>  (* Fold over sfs, build a PatSort *)
                                             mkUNIONps(mkSORTps C(conjoin_apply sf), acc_ps))
                                          EMPTYps
                                          sfs)
                     fun con_to_srt con = case Env.lookupCE (CE, con) of SOME srt => srt
                     val patsort_opt = sfsopt_to_psopt match_sfs_opt  (* Argument Patsort *)
                     val patsort = 
                       Env.RCFold   (* fold over RC, building residual PatSort *)
                         (fn ((con2, sfsopt2), acc_ps) => 
                            if (con2 = con) then acc_ps
                            else mkUNIONps(mkVALCONSps(Ident.implode_LongId(path, con2), 
                                                       ([srt], con_to_srt con2, 
                                                        srts_1st),
                                                       sfsopt_to_psopt sfsopt2), 
                                           acc_ps))
                         EMPTYps
                         RC
                   in 
                      (patsort, patsort_opt, [srt], srts_1st)
                   end
        )   (* Of "(case Env.Lookup_longid ..." inside the case for "SORTps srt" *)
      | VALCONSps(longid1, (srts, _, srtInstances), patsort_opt) =>
        if (longid1 = longid) then (EMPTYps, patsort_opt, srts, srtInstances) else (ps, SOME EMPTYps, srts, srtInstances)
      | UNIONps(ps1, ps2) =>
        let
          val (patsort1, patsort1_opt, srts1, instances1) = dividePatsort(C, ps1, longid)
          val (patsort2, patsort2_opt, srts2, instances2) = dividePatsort(C, ps2, longid)
          val instances = case instances1 of [] => instances2 | _ => instances1
          val patsort = mkUNIONps(patsort1, patsort2)
          val patsort_opt = case (patsort1_opt, patsort2_opt)
                              of (SOME ps1', SOME ps2') => SOME(mkUNIONps(ps1', ps2'))
                               | _ => NONE
          val srts = (if isEmptyPS C ps1 then [] else srts1) @ 
                     (if isEmptyPS C ps2 then [] else srts2)
        in
          (patsort, patsort_opt, srts, instances)
        end
      | EMPTYps => (EMPTYps, SOME EMPTYps, [], []) (* Sort and instances are never used. *)
      | _ => Crash.impossible "RefDec.dividePatsort"

    (* Project a pattern-sort on a label, to get a union represented as a list of pairs of patsorts
       with one for the label (with no new unions) and one for the corresponding remaining fields. *)
    and projectPatsort (C : Env.Context, ps : PatSort, lab: RG.lab) 
          : (PatSort * PatSort) list =
     case ps 
     of SORTps srt =>
       (case (RO.unSortRecSort srt) 
          of NONE => []   (* Error caught elsewhere *)
           | SOME recsort =>
             let
               val recmap = RO.recSortToMap recsort
               val out_patsort1 = case (SortedFinMap.lookup recmap lab)
                                    of NONE => EMPTYps (* impossible? *)
                                     | SOME srt => mkSORTps C srt
               val recmap' = case (SortedFinMap.remove(lab, recmap))
                               of SOME recmap' => recmap'
                                | NONE => recmap
               val out_patsort2 = 
                 SortedFinMap.Fold (fn ((lb, srt), b) => mkADDFIELDps(lb, mkSORTps C srt, b))
                                   UNITps recmap'
             in
               [(out_patsort1, out_patsort2)]
             end)
      | ADDFIELDps(lab1, ps1, ps2) =>
        if (lab1 = lab) then [(ps1, ps2)]   
        else map (fn (out_ps1, out_ps2) => (out_ps1, mkADDFIELDps(lab1, ps1, out_ps2)))
                 (projectPatsort(C, ps2, lab))
      | UNIONps(ps1, ps2) => (projectPatsort(C, ps1, lab)) @ (projectPatsort(C, ps2, lab))
      | EMPTYps => []
      | _ => Crash.impossible "RefDec.projectPatsort"

    (*********
    Environments to be used initially in refining a datbind
     - we fill in the real CE and "covariance" later.
    *********)

    fun datbind_to_info (RG.DATBIND((i (*[ :> ElabInfo.ElabInfo * RefInfo.RefDecMemo ref ]*)), 
                                             tyvars, tycon, _, datbind_opt)) = i

    (*[ val datatype_initial_T_TE_RE :> RG.datbind -> TyNameEnv * Env.TyEnv * Env.SortEnv ]*)
    fun datatype_initial_T_TE_RE (RG.DATBIND((i (*[ :> ElabInfo.ElabInfo * RefInfo.RefDecMemo ref ]*)), 
                                             tyvars, tycon, _, datbind_opt)) =
        let
          val tyname = case getPostElabTypeInfo i
                         of SOME (TypeInfo.DATBIND_INFO {TyName}) => TyName
                          | _ => Crash.impossible "RefDec.ref_datbind"
          val sortcon = tycon
          val sortname = 
            SortName.freshSortName 
              {conjuncts=[sortcon], 
               variance= map TyVar.variance tyvars, 
               tyname=tyname}
          val sortFcn =
            RO.SortName_in_SortFcn sortname
          val RL = Env.singleRL sortname
          val tystr =
            Env.mkTyStr(Env.emptyCE, Env.emptyR, sortname, true, RL)
          val T = Env.singleT(tyname, tystr)
          val TE = Env.singleTE(tycon, sortFcn)
          val RE = Env.singleRE(sortcon, sortFcn)
        in
          case datbind_opt 
            of NONE => (T, TE, RE)
             | SOME datbind => 
                 let
                   val (T', TE', RE') = datatype_initial_T_TE_RE datbind
                 in
                  (Env.T_plus_T(T, T'), Env.TE_plus_TE(TE, TE'), 
                   Env.RE_plus_RE(RE, RE'))
                 end
        end

    fun datasort_initial_RE (C, RG.DATBIND(i, sortvars, sortcon, 
                                               consortbind, datsortbind_opt)) =
      let
        val RG.CONBIND(i, RG.OP_OPT(longid, _), _, _) = consortbind
        val RE = 
          case Env.Lookup_longid(C, longid)
            of SOME (Env.LONGCON sortscheme) =>
               let
                 val (svs, sort) = RO.instance_vars sortscheme
                 val conjunct = RO.firstConjunct sort
                 val sort_cons =
                   case RO.unSortArrow conjunct
                     of SOME (_, sort2) => sort2 | NONE => conjunct
                 val (svs', sortname) = 
                   case RO.unSortCons sort_cons
                     of SOME x => x
                      | NONE => Crash.impossible "RefDec.datasort_initial_RE(1)"
                 val tyname = SortName.tyname sortname
                 val variance = case (List.length sortvars) = (List.length svs')
                                  of true => map TyVar.variance sortvars
                                   | false => map (fn _ => TyVar.MIXED) svs'  (* impossible? *)
                 val new_sortname = SortName.freshSortName 
                                     {conjuncts = [sortcon], 
                                      variance = variance, 
                                      tyname = tyname}
               in
                 Env.singleRE(sortcon, RO.SortName_in_SortFcn new_sortname)
               end
             | _ => (* impossible? *)
               let            (* Return a dummy value, generate an error later. *)
                 val tyname = TyName.freshTyName
                                 {tycon= sortcon,
                                  arity=List.length sortvars,
                                  equality=false}
                 val sortname = SortName.freshSortName
                                 {conjuncts = [sortcon], 
                                  variance = map TyVar.variance sortvars, 
                                  tyname = tyname}
               in              
                 Env.singleRE (sortcon, RO.SortName_in_SortFcn sortname)
               end
      in
          case datsortbind_opt
            of NONE => RE
             | SOME datsortbind => 
                let
                  val RE' = datasort_initial_RE (C, datsortbind)
                in
                  Env.RE_plus_RE (RE, RE')
                end
      end

      fun dec_depends_on_VE (RG.DATATYPEdec _) = false
        | dec_depends_on_VE (RG.DATASORTdec _) = false
        | dec_depends_on_VE _ = true

(*    fun dec_depends_on_VE (RG.VALdec _) = true
        | dec_depends_on_VE (RG.LOCALdec(i, dec1, dec2)) = 
            (dec_depends_on_VE dec1) orelse (dec_depends_on_VE dec2)
        | dec_depends_on_VE (RG.SEQdec(I, dec1, dec2)) = 
            (dec_depends_on_VE dec1) orelse (dec_depends_on_VE dec2) *)

    (* internal declaration refinement. *)
(*    (*[ val ref_decR : Context * RG.dec -> (TyNameEnv * Env) RComp ]*)
    (*[ val ref_dec_memo : Context * RG.dec -> (TyNameEnv * Env) RComp ]*)
    (*[ val ref_dec_internal : Context * RG.dec -> (TyNameEnv * Env) RComp ]*) *)
    fun ref_dec_internal(C, dec) errflag = 
     let val () = Env.debug_push (fn () => ["ref_dec_internal:",PP.flatten1 (RG.layoutDec dec)])
         val res =  

     case dec of
       RG.DATATYPEdec(i, datbind) =>
          let
            val (T, TE, RE) = datatype_initial_T_TE_RE datbind
            val E = Env.E_plus_E (Env.TE_in_E TE, Env.RE_in_E RE)
          in
            letCV (ref_datbind (Env.C_plus_C (C, Env.mkC(T, E)), datbind, true))
                  (fn (outT, outVE, _) => 
            noRedo (outT, Env.E_plus_E (E, Env.VE_in_E outVE))  ) errflag
          end
      | RG.DATASORTdec(i, datsortbind) =>
          letCV (ref_datsortbind_completeRG (C, datsortbind))  (fn res => noRedo res) errflag
      | RG.VALdec(i, tyvars, valbind) =>    (* tyvars scoping okay? - Rowan 12jul01 *)
         (Env.debug_push (fn () => ["VALdec",PP.flatten1 (RG.layoutDec dec)]);
          letRV (ref_valbind (C, valbind)) (fn VE => 
          (Env.emptyT, Env.VE_in_E VE)  ) errflag
          before  Env.debug_pop (fn () => ["VALdec(exit)"])
         )

      | RG.VALsdec(i, valsdesc) =>
         ((* Env.debug_push (fn () => lines_pp (RG.layoutDec dec)); *)
          letCV (ref_valsdesc (C, valsdesc))  (fn TG =>
          noRedo (Env.emptyT, Env.TG_in_E TG)  ) errflag
          (*      before  Env.debug_pop (fn () => []) *)
         )
      | RG.EXCEPTIONdec(i, exbind) =>
          letCV (ref_exbind(C, exbind)) (fn EE =>   (* turn into VE - EE not used anymore *)
          noRedo (Env.emptyT, Env.VE_in_E (Env.VE_of_EE EE))  ) errflag
      | RG.TYPEdec(i, typbind) =>
          letCV (ref_typbind(C, typbind)) (fn (TE, RE) =>
          noRedo (Env.emptyT, Env.E_plus_E (Env.TE_in_E TE, Env.RE_in_E RE))  ) errflag
      | RG.SORTdec(i, typbind) =>
          letCV (ref_srtbind(C, typbind)) (fn RE => 
          noRedo (Env.overrideDefaults (Env.T_of_C C) RE, 
                  Env.E_plus_E (Env.RE_in_E RE, Env.TE_in_E (Env.convertREtoTE RE)) )  ) errflag
      | RG.LOCALdec(i, dec1, dec2) =>
          letR (ref_decR (Env.C_erase_TG C, dec1)) (fn (T1, E1) => 
          letRV (ref_decR (Env.C_plus_E (Env.C_plus_T(C,T1), E1), dec2)) (fn (T2, E2) => 
          (Env.T_plus_T(T1, T2), Env.E_erase_TG E2)  )) errflag
      | RG.INFIXdec(i, n_opt, ids) => noErr (noRedo (Env.emptyT, Env.emptyE))
      | RG.INFIXRdec(i, n_opt, ids) => noErr (noRedo (Env.emptyT, Env.emptyE))
      | RG.NONFIXdec(i, ids) => noErr (noRedo (Env.emptyT, Env.emptyE))
      | RG.SEQdec(i, dec1, dec2) => 
         (Env.debug_push (fn () => "SEQdec(1)"::lines_pp (RG.layoutDec dec));
          letR (ref_decR (C, dec1)) (fn (T1, E1) =>
          (Env.debug_push (fn () => "SEQdec(2)"::lines_pp (RG.layoutDec dec));
          letRV (ref_decR (Env.C_plus_T(Env.C_plus_E(C,E1),T1), dec2)) (fn (T2, E2) =>
          (Env.debug_push (fn () => "SEQdec(3)"::lines_pp (RG.layoutDec dec));
          (Env.T_plus_T(T1, T2), Env.E_plus_E(E1, E2))  )) )) errflag
          before  Env.debug_pop (fn () => ["SEQdec(4)"]) )

      | RG.EMPTYdec i =>
          noErr (noRedo (Env.emptyT, Env.emptyE))

      | RG.OPENdec(i, list) =>
             let
               fun process (RG.WITH_INFO(i, longstrid) :: rest) : Env =
                        (case Env.Lookup_lstrid (Env.C_in_B C, longstrid) of
                           SOME E => (Env.E_plus_E (E, process rest))
                         | NONE =>   impossible "ref_decR:OPENdec")
                 | process nil = Env.emptyE
             in                (* emptyT below because tynames are already in C *)
               noErr (noRedo (Env.emptyT, process list))
             end
      | RG.DATATYPE_REPLICATIONdec(i, tycon, longtycon) => 
             (case Env.Lookup_longsortcon(C, longtycon) of
                SOME sortfcn =>
                  let
                    val RE = Env.singleRE (tycon, sortfcn)
                    val TE = Env.singleTE (tycon, sortfcn)
                    val E1 = Env.E_plus_E (Env.RE_in_E RE, Env.TE_in_E TE)
                    val SOME sn = RO.SortFcn_to_SortName sortfcn
                    val tn = SortName.tyname sn
                    val SOME tystr = Env.Lookup_tyname (C, tn)
                    val VE = Env.CE_to_VE (Env.CE_of_TyStr tystr)
                    val T = Env.singleT(tn, tystr)
                  in
                    noErr (noRedo (Env.emptyT, Env.E_plus_E(E1, Env.VE_in_E VE)))
                  end
              | NONE =>  impossible "ref_dec: DATATYPE_REPLICATIONdec")
      | RG.ABSTYPEdec(i, datbind, dec) =>  (* INCOMPLETE - may crash due to wrong tynames *)

          let
            val (T, TE, RE) = datatype_initial_T_TE_RE datbind
            val E = Env.E_plus_E (Env.TE_in_E TE, Env.RE_in_E RE)
          in
            letC (ref_datbind (Env.C_plus_C (C, Env.mkC(T, E)), datbind, true))
                 (fn (T1, VE1, _) =>                
            letRV (ref_decR(Env.C_plus_E (Env.C_plus_VE (Env.C_plus_T(C,T1), VE1), E), dec))
                  (fn (T2, E2) =>
            let
(*               val (eTE, ephi) = case getPostElabTypeInfo i
                                  of SOME (TypeInfo.ABSTYPE_INFO x) => x
                                   | _ => Crash.impossible "RefDec.ref_decR:ABSTYPEdec"
               val ephi_inverse = StatObject.Realisation.inverse ephi
               val Ts1 = Env.Tdom T1
               val Rs1 = map (Env.TNtoSN Env.mkC(T1, Env.emptyE)) Ts1
               val Rset1 = SortName.Set.fromList Rs1

               val (Tset3, rphi_inverse) = RO.Realisation.renaming' ephi_inverse Rset1
               val E3 = Env.Rea.on_Env (Env.conjSortNameC Env.emptyC) rphi_inverse E2
*)
(* NOW: create T3 which is an abstraction from T2: domain is mapped by ephi_inverse,
        mlSN is mapped by rphi_inverse  *)

(** FIX: Need to write rEnv.ABS based on Env.ABS  **)
(*               val (T3,E3,rphi) = Env.ABS (TE, E2) *)
(*               val (T3,E3,phi) = Environments.ABS (TE2, E)  *)
                 (* the realisation returned maps abstract type 
                  * names to type names for the datbind. *)
               val T3 = Env.emptyT (* FIX! Rowan 5mar03 *)
               val E3 = E2 (* FIX! Rowan 5mar03 *)
            in
             (Env.T_plus_T (Env.T_plus_T(T1, T2), T3), Env.E_erase_TG E3)
            end  )) errflag
          end
      | RG.UNRES_FUNdec _ => Crash.impossible "RefDec.ref_decR:UNRES_FUNdec"
      val () = Env.debug_pop (fn()=>[])
     in
         res
     end

    (* Internal, memoizing ref_dec *)
    and ref_dec_memo(C, dec) errflag = 
      let
        open RefInfo     (* for datatype constructors *) 
        val refDecMemo = RefInfo.to_RefDecMemo (RG.get_info_dec dec)
        (* val () = Env.debug_push (fn () => "\n****ref_dec_memo: " :: (lines_pp (RG.layoutDec dec))) *)
        val res = 
             if dec_depends_on_VE dec then
               let val memotable = get_DEPEND refDecMemo (* Will add table if not there. *)
                   val memo = lookupMemoVE (Env.VE_of_E (Env.E_of_C C), memotable)
               in if !memoizeOn then Comp.memoInR memo (ref_dec_internal (C, dec)) errflag
                  else ref_dec_internal (C, dec) errflag
               end
             else
             case !refDecMemo of
                 DEC_NODEPEND memoed => memoed
               | EMPTY => 
                 (let val (res, errs) = ref_dec_internal(C, dec) true  (* memoize with errors always *)
                      val _ =  (refDecMemo := DEC_NODEPEND (res, errs))
                  in
                      case (errflag, errs) of
                          (false, _::_) =>  raise Comp.Fail
                        | _ =>  (res, errs)
                  end)
         (* val () = Env.debug_pop (fn () => []) *)
      in
          res
      end
    and ref_decR args = ref_dec_memo args (* ref_dec_internal args *)

    (* External ref_dec: checks that there is a minimum result.  *)
    (* Adds memotables to the AST via dec_toRG. *)
    (* val ref_dec : Context * IG.dec -> (TyNameEnv * Env) Comp *)
    and ref_dec (C, dec) errflag = 
      let val decRG = dec_toRG dec  
          val () = Env.clearManySortVars()  (* Clear the set of variables used during memotable lookups. *)
           val pop = Env.debug_push2 (fn () => lines_pp (RG.layoutDec decRG))
          val (redo1, errs1) = ref_decR (C, decRG) errflag
          val ((T1, E1), rc1) = Comp.redoToResAndRC redo1
          val VE_list = map (fn (T, E) => Env.VE_of_E E) 
                            (Comp.redo_to_list redo1)
(*        val _ = print ("ref_dec: #results = " ^ Int.toString (length VE_list) 
                          ^ " #errors = " ^ (Int.toString (length errs1)) ^ "\n")   *)
          val newT = Env.T_plus_T (Env.T_of_C C, T1)
          val conjoinedVE = foldl' (Env.VE_conjoin_vars newT) VE_list
          val res = 
            case VE_list of
                h1::h2::t =>    (* Many VEs are allowed, as long as one of them is a minimum. *)
                  (case List.find (fn VE => Env.subVE newT (VE, conjoinedVE)) VE_list  of
                      SOME VE =>  ((T1, Env.E_plus_E (E1, Env.VE_in_E VE)), errs1)
                    | NONE => adderrs (error ((T1, E1), RG.get_info_dec decRG, REI.NOT_UNIQUE)
                                             errflag, 
                                       errs1 ) )
              | _ => ((T1, E1), errs1)
          val () = pop (fn () => [])
      in
        res
      end
      
    and ref_typbind(C, RG.TYPBIND(i, tyvars, tycon, ty, typbind_opt)) errflag = 
      let
        val (svs, C1) = Env.C_plus_U' (C, tyvars)
        val sortsvs = map RO.mkSortSortVar svs
        val (srt, errs1) = ref_ty (C1, ty, (true, false)) errflag
        val sortfcn = RO.mkSortFcn(svs, srt)
        val TE1 = Env.singleTE(tycon, sortfcn)
        val RE1 = Env.singleRE(tycon, sortfcn)
      in
        case typbind_opt 
          of NONE => ((TE1, RE1), errs1)
           | SOME typbind => 
             case (ref_typbind (C, typbind) errflag) of ((TE2, RE2), errs2) =>
             ( (Env.TE_plus_TE(TE1, TE2), Env.RE_plus_RE(RE1, RE2)), errs2 @ errs1)
      end

    and ref_srtbind(C, RG.TYPBIND(i, tyvars, sortcon, sort, srtbind_opt)) errflag = 
      let
        val (svs, C1) = Env.C_plus_U' (C, tyvars)
        val sortsvs = map RO.mkSortSortVar svs
        val (srt, errs1) = ref_ty(C1, sort, (true, false)) errflag
        val RE1 = Env.singleRE(sortcon, RO.mkSortFcn(svs, srt))
      in
        case srtbind_opt of NONE => (RE1, errs1)
                          | SOME srtbind => 
                              case ref_srtbind(C, srtbind) errflag of (RE2, errs2) =>
                                (Env.RE_plus_RE(RE1, RE2), errs2 @ errs1)
      end
      
    and ref_exbind(C, RG.EXBIND(i, RG.OP_OPT(excon, withOp), ty_opt, exbind_opt)) errflag =
         let
           val (srt, errs1) = 
             case ty_opt of NONE => noErr RO.SortExn
                          | SOME ty => letCV (ref_ty(C, ty, (true, false))) (fn srt1 =>
                                       RO.mkSortArrow(srt1, RO.SortExn)  ) errflag
           val EE1 = Env.singleEE(excon, srt)
         in          
           letCV (ref_exbind_opt (C, exbind_opt)) (fn EE2 =>
           Env.EE_plus_EE(EE1, EE2)  ) errflag
         end
      | ref_exbind (C, RG.EXEQUAL(i, RG.OP_OPT(excon, exconOp), 
                                  RG.OP_OPT(longid, longidOp), rest)) errflag =
          (case Env.Lookup_longid(C, longid) of
             SOME (Env.LONGEXCON srt) =>
               letCV (ref_exbind_opt (C, rest))  (fn EE_rest =>
               (Env.EE_plus_EE (Env.singleEE (excon, srt), EE_rest))  ) errflag
           | _ => impossible "ref_exbind")  (* caught in elaboration *)
             
    and ref_exbind_opt (C, NONE) errflag = noErr Env.emptyEE
      | ref_exbind_opt (C, SOME exbind) errflag = ref_exbind(C, exbind) errflag

    (* val ref_datbind :> Context * datbind * bool -> (TyNameEnv * VarEnv * bool) Comp *)
    and ref_datbind (C, RG.DATBIND(i, tyvar_list, tycon, conbind, datbind_opt),
                     covariant_sofar) errflag = 
      let
        val sortfcn = case Env.Lookup_sortcon (C, tycon)
                        of SOME sortfcn => sortfcn
                         | NONE => Crash.impossible "RefDec.ref_datbind(1)"
        val sortname = case RO.SortFcn_to_SortName sortfcn
                         of SOME sortname => sortname
                          | NONE => Crash.impossible "RefDec.ref_datbind(2)"
        val tyname = SortName.tyname sortname 
        val (sv_list, C1) = Env.C_plus_U' (C, tyvar_list)
        val sort_list = map RO.mkSortSortVar sv_list
        val sort = RO.mkSortConsSort
                     (RO.mkConsSort(sort_list, sortname))
        val ((CE, RC, covariant_conbind), errs1) = ref_conbind (C1, sort, conbind) errflag
        val VE = Env.CE_to_VE CE
(*        val empty_sn = SortName.freshEmptySortName tyname*)
(*        val R = Env.R_plus_R(Env.singleR(sortname, RC), Env.singleR(empty_sn, Env.emptyRC))*)
(*        val RL = Env.addRL(Env.addRL(Env.singleRL sortname, (sortname, empty_sn), empty_sn),*)
(*                           (empty_sn, empty_sn), empty_sn)*)
        val R = Env.singleR(sortname, RC)
        val RL = Env.singleRL sortname

        val ((T', VE', covariant), errs2) = 
          ref_datbind_opt(C, datbind_opt, covariant_sofar andalso covariant_conbind) errflag
        val _ = if covariant then (SortName.set_Covariance sortname)
(*                                   SortName.set_Covariance empty_sn)  *)
                else ()
        val tystr = Env.mkTyStr(CE, R, sortname, covariant, RL)
        val outT = Env.T_plus_T (Env.singleT (tyname, tystr), T')
      in
        ((outT, Env.VE_plus_VE (VE, VE'), covariant), errs2 @ errs1)
      end

    and ref_datbind_opt (C, SOME datbind, covariant_sofar) errflag =
          ref_datbind (C, datbind, covariant_sofar) errflag
      | ref_datbind_opt (C, NONE, covariant_sofar) errflag =
          noErr (Env.emptyT, Env.emptyVE, covariant_sofar)

    (* Constructor binding *)
    (* bool is whether type has only covariant constructors *)
    and ref_conbind (C, sort, conbind) errflag
          : (Env.ConEnv * Env.SortCons * bool) * (Error list) = 
      case conbind of
        RG.CONBIND(i, RG.OP_OPT(longcon, withOp), ty_opt, conbind_opt) =>         
          let
            val ((sort1, sortFcn_opt, covariant), errs1) = 
              case ty_opt
                of SOME ty => 
                    letCV (ref_ty (C, ty, (true,false))) (fn sort2 =>
                    (RO.mkSortArrow (sort2, sort), 
                     SOME (RO.mkSortFcn (RO.sortvarsSort sort, sort2)), (* Note order! *)
                     RO.covariant_sort (covariant_sortname C) sort2)  ) errflag
                 | NONE => noErr (sort, NONE, true)

            val sortScheme = RO.Close_Sort sort1    (* Vars in order of last occurance *)
            val ((CE, RC, covariant_rest), errs2) =
              ref_conbind_opt(C, sort, conbind_opt) errflag
            val con = Ident.id_of_longid longcon
          in
              ((Env.CE_plus_CE (Env.singleCE(con, sortScheme), CE),
                Env.RC_plus_RC (RC, Env.singleRC(con, sortFcn_opt)),
                covariant_rest andalso covariant),
               errs2 @ errs1)
          end

    and ref_conbind_opt(C, sort, conbind_opt) errflag : 
        (Env.ConEnv * Env.SortCons * bool) * (Error list) =
      case conbind_opt of SOME(conbind) => ref_conbind(C, sort, conbind) errflag
                        | NONE => noErr (Env.emptyCE, Env.emptyRC, true)

    and ref_datsortbind (C, RG.DATBIND(i, sortvar_list, sortcon, consortbind, 
                                            datsortbind_opt)) errflag = 
      let
        val sortfcn = case Env.Lookup_sortcon (C, sortcon)
                        of SOME sortfcn => sortfcn
                         | NONE => Crash.impossible "RefDec.ref_datsortbind(1)"
        val sortname = case RO.SortFcn_to_SortName sortfcn
                         of SOME sortname => sortname
                          | NONE => Crash.impossible "RefDec.ref_datsortbind(2)"
        val tyname = SortName.tyname sortname 
        val cov = case Env.Lookup_tyname (C, tyname)
                    of SOME tystr => Env.covariant_of_TyStr tystr
                     | NONE => true   (* A dummy value.  The error is caught later. *)
        val _ = if cov then (SortName.set_Covariance sortname) else ()
        val ((), errs1) = if cov orelse !Flags.allow_noncovariant_refinements then
                             noErr ()  (* Still need to show non-covariant is sound. *)
                          else 
                              error ((), i, REI.NOT_COVARIANT) errflag
        val (sv_list, C1) = Env.C_plus_U' (C, sortvar_list)
        val sort_list = map RO.mkSortSortVar sv_list
        val sort = RO.mkSortConsSort
                     (RO.mkConsSort(sort_list, sortname))
        val (RC, errs2) = ref_consortbind (C1, sort, consortbind) errflag
        val TRmap = FinMapEq.singleton (tyname, Env.singleR(sortname, RC))
        val (TRmap', errs3) = ref_datsortbind_opt(C, datsortbind_opt) errflag
        val out_TRmap = FinMapEq.mergeMap TyName.eq Env.R_plus_R TRmap' TRmap
      in
        (out_TRmap, errs3 @ errs2 @ errs1)
      end

    and ref_datsortbind_opt (C, SOME datsortbind) errflag =
          ref_datsortbind (C, datsortbind) errflag
      | ref_datsortbind_opt (C, NONE) errflag = noErr FinMapEq.empty

    and ref_datsortbind_completeRG (C, datsortbind) errflag = 
          let
            val RE = datasort_initial_RE (C, datsortbind)
            val C' = Env.C_plus_E (C, Env.E_plus_E (Env.RE_in_E RE, 
                                                    Env.TE_in_E (Env.convertREtoTE RE)))
          in
            case ref_datsortbind (C', datsortbind) errflag
              of (TR_map, []) => (* no errors *)
                 let  
                   val (outT, outRE, outVE) = Env.completeT (C', FinMapEq.list (* (raise Match) *)
                                                                               TR_map, RE)
                   val outE = Env.E_plus_E (Env.E_plus_E (Env.RE_in_E outRE, Env.VE_in_E outVE),
                                            Env.TE_in_E (Env.convertREtoTE outRE))                   
                   val _ =  if !Flags.PRINT_DATASORT_PROGRESS then
                                let fun pr s = TextIO.output (TextIO.stdErr, s)
                                in (pr "[Finished analysis of datasorts:";
                                    map (fn sortcon => pr (" " ^ TyCon.pr_TyCon sortcon) )
                                        (Env.REdom outRE);
                                    pr ".]\n";
                                    ())
                                end
                            else ()
                 in
                   ((outT, outE), [])
                 end
               | (_, errs) =>    (* Env.completeT will crash if errors *)
                   ((Env.emptyT, Env.emptyE), errs)
          end                               
       
    and ref_datsortbind_complete (C, datsortbind) errflag = 
        ref_datsortbind_completeRG (C, MapDecEtoR.map_datbind_info RefInfo.from_ElabInfo 
                                                                   datsortbind  ) errflag

    and ref_consortbind (C, srt, RG.CONBIND(i, RG.OP_OPT(longid, withOp),
                                            sort_opt, consortbind_opt)) errflag =
      let
        val ((srt2, sortFcn_opt), errs1) =
          case sort_opt
            of SOME sort => 
               letCV (ref_ty (C, sort, (true, false))) (fn srt1 =>
               (RO.mkSortArrow(srt1, srt), 
                SOME (RO.mkSortFcn (RO.sortvarsSort srt, srt1)))  ) errflag
             | NONE => noErr (srt, NONE)
        val sortScheme = RO.Close_Sort srt2
        val (_, id) = Ident.decompose longid
        val (RC, errs2) = 
          ref_consortbind_opt(C, srt, consortbind_opt) errflag
        val (out_RC, errs3) =
          case Env.Lookup_longid (C, longid)
            of SOME (Env.LONGCON sortScheme1) => 
               (case RO.compatible_SortScheme (sortScheme, sortScheme1)
                  of true => noErr (Env.RC_plus_RC (Env.singleRC(id, sortFcn_opt), RC))
                              
                   | false => error (RC, i, REI.INCOMPATIBLE(sortScheme, sortScheme1)) errflag
               )
             | _ => error (RC, i, REI.LOOKUP_LONGCON longid) errflag
      in
        (out_RC, errs3 @ errs2 @ errs1)
      end

    and ref_consortbind_opt(C, srt, SOME consortbind) errflag =
          ref_consortbind(C, srt, consortbind) errflag
      | ref_consortbind_opt (C, srt, NONE) errflag = noErr Env.emptyRC

    (* Get PLAINvalbind info, return TyVars, Type, and C plus Uenv. *)
    and add_valbind_Uenv (C, RG.RECvalbind(_, valbind)) : RO.TyVar list * RO.Type * Env.Context = 
                add_valbind_Uenv (C, valbind)
      | add_valbind_Uenv (C, RG.PLAINvalbind(i, _, _, _)) =
                case (getPostElabTypeInfo i)
                  of SOME(TypeInfo.PLAINvalbind_INFO{tyvars, Type, Uenv}) => 
                        (tyvars, Type, Env.C_plus_U (C,Uenv))
                   | _ => Crash.impossible "RefObject.add_valbind_Uenv(1)"

    and ref_valbind (C, valbind) errflag : Env.VarEnv Redo Result =        
       case valbind of
         RG.RECvalbind(i, valbind) =>
          let
              val (_, _, Cnew) = add_valbind_Uenv (C,valbind)
              fun valbindVE valbind errflag2 = 
                case valbind of
                   RG.RECvalbind(i, valbind) =>  valbindVE valbind errflag2
                 | RG.PLAINvalbind(i, pat1, exp2, valbind_opt) =>
                    let    (* pat1 can't contain constructors or pairs since exp2 is fn *)
                      val srtR1 = case get_goal_srtRC(Cnew, pat1)  of
                                     SOME srtR =>  srtR
                                   | NONE =>  noErrC (noRedo (findMLSort(Cnew, i)))
                    in
                      letR srtR1  (fn srt1 =>
                      letC (ref_pat (Cnew, pat1, mkSORTps C srt1))  (fn ((VE1, _)::_, _, _) =>
                      (pr_debug ("ref_valbind: REC ", fn () => Env.layoutVE VE1);
                       case valbind_opt of NONE => noErrC (noRedo VE1)
                                         | SOME vb => 
                                             letRV (valbindVE vb)  (fn VE2 =>
                                             Env.VE_plus_VE(VE1, VE2) )))) errflag2
                    end  
            in
              letR (valbindVE valbind)  (fn VE =>
              ref_valbind (Env.C_plus_VE(C, VE), valbind)  ) errflag
            end  (* Raise an error here if VE not met?  (can only happen if no goal) *)
      | RG.PLAINvalbind(i, pat1, exp2, valbind_opt) =>
        let
          val (scoped_tyvars, Type, Cnew) = add_valbind_Uenv (C, valbind)
          val scoped_sortvars = map RO.SVofTV scoped_tyvars
          val _ = pr_debug ("ref_valbind: ", 
                       fn () => TypeInfo.layout (valOf (getPostElabTypeInfo i)))
          fun cont1 VE2 : Env.VarEnv RComp =   (* continuation for VE of pat1 *)
                let val closedVE2 = Env.closVE(VE2, scoped_sortvars)
                in case valbind_opt
                     of NONE => noErrC (noRedo closedVE2)
                      | SOME valbind2 => 
                         letRV (ref_valbind(C, valbind2)) (fn VE3 =>
                         Env.VE_plus_VE (closedVE2, VE3)  )
                end
          fun cont2 srt : Env.VarEnv RComp =   (* continuation for sort of exp2 *)
             (assert (eqTypes "PLAINvalbind: " (RO.tyOfSort srt) Type);          
              letC (ref_pat_max(C, pat1, mkSORTps C srt))  (fn (VE_list, out_patsort) =>
              case (VE_list, isEmptyPS C out_patsort)
                of ([VE], true) => cont1 VE
                 | ([VE], false) => letC (error (VE, i, REI.UNMATCHED srt)) cont1
                 | (_, false) => letC (error (bogusVEpat pat1, i, REI.UNMATCHED srt)) cont1
                 | (_, true) => letC (error (bogusVEpat pat1, i, REI.MULTIPLE_BINDINGS)) cont1) )

(* The following would work great, except that VE_list is a union, so backtracking isn't
   what we want: we can't succeed when just one succeeds - all must succeed. *)
(*            letC (if isEmptyPS C out_patsort then noErrC ()
                    else error ((), i, REI.UNMATCHED srt) ) (fn () =>
              letR (tryEachV VE_list) cont1 ) ) )
*)
          val Cexp = Env.C_erase_TG Cnew
        in
            case get_goal_srtRC (Cnew, pat1)  of
               SOME gsrtR =>  letR gsrtR  (fn gsrt =>
                              letC (check_exp (Cexp, exp2, gsrt))  (fn () =>
                              cont2 gsrt  )) errflag
             | NONE =>  letR (infer_exp(Cexp, exp2)) cont2 errflag
        end

    and bogusVEpat (RG.ATPATpat (_, atpat)) = bogusVEatpat atpat
      | bogusVEpat (RG.CONSpat (_, _, atpat)) = bogusVEatpat atpat
      | bogusVEpat (RG.TYPEDpat(_, pat, _)) = bogusVEpat pat
      | bogusVEpat (RG.SORTEDpat(_, pat, _)) = bogusVEpat pat
      | bogusVEpat (RG.LAYEREDpat(_, RG.OP_OPT(id, _), _, pat)) = 
           Env.VE_plus_VE (Env.singleVarVE(id, bogus_sortsch), bogusVEpat pat)
      | bogusVEpat (RG.UNRES_INFIXpat _) = impossible "bogusVEpat"

    and bogusVEatpat (RG.LONGIDatpat (i, RG.OP_OPT(longid, _))) = 
         (case (getPostElabTypeInfo i)
            of SOME (TypeInfo.VAR_PAT_INFO _) => 
                  Env.singleVarVE(Ident.id_of_longid longid, bogus_sortsch)
             | _ => Env.emptyVE)
      | bogusVEatpat (RG.RECORDatpat (i, patrow_opt)) = bogusVEpatrowopt patrow_opt
      | bogusVEatpat (RG.PARatpat (_, pat)) = bogusVEpat pat
      | bogusVEatpat _ = Env.emptyVE

    and bogusVEpatrowopt (SOME (RG.PATROW(_, _, pat1, patrow_opt2))) = 
         Env.VE_plus_VE (bogusVEpat pat1, bogusVEpatrowopt patrow_opt2)
      | bogusVEpatrowopt _ = Env.emptyVE

    and ref_valsdesc (C, RG.VALSDESC (i, id, tys, valsdesc_opt)) errflag : TyGoals Result =
        let val tyvars = ListHacks.minus (RG.getExplicitTyVarsTy (hd tys), Env.U_of_C C)
            val (svs, Cnew) = Env.C_plus_U' (C, tyvars)
        in
          letC (mapC (fn ty => (letCV (ref_ty (Cnew, ty, (true, true)))) (fn srt =>
                               RO.Close_SortScheme (RO.Sort_in_SortScheme srt, svs) ))
                     tys)
          (fn srtschs =>
          letCV (ref_valsdesc_opt (C, valsdesc_opt))  (fn TG2 =>
          Env.TG.add0(id, srtschs, TG2)  
          )) errflag
(*
          letR (Comp.tryEachV tys) (fn ty =>  (* Better to tryEach in get_goal_srtC *)
          letC (ref_ty (Cnew, ty, (true, true)))  (fn srt =>
          letRV (ref_valsdesc_opt (C, valsdesc_opt))  (fn TG2 =>
          (Env.TG.add0(id, RO.Close_SortScheme (RO.Sort_in_SortScheme srt, svs), TG2))
          ))) errflag
*)
        end

    and ref_valsdesc_opt (C, NONE) errflag = noErr Env.TG.empty
      | ref_valsdesc_opt (C, SOME valsdesc) errflag = ref_valsdesc (C, valsdesc) errflag

    (****** types ******)
    (* Checks variance following covar & contra - true if variance is allowed *)
    and ref_ty (C :  Context, ty : RG.ty, (covar:bool, contra:bool)) errflag
             : RO.Sort Result =
        case ty of
          RG.TYVARty(i, tyvar) =>
            let
              val srt = RO.MLSortOfTy (TNtoSN C) (Env.ExplicitTyVar_lookup C tyvar)
                        handle Match => RO.mkSortSortVar (sortvar_to_sv tyvar)
                                       before (out_debug (fn () => "ExplicitTyVar_lookup: NONE"))
              val _ = out_debug (fn () => "ExplicitTyVar_lookup: " ^ 
                                           (TyVar.pr_tyvar tyvar) ^ " => " ^ (RO.pr_Sort srt))
            in
              case (TyVar.variance tyvar, covar, contra) of
                 (TyVar.COVARIANT, true, _) =>  noErr srt
               | (TyVar.CONTRAVARIANT, _, true) =>  noErr srt
               | (TyVar.MIXED, _, _) =>  noErr srt
               | (TyVar.IGNORED, true, true) =>  noErr srt
               | _ => error (srt, i, REI.VARIANCE) errflag
            end
        | RG.RECORDty(i, NONE) =>  (* unit *)
            noErr RO.SortUnit
        | RG.RECORDty(i, SOME tyrow) =>
            letCV (ref_tyrow(C, tyrow, (covar, contra))) RO.mkSortRecSort errflag
        | RG.CONty(i, ty_list, longtycon) =>
            let
              val sortfcn1 = case Env.Lookup_longsortcon(C, longtycon)  of
                                SOME sortfcn => sortfcn
                              | NONE => Crash.impossible ("RefDec.ref_ty : CONty(1).  longtycon = "
                                                          ^ TyCon.pr_LongTyCon longtycon)
              val sortfcn2 = case Env.Lookup_longtycon(C, longtycon)  of
                                SOME sortfcn => sortfcn
                              | NONE => Crash.impossible "RefDec.ref_ty : CONty(2)"
              val sort_err_list = 
                map (fn (ty, variance) =>
                       ref_ty (C, ty, calc_variance (variance, covar, contra)) errflag)
                    (ListPair.zip (ty_list, RO.varianceSortFcn sortfcn1))
              val srt = RO.applySortFcn(sortfcn1, map #1 sort_err_list)
              val _ = out_debug (fn () => "ref_ty CONty: " ^ (RO.pr_Sort (#1 (hd sort_err_list)))
                                          ^ " => " ^ (RO.pr_Sort srt)) handle Empty => ()
              val errs = ListHacks.flatten (map #2 sort_err_list)
            in
              if RO.compatible_SortFcn(sortfcn1, sortfcn2)  then  (srt, errs)
              else  adderrs (error (srt, i, REI.SHADOWED longtycon) errflag, errs)
            end
        | RG.FNty(i, ty1, ty2) =>
            letC (ref_ty(C, ty1, (contra, covar)))  (fn srt1 =>
            letCV (ref_ty(C, ty2, (covar, contra)))  (fn srt2 =>
            RO.mkSortArrow(srt1, srt2)  )) errflag
        | RG.PARty(i, ty) =>
            ref_ty(C, ty, (covar, contra)) errflag
        | RG.INTERty(i, ty1, ty2) =>  (* disallow outside RML-spec-comments? *)
            letC (ref_ty(C, ty1, (covar, contra)))  (fn srt1 =>
            letC (ref_ty(C, ty2, (covar, contra)))  (fn srt2 =>
            if RO.compatible_Sort (srt1, srt2)  then
              noErrC (RO.mkSortConj (Env.conjSortNameT' (Env.T_of_C C)) (srt1, srt2))
            else  error (bogus_sort, i,      (* This should have been caught in ElabDec! *)
                         REI.INCOMPATIBLE (RO.Sort_in_SortScheme srt1,
                                           RO.Sort_in_SortScheme srt2))  )) errflag

    and ref_tyrow (C :  Context, tyrow : RG.tyrow, (covar, contra)) errflag 
            : RO.RecSort Result =
        case tyrow of RG.TYROW(i, lab, ty, tyrow_opt) =>
            letC (ref_ty(C, ty, (covar, contra)))  (fn sort =>
            case tyrow_opt of
              SOME tyrow => letCV (ref_tyrow(C, tyrow, (covar, contra)))  (fn rho =>
                            RO.addField (lab,sort) rho  )
            | NONE => noErrC (RO.addField (lab,sort) RO.emptyRecSort)  )  errflag

    and ref_patrow_opt (C: Context, patrow_opt: RG.patrow option, patsort: PatSort) errflag
          : ((Env.VarEnv * PatSort) list * PatSort * PatSort) Result =
     case patrow_opt  of
       SOME (RG.PATROW(i, lab1, pat1, patrow_opt2)) =>   (* this is messy *)
        let
          (* val () = Env.debug_push (fn () => "\n****ref_patrow_opt:SOME"
                                            :: lines_pp (RG.layoutPat pat1)); *)

          fun productVE_psrt_list (VE_psrt_list1, VE_psrt_list2) = 
            List.foldl (fn ((VE1,psrt1), b) => 
                          b @ (map (fn (VE2, psrt2) => (Env.VE_plus_VE(VE1, VE2), 
                                                        ADDFIELDps (lab1, psrt1, psrt2)) ) 
                                   VE_psrt_list2))
                       [] VE_psrt_list1
          (* A union as a list of pairs of patsorts: one for lab1, and for the rest of the row *)
          val patsort_pairs = case projectPatsort(C, patsort, lab1) 
                                of [] => [(EMPTYps, EMPTYps)]  (* Need at least one *)
                                 | ps_pair => ps_pair         
          fun ref_patsort_pair (patsort1, patsort2) =
              letC (ref_pat (C, pat1, patsort1)) (fn (VE_psrt_list1, match_ps1, out_patsort1) =>
              letCV (ref_patrow_opt(C, patrow_opt2, patsort2))
                                                 (fn (VE_psrt_list2, match_ps2, out_patsort2) =>
              (productVE_psrt_list(VE_psrt_list1, VE_psrt_list2),
               mkADDFIELDps(lab1, match_ps1, match_ps2),
               mkUNIONps(mkADDFIELDps(lab1, out_patsort1, out_patsort2),   (* Better patsort? *)
                         mkUNIONps(mkADDFIELDps(lab1, out_patsort1, match_ps2), 
                                   mkADDFIELDps(lab1, match_ps1, out_patsort2)) )
(* With the version below, match_ps is never needed and could be removed from ref_pat.  *)
(* But, this is slightly less precise, and leads to many redundant contexts.  *)
(*             mkUNIONps(mkADDFIELDps(lab1, out_patsort1, patsort2), 
                         mkADDFIELDps(lab1, patsort1, out_patsort2)) *)
                              )   ) ) errflag


          fun combine_results (((VE_psrt_list1, match_ps1, out_ps1), errs1),
                               ((VE_psrt_list2, match_ps2, out_ps2), errs2)) =
            let val errs = case errs1 of [] => errs2 
                                       | _ => errs1
            in
                ((VE_psrt_list1 @ VE_psrt_list2, mkUNIONps(match_ps1, match_ps2), 
                                                 mkUNIONps(out_ps1, out_ps2)      ),
                 errs)
            end

          val res = foldl' combine_results (map ref_patsort_pair patsort_pairs)
(*          val () = Env.debug_pop (fn () => "\n****ref_patrow_opt:SOME"
                                            :: lines_pp (RG.layoutPat pat1)); *)
(*          val _ =  print ("PATROW: " ^ (Int.toString(List.length (#1 (#1 res)))) ^ "\n") *)
        in
          res
        end                  
     | _ =>      (* Case for NONE or SOME DOTDOTDOT *)
            noErr ([(Env.emptyVE, patsort)], patsort, EMPTYps)

    (* Returns possible VE's with corresponding sorts for the pattern plus both matched and 
       remaining PatSort.   *)
    and ref_atpat (C: Context, atpat: RG.atpat, patsort: PatSort) errflag
          : ((Env.VarEnv * PatSort) list * PatSort * PatSort) Result =
     case atpat
     of RG.LONGIDatpat(i, RG.OP_OPT(longid, withOp)) => 
       (case (getPostElabTypeInfo i)
          of SOME (TypeInfo.CON_INFO {instances, ...}) => 
             let val (out_patsort, arg_ps, srtsPat,srt_instances) = dividePatsort(C, patsort, longid)     
                 val (SOME (Env.LONGCON ssch)) = Env.Lookup_longid(C,longid)
                 val matched_patsort = mkVALCONSps(longid, (srtsPat, ssch, srt_instances),
                                                   arg_ps)    (* The instances shouldn't be used? *)
                 val VE_psrt_list = case arg_ps of NONE => [(Env.emptyVE, matched_patsort)] 
                                                | SOME _ => []
             in
               noErr (VE_psrt_list, matched_patsort, out_patsort)
             end
           | SOME (TypeInfo.EXCON_INFO _) => 
             let val (out_patsort, arg_ps, srtsPat, srt_instances) = dividePatsort(C, patsort, longid)
                 val (SOME (Env.LONGEXCON srt)) = Env.Lookup_longid(C,longid)
                 val ssch = RO.Sort_in_SortScheme srt
                 val VE_psrt_list = case arg_ps 
                                      of NONE => [(Env.emptyVE, mkSORTps C srt)] 
                                       | SOME _ => []
             in
                 noErr (VE_psrt_list, mkVALCONSps(longid, (srtsPat, ssch, srt_instances), arg_ps), out_patsort)
             end
           | _ =>   (* must be variable pattern *)
             let val id = Ident.id_of_longid longid
                 fun srt_to_VE_psrt srt =  (* Could have many or zero VEs in this case.  *)
                   (Env.singleVarVE(id, RO.Sort_in_SortScheme srt), mkSORTps C srt)
                 val (srts, warn) = patSort_to_Sorts (C, patsort)
                 val res = (map srt_to_VE_psrt srts, patsort, EMPTYps)
             in
               if warn then error(res, i, REI.TOO_MUCH) errflag else noErr res
             end)
      | RG.RECORDatpat(i, patrow_opt) =>  ref_patrow_opt (C, patrow_opt, patsort) errflag
      | RG.WILDCARDatpat i => 
            let val outVEpatsorts = 
                if isEmptyPS C patsort then [] else [(Env.emptyVE, patsort)]
                (* The following is less efficient.  It is retained here because exposes a bug
                   while checking Twelf, file cs-eq-strings.sml *)
                (* case patSort_to_Sorts (C, patsort) of [] =>[]
                                                    | _ => [(Env.emptyVE, patsort)] *)
            in
                noErr (outVEpatsorts, patsort, EMPTYps)
            end
      | RG.SCONatpat (i, scon) =>   (* no refinements currently *)
          noErr ([(Env.emptyVE, patsort)], patsort, patsort)
      | RG.PARatpat(i, pat) =>  ref_pat (C, pat, patsort) errflag

    (* Helper for ref_pat below.  Checks that each matching case is less than the bound. *)
    and check_pat_ty (C, i, pat1, patsort2, ty3) errflag =
          letC (ref_ty0 (C, ty3))  (fn srt3 =>
          letC (ref_pat (C, pat1, patsort2))  (fn res as (VE_psrt_list, _, _) => fn errflag2 =>
          case (map (fn (_, psrt) => (patSort_to_Sorts (C, psrt))) VE_psrt_list ) of srtsWarnList =>
          case List.exists (fn w => w) (map #2 srtsWarnList) of warn =>
          case (if warn then error(res, i, REI.TOO_MUCH) errflag2  else  noErr res) of (res2, errs) =>
          case List.find (fn srt => not (RO.subSort (Env.conjSortNameC C) (srt, srt3)))
                         (ListHacks.flatten (map #1 srtsWarnList))
            of NONE => (res2, errs)   (* if warn then error(res, i, REI.TOO_MUCH)  else  noErrC res *)
             | SOME srt => adderrs (error (res, i, REI.NOT_SUBSORT(srt, srt3)) errflag2, errs)  )) errflag

    (****** Patterns (patsort input).  ******)
    (* Returns possible VE's with corresponding sorts of pattern plus both matched and 
       remaining PatSort. *)
    and ref_pat0 (C: Env.Context, pat: RG.pat, patsort: PatSort) errflag
          : ((Env.VarEnv * PatSort) list * PatSort * PatSort) Result =
     case pat
     of RG.ATPATpat(i1, atpat) => ref_atpat (C, atpat, patsort) errflag
      | RG.TYPEDpat(i, pat1, ty2) => ref_pat (C, pat1, patsort) errflag
      | RG.SORTEDpat(i, pat1, tys2) => 
           letRC (tryEachV tys2) (fn ty2 =>
           check_pat_ty (C, i, pat1, patsort, ty2) ) errflag
      | RG.CONSpat(i, RG.OP_OPT(longid, withOp), atpat1) =>
           let
(*           val instances = case (getPostElabTypeInfo i)
                               of SOME (TypeInfo.CON_INFO {instances, ...}) => 
                                    map (RO.MLSortOfTy (TNtoSN C)) instances  (* Not right, but unused (?) *)
                                | SOME (TypeInfo.EXCON_INFO _) => []
                                | _ => Crash.impossible "RefDec.ref_pat(1)"  *)
             val (out_ps, ps_opt, srtsPat, instances) = dividePatsort(C, patsort, longid)
             val ps1 = case ps_opt of NONE => EMPTYps | SOME ps1 => ps1  (* NONE => type error *)
             val ((VE_psrt_list, match_ps1, out_ps1), errs1) = ref_atpat(C, atpat1, ps1) errflag
             val con_ssch = case Env.Lookup_longid (C, longid) of 
                               SOME (Env.LONGCON ssch) => ssch
                             | SOME (Env.LONGEXCON srt) => RO.Sort_in_SortScheme srt
             fun mkPS ps = mkVALCONSps(longid, (srtsPat, con_ssch, instances), SOME ps)
             val out_VE_psrt_list = map (fn (VE, psrt1) => (VE, mkPS psrt1)) VE_psrt_list
           in
             ( (out_VE_psrt_list, mkPS match_ps1, mkUNIONps(out_ps, mkPS out_ps1)), errs1)
           end
      | RG.LAYEREDpat(i, RG.OP_OPT(id, withOp), ty_opt, pat2) =>
           let
             fun extend_VE_psrt VE_psrt_list = 
               let val res_sWarnList = 
                 (map (fn (VE, psrt) =>
                  case (patSort_to_Sorts (C, psrt)) of (p2srts,warn) =>
                  (map (fn srt => (Env.VE_plus_VE (VE, Env.singleVarVE(id, RO.Sort_in_SortScheme srt)), 
                                  mkSORTps C srt) )
                      p2srts
                  , warn)  )
                  VE_psrt_list )
                   val res = ListHacks.flatten (map #1 res_sWarnList)
               in
                  if List.exists (fn b=>b) (map #2 res_sWarnList) then  error(res, i, REI.TOO_MUCH)
                  else noErrC res
               end               
           in
             letC (ref_pat (C, pat2, patsort))
                   (*(case ty_opt of NONE =>  ref_pat (C, pat2, patsort)
                                   | SOME ty =>  check_pat_ty (C, i, pat2, patsort, ty)) (* Wrong! *) *)
                   (fn (VE_psrt_list, match_ps, out_patsort) =>
             letCV (extend_VE_psrt VE_psrt_list) (fn res =>
             (res, match_ps, out_patsort) ) ) errflag
           end        
      | RG.UNRES_INFIXpat _ => Crash.impossible "RefDec.ref_pat"
    
    and ref_pat (C: Env.Context, pat: RG.pat, patsort: PatSort) errflag
         : ((Env.VarEnv * PatSort) list * PatSort * PatSort) Result =
     ((*if sizePS patsort > 50 then
          (pr_indent (("ref_pat: START " ^ Int.toString (sizePS patsort) ));
           largestPS := sizePS patsort)
      else (); *)
      (*Env.debug_push (fn () => "\n****ref_pat"
                               :: lines_pp (RG.layoutPat pat)); *)
      case ref_pat0(C, pat, patsort) errflag of res =>
      ( (* Env.debug_pop (fn () => []);  *)
        (* if sizePS (#3 (#1 res)) > 50 then       
            (pr_indent ("ref_pat: END " ^ Int.toString (sizePS (#3(#1 res))) ^ 
                        " MATCHED: " ^ (pr_PatSort (#3 (#1 res)))   );
                        largestPS := sizePS (#3 (#1 res)))
         else ();*)
        res))

    (* Remove non-maximal environments.  Later, it would be better to rely on memoization. *)
    and ref_pat_max(C: Env.Context, pat: RG.pat, patsort: PatSort) errflag
          : (Env.VarEnv list * PatSort) Result =
      letCV (ref_pat (C, pat, patsort))  (fn (VE_psrt_list, match_ps, patsort) =>
      ((*if length VE_psrt_list > 1 then 
          print ("ref_pat_max: " ^ (Int.toString(List.length VE_psrt_list)) ^ "\n")
       else (); *)
      (let (*val () = Env.debug_push (fn () => "\n****ref_pat_max(middle):"
                                               :: lines_pp (RG.layoutPat pat)) *)
           val res = 
       (List.foldr (fn ((VE,_), accVE_list) => 
                     ListHacks.addMax (Env.subVE(Env.T_of_C C)) (VE, accVE_list))
                   [] 
                   VE_psrt_list, 
        patsort)
           val lengthVE = Env.VEfold (fn (_, acc)  => acc + 1) 0
(*         val _ = if length (#1 res) <> 1 orelse (lengthVE (hd (#1 res))) > 1 then

                      ( print ("res: " ^ (Int.toString(List.length (#1 res))) ^ "\n");
                        app pr (map (fn VE => ("VE=", Env.layoutVE VE)) (#1 res));
                        print "\n" (*;
                        print (pr_PatSort (#2 res) ^ "\n\n") *) )
                   else () *)
(*         val () = Env.debug_pop (fn () => []) *)
       in
         res
       end)  ) ) errflag

    (* Find a goal sort either at the top of a pattern or in TyGoals of C *)
    and get_goal_srtRC (C, RG.TYPEDpat(i, pat, ty)) = get_goal_srtRC (C, pat)
      | get_goal_srtRC (C, RG.SORTEDpat(i, pat, tys)) =          
          SOME (letR (tryEachV tys) (fn ty =>
                letCV (ref_ty0 (C, ty)) (fn ty => 
                noRedo ty ) ))
      | get_goal_srtRC (C, RG.ATPATpat(i1, RG.PARatpat(i2, pat))) = 
          get_goal_srtRC (C, pat)
      | get_goal_srtRC (C, RG.ATPATpat(i1, RG.LONGIDatpat(i2, RG.OP_OPT(longid, withOp)))) = 
          (case (getPostElabTypeInfo i2)  of
            SOME (TypeInfo.VAR_PAT_INFO {tyvars, ...}) =>
               (case Env.Lookup_tygoal (C, Ident.id_of_longid longid)  of
                   SOME srtschs => SOME (letRV (tryEachV srtschs) (fn srtsch =>
                                         (RO.instance (srtsch, map TVtoSort tyvars)) ))
(*                                   before 
                                 (pr_debug ("get_goal_srtRC : ", 
                                            fn () => RO.layoutSortScheme srtsch  )) *)
                 | NONE => NONE before (pr_debug ("get_goal_srt FAILED: ", 
                                          fn () => Env.TG.layout (Env.TG_of_E (Env.E_of_C C))))
                           )
          | SOME _ => NONE  (* CON_INFO or EXCON_INFO *)
          | NONE => impossible "get_goal_srtRC: No Info") 
      | get_goal_srtRC (C, _) = NONE

    (* Find a goal sort either at the top of a pattern or in TyGoals of C *)
(*    and get_goal_srtC (C, RG.TYPEDpat(i, pat, ty)) = get_goal_srtC (C, pat)
      | get_goal_srtC (C, RG.SORTEDpat(i, pat, [ty])) = SOME (ref_ty0 (C, ty))
      | get_goal_srtC (C, RG.ATPATpat(i1, RG.PARatpat(i2, pat))) = 
          get_goal_srtC (C, pat)
      | get_goal_srtC (C, RG.ATPATpat(i1, RG.LONGIDatpat(i2, RG.OP_OPT(longid, withOp)))) = 
          (case (getPostElabTypeInfo i2)  of
            SOME (TypeInfo.VAR_PAT_INFO {tyvars, ...}) =>
               (case Env.Lookup_tygoal (C, Ident.id_of_longid longid)  of
                   SOME srtsch => SOME (noErrC (RO.instance (srtsch, map TVtoSort tyvars)))
                                   before 
                                 (pr_debug ("get_goal_srt : ", 
                                            fn () => RO.layoutSortScheme srtsch  ))
                 | NONE => NONE before (pr_debug ("get_goal_srt FAILED: ", 
                                          fn () => Env.TG.layout (Env.TG_of_E (Env.E_of_C C))))
                           )
          | SOME _ => NONE  (* CON_INFO or EXCON_INFO *)
          | NONE => impossible "get_goal_srtC: No Info") 
      | get_goal_srtC (C, _) = NONE
*)

    (* Takes a patsort as input, and returns residual patsort.  goal sort is for exp's  *)
    and ref_match (C, match as RG.MATCH(i1, RG.MRULE(i2, pat, exp), match_opt), ps1, gsrt) errflag
           : PatSort Result =
      let
        val pop = Env.debug_push2 (fn () => ("\n****ref_match: sort = " ^ RO.pr_Sort gsrt)
                                           :: ("patsort = " ^ pr_PatSort ps1)
                                           :: lines_pp (RG.layoutMatch match))
        val _ = assert (fn () => 
                           case patSort_to_Sorts (C, ps1) 
                            of ([], _) => NONE
                             | (srt1::srts, _) => (map (fn srt2 => 
                                                           assert (compatSorts "ref_match" srt1 srt2))
                                                       srts; NONE) )

        val ((VElist, residPatsort), errs1) = ref_pat_max (C, pat, ps1) errflag

        fun isWarning (_, REI.UNMATCHED srt) = true
          | isWarning (_, REI.TOO_MUCH) = true
          | isWarning _ = false

        fun do_checks (VE1 :: VEtail) = 
              (case check_exp (Env.C_plus_VE (C, VE1), exp, gsrt) errflag   of  
                  ((), []) =>  do_checks VEtail
                | ((), errs) =>    (* Return the first errors, if none the first warnings. *)
                  (case List.partition isWarning errs of
                       (warnings, []) => (case do_checks VEtail of 
                                              (_, []) => (warnings, []) (* No errs, first warns. *)
                                            | warns_errs2 => warns_errs2)
                     | warns_errs => warns_errs ) )  (* There is an error *)
          | do_checks [] = ([], [])  (* no warnings, no errs *)
        val (warns2, errs2) = do_checks VElist
        val (endPatsort, errs3) = 
          case match_opt 
            of NONE => noErr residPatsort
             | SOME match2 => ref_match (C, match2, residPatsort, gsrt) errflag

        val _ = assert (fn () => 
                           case patSort_to_Sorts (C, endPatsort) 
                            of ([],_) => NONE
                             | (srt1::srts, _) => 
                               (map (fn srt2 => assert (compatSorts "ref_match:END" srt1 srt2)) srts; 
                                NONE) )
        val () = pop (fn () => ("ref_match: sort = " ^ RO.pr_Sort gsrt)
                                               :: lines_pp (RG.layoutMatch match))

      in
        (endPatsort, errs3 @ warns2 @ errs2 @ errs1)
      end


    (****** Expressions  ******)
    (* The goal sort and Context must correctly refine the corresponding types. *)
    (* Could add a separate VE containing only the variables with multiple possible sorts.  *)
    and check_exp0 (C, exp : RG.exp, gsrt : RO.Sort) errflag
           : unit Result =
      case exp of
         RG.FNexp(i, match) =>
            (case (RO.unSortConj gsrt)
               of SOME (srt1, srt2) =>
                    letC (check_exp (C, exp, srt1))  (fn () =>
                    check_exp (C, exp, srt2)  ) errflag
                | NONE =>
                    let
                      val (srt1, srt2) =
                        case (RO.unSortArrow gsrt)
                          of NONE => if RO.isBogusSort gsrt  then  (RO.bogusSort, RO.bogusSort)
                                     else  Crash.impossible "RefDec.check_exp:FNexp"
                           | SOME(srt1, srt2) => (srt1, srt2)
                    in
                      letC (ref_match(C, match, mkSORTps C srt1, srt2))  (fn endPatSort =>
                      if isEmptyPS C endPatSort then  noErrC ()
                        else  (error((), i, REI.UNMATCHED srt1))    ) errflag
                    end)
       (* Need to allow this for the expansion of a "case".  Later consider: C := ... | C I   *)
       | RG.APPexp(i, exp1 as (RG.FNexp _), atexp2) =>
            letRC (infer_atexp (C, atexp2))  (fn srt2 =>
            check_exp(C, exp1, RO.mkSortArrow(srt2, gsrt))  ) errflag
       | RG.RAISEexp(i, exp) =>  (* Could make this inferable, but the sort could be huge.  *)
            check_exp(C, exp, RO.SortExn) errflag
       | RG.HANDLEexp(i, exp1, match2) =>
            letC (check_exp (C, exp1, gsrt))  (fn () =>
            letCV (ref_match (C, match2, mkSORTps C RO.SortExn, gsrt))  (fn _ => (* ignore residual *)
            ()  )) errflag
       | RG.ATEXPexp(i, atexp) => check_atexp (C, atexp, gsrt) errflag
       | RG.TYPEDexp(i, exp1, ty2) => check_exp (C, exp1, gsrt) errflag

       | _ => letRC (if (* exp_has_var_head exp*) false then
                         check_poly_head_exp (C, exp, gsrt, 0)
                     else
                         infer_exp (C, exp))
              (fn inf_srt =>    (* infer sort and compare *)
              if (RO.subSort (Env.conjSortNameC C) (inf_srt, gsrt)) then
                  noErrC ()
              else
                  error((), RG.get_info_exp exp, REI.NOT_SUBSORT(gsrt, inf_srt))  ) errflag

    and infer_exp0 (C, exp : RG.exp) errflag
           : RO.Sort Redo Result =  
      case exp of
         RG.TYPEDexp(i, exp1, ty2) => infer_exp (C, exp1) errflag
       | RG.SORTEDexp(i, exp1, tys2) =>
             letR (Comp.tryEachV tys2) (fn ty =>
             letC (ref_ty0 (C, ty))  (fn srt2 =>
             letCV (check_exp(C, exp1, srt2))  (fn () =>
             noRedo srt2  ))) errflag

       (* Need to handle the expansion of a "#lab atexp" differently. *)
       | RG.APPexp(i, exp1 as RG.ATEXPexp (_, RG.PARatexp (_, RG.FNexp (_,RG.MATCH(_,RG.MRULE(_, 
                         pat11,
                         exp12 as RG.ATEXPexp(_, RG.IDENTatexp(_,RG.OP_OPT(longid2,_)) )),NONE)))), 
                      atexp2) =>
             letR (infer_atexp(C, atexp2))  (fn srt2 => 
             case ref_pat_max (C, pat11, mkSORTps C srt2) errflag
               of (([VE11], residPS), errs11) =>  
                   if not (isEmptyPS C residPS) then impossible "infer_exp:label-project (1)" else
                   infer_exp (Env.C_plus_VE (C, VE11), exp12)
                | _ => impossible "infer_exp:label-project (2)") errflag

       (* Need to allow this for the expansion of a "case". *)
       | RG.APPexp(i, exp1 as (RG.FNexp _), atexp2) =>
           let val def_srt = findMLSort(C, RG.get_info_exp exp)  in
             letCV (check_exp (C, exp, def_srt))  (fn () =>
             noRedo def_srt ) errflag
           end

(*       | RG.APPexp(i, exp1 as RG.ATEXPexp(_, RG.SCONatexp _), atexp2) =>
             letR (infer_exp(C, exp1))  (fn srt1 =>
             apply_consort_to_atexp(C, srt1, atexp2)  ) errflag  *)

       | RG.APPexp(i, exp1, atexp2) =>   (* We could use distributivity here for constructors... *)
             letR (infer_exp(C, exp1))  (fn srt1 =>  (* With non-covariant arguments, it gives a *)
             apply_sort_to_atexp(C, srt1, atexp2)  ) errflag                (* different result. *)

       | RG.ATEXPexp(i, atexp) => infer_atexp (C, atexp) errflag
       | RG.UNRES_INFIXexp _ => impossible "infer_exp:UNRES_INFIXexp"
       | _ =>   (* use default sort *)
           let val def_srt = findMLSort(C, RG.get_info_exp exp)  in
             letCV (check_exp (C, exp, def_srt))  (fn () =>
             noRedo def_srt ) errflag
           end

    and check_atexp0 (C, atexp : RG.atexp, gsrt : RO.Sort) errflag
           : unit Result = 
      case atexp of
        RG.IDENTatexp (i, RG.OP_OPT(longid, withOp)) =>
           let
             val instances = (case (getPostElabTypeInfo i)
                                  of SOME (TypeInfo.VAR_INFO {instances}) => instances
                                   | SOME (TypeInfo.CON_INFO {instances, ...}) => instances
                                   | SOME (TypeInfo.EXCON_INFO _) => []
                                   | _ => impossible "check_atexp0(1)")
             val srtsch =
                case Env.Lookup_longid(C, longid) 
                  of SOME(Env.LONGVAR sscheme) =>  sscheme
                   | SOME(Env.LONGCON sscheme) =>  sscheme
                   | SOME(Env.LONGEXCON srt) =>  RO.Sort_in_SortScheme srt
                   | NONE => impossible ("check_atexp0:IDENTatexp not found: " ^ 
                                         (Ident.pr_longid longid) )
             val sortInstances = map (RO.MLSortOfTy (TNtoSN C)) instances

             val isInstance =   (* Slightly staged computation. *)
                 if false (*List.all (RO.covariant_sort (covariant_sortname C)) sortInstances*) then
                     let val full_inst = full_instance (C, srtsch, instances)  in
                        fn srt => RO.subSort (Env.conjSortNameC C) (full_inst, srt)
                     end
                 else
                     fn srt => RO.isInstance (Env.conjSortNameC C) srt srtsch
           in
(*             if List.all isInstance (RO.list_Conjuncts gsrt) then noErrC () errflag *)
             if isInstance gsrt then noErrC () errflag        (* Must match closely! *)
             else let val (_, srt2) = RO.instance_vars srtsch
                  in  error((), RG.get_info_atexp atexp, REI.NOT_SUBSORT(gsrt, srt2)) errflag
                  end
           end

      | RG.INSTatexp (i, RG.OP_OPT(longid, withOp), tys) =>
           let
             val instances = (case (getPostElabTypeInfo i)
                                  of SOME (TypeInfo.VAR_INFO {instances}) => instances
                                   | SOME (TypeInfo.CON_INFO {instances, ...}) => instances
                                   | SOME (TypeInfo.EXCON_INFO _) => []
                                   | _ => impossible "check_atexp0(1)")
             val srtsch =
                case Env.Lookup_longid(C, longid) 
                  of SOME(Env.LONGVAR sscheme) =>  sscheme
                   | SOME(Env.LONGCON sscheme) =>  sscheme
                   | SOME(Env.LONGEXCON srt) =>  RO.Sort_in_SortScheme srt
                   | NONE => impossible ("check_atexp0:INSTatexp not found: " ^ 
                                         (Ident.pr_longid longid) )

             val srtsC = foldr (fn (ty,rest) => letC (ref_ty0 (C, ty)) (fn srt => letCV rest (fn r =>srt::r)) ) 
                               (noErrC []) 
                               tys
           in
             letC srtsC  ( fn srts=>  
             case rev srts of srts =>     (* Change to the more natural "order of last occurrence" *)             
             if RO.subSort (Env.conjSortNameC C) (RO.instance (srtsch, srts), gsrt) then noErrC ()
             else let val (_, srt2) = RO.instance_vars srtsch
                  in  error((), RG.get_info_atexp atexp, REI.NOT_SUBSORT(gsrt, srt2))
                  end ) errflag
           end

       | RG.LETatexp(i, dec, exp) =>
            letRC (ref_decR (C, dec))  (fn (T, E) =>
            check_exp (Env.C_plus_T (Env.C_plus_E(C,E), T), exp, gsrt)  ) errflag
       | RG.PARatexp(i, exp) => check_exp (C, exp, gsrt) errflag
       | RG.RECORDatexp(i, exprow_opt) =>
            (* Below: better would be to check with a record of bogusSorts.  *)
          if  RO.isBogusSort gsrt  then  letRC (infer_atexp(C,atexp))  (fn inf_srt =>
                                         noErrC ()  ) errflag
          else  
            let                
              val grecmap : (RO.lab, RO.Sort) SortedFinMap.map = 
                      case (RO.unSortRecSort gsrt)  of        (* mkConjSort distributes rec's *)
                         SOME recsort => RO.recSortToMap recsort
                       | NONE => impossible ("check_atexp:" ^
                                               (PP.flatten1 (SourceInfo.layout (to_SourceInfo i))))
            in
              check_exprow_opt (C, exprow_opt, grecmap) errflag
            end
       | _ => letRC (infer_atexp (C, atexp))  (fn inf_srt =>  (* infer sort and compare *)
              if (RO.subSort (Env.conjSortNameC C) (inf_srt, gsrt)) then
                 noErrC ()
              else
                 error((), RG.get_info_atexp atexp, REI.NOT_SUBSORT(gsrt, inf_srt))  ) errflag

    and infer_atexp0 (C, atexp : RG.atexp) errflag
           : RO.Sort Redo Result = 
      case atexp of
         RG.IDENTatexp (i, RG.OP_OPT(longid, withOp)) =>
           let
             val instances  = (case (getPostElabTypeInfo i)
                                  of SOME (TypeInfo.VAR_INFO {instances}) => instances
                                   | SOME (TypeInfo.CON_INFO {instances, ...}) => instances
                                   | SOME (TypeInfo.EXCON_INFO _) => []
                                   | _ => impossible "ref_atexp'(1)")

             val (srt,warn) =         (* Instance could be huge.  Use "local type inference"? *)
                case Env.Lookup_longid(C, longid) 
                  of SOME(Env.LONGVAR sscheme) =>  full_or_default_inst (C, sscheme, instances)
                   | SOME(Env.LONGCON sscheme) =>  full_or_default_inst (C, sscheme, instances)
                   | SOME(Env.LONGEXCON srt) =>  (srt,false)
                   | NONE => impossible ("infer_atexp:IDENTatexp not found: " ^ 
                                         (Ident.pr_longid longid) )
             in   (* Add warning here. *)
               if warn then error(noRedo srt, i, REI.TOO_MUCH) errflag
               else noErr (noRedo srt)
             end

       | RG.INSTatexp (i, RG.OP_OPT(longid, withOp),tys) =>
           let
             val instances  = (case (getPostElabTypeInfo i)
                                  of SOME (TypeInfo.VAR_INFO {instances}) => instances
                                   | SOME (TypeInfo.CON_INFO {instances, ...}) => instances
                                   | SOME (TypeInfo.EXCON_INFO _) => []
                                   | _ => impossible "ref_atexp'(1)")

             val srtsch =
                case Env.Lookup_longid(C, longid) 
                  of SOME(Env.LONGVAR sscheme) =>  sscheme
                   | SOME(Env.LONGCON sscheme) =>  sscheme
                   | SOME(Env.LONGEXCON srt) =>  RO.Sort_in_SortScheme srt
                   | NONE => impossible ("infer_atexp0:INSTatexp not found: " ^ 
                                         (Ident.pr_longid longid) )

             val srtsC = foldl (fn (ty,rest) => letC (ref_ty0 (C, ty)) (fn srt => letCV rest (fn r =>srt::r)) ) 
                               (noErrC []) 
                               tys
             in
                letCV srtsC  ( fn srts =>
                case rev srts of srts =>     (* Change to the more natural "order of last occurrence" *)
                (ListPair.app  (fn (s,t) => assert (eqTypes "INSTatexp: " (RO.tyOfSort s) t)) (srts,instances);
                 noRedo (RO.instance (srtsch, srts)) ) ) errflag
             end

       | RG.LETatexp(i, dec, exp) =>
           letR  (ref_decR (C, dec))  (fn (T, E) =>
           infer_exp (Env.C_plus_T (Env.C_plus_E(C,E), T), exp)  ) errflag
       | RG.PARatexp(i, exp) =>  infer_exp (C, exp) errflag
       | RG.RECORDatexp(i, exprow_opt) =>
           letRV (infer_exprow_opt(C, exprow_opt)) RO.mkSortRecSort errflag
       | RG.SCONatexp(i, scon) =>
           let        (* Elaboration succeeded, so no possible error *)
             val isWord8 = case ElabInfo.to_OverloadingInfo (to_ElabInfo i) (* for WORD/WORD8 *)
                             of SOME ElabInfo.OverloadingInfo.RESOLVED_WORD8 => ((*print "HERE\n"; *) true)
                              | _ => ((*print "THERE\n";*) false)
           in
             noErr (noRedo (RO.GetSortscon (scon, isWord8)))
           end

    and check_exp (C, exp : RG.exp, gsrt : RO.Sort) errflag
           : unit Result =
         let (*val _ = pr_debug ("check_exp: ", fn () => RG.layoutExp exp) 
             val _ = out_debug (fn () => "  sort for check_exp: "^ RO.pr_Sort gsrt) *)
             val _ = pr_debug ("  info for check_exp: ", 
                       fn () => case getPostElabTypeInfo (RG.get_info_exp exp) of
                                   NONE =>  StringTree.LEAF "NO-INFO"
                                 | SOME i =>  TypeInfo.layout i)
             val pop = Env.debug_push2 (fn () => ("\n****check_exp: sort = " ^ RO.pr_Sort gsrt)
                                               :: lines_pp (RG.layoutExp exp))
             val refDecMemo = RefInfo.to_RefDecMemo (RG.get_info_exp exp)
             val memotable = RefInfo.get_CHECKABLE refDecMemo (* Will add table if not there. *)
             val memo = RefInfo.lookupMemoVEsort ((Env.VE_of_E (Env.E_of_C C), gsrt), memotable)
             val res =  if !memoizeOn then Comp.memoIn memo (check_exp0 (C, exp, gsrt)) errflag
                        else check_exp0 (C, exp, gsrt) errflag
             val _ = pop (fn () => 
                                     ["  RESULT: " ^ (case res of (_, []) => "YES" | _ => "NO")] )

(*             val _ = pr_debug ("check_exp: ", fn () => RG.layoutExp exp)
             val _ = out_debug (fn () => "  sort for check_exp: "^ RO.pr_Sort gsrt)
             val _ = out_debug (fn () => "  RESULT: " ^ (case res of (_, []) => "YES" | _ => "NO"))
*)
         in   res  end

    and check_atexp (C, atexp : RG.atexp, gsrt : RO.Sort) errflag
           : unit Result =
         let val _ = pr_debug ("check_atexp: ", fn () => RG.layoutAtexp atexp)
             val _ = out_debug (fn () => "  sort for check_atexp: "^ RO.pr_Sort gsrt)
             val _ = pr_debug ("  info for check_atexp: ", 
                       fn () => case getPostElabTypeInfo (RG.get_info_atexp atexp) of
                                   NONE =>  StringTree.LEAF "NO-INFO"
                                 | SOME i =>  TypeInfo.layout i)
             val pop = Env.debug_push2 (fn () => ("\n****check_atexp: sort = " ^ RO.pr_Sort gsrt)
                                               :: lines_pp (RG.layoutAtexp atexp))

             val refDecMemo = RefInfo.to_RefDecMemo (RG.get_info_atexp atexp)
             val memotable = RefInfo.get_CHECKABLE refDecMemo (* Will add table if not there. *)
             val memo = RefInfo.lookupMemoVEsort ((Env.VE_of_E (Env.E_of_C C), gsrt), memotable)
             val res =  if !memoizeOn then Comp.memoIn memo (check_atexp0 (C, atexp, gsrt)) errflag
                        else check_atexp0 (C, atexp, gsrt) errflag
             val _ = pop (fn () => 
                                     ["  RESULT: " ^ (case res of (_, []) => "YES" | _ => "NO")] )
         in   res  end

    and infer_exp (C, exp : RG.exp) errflag
           : RO.Sort Redo Result = 
         let val _ = pr_debug ("  info for infer_exp: ", 
                       fn () => case getPostElabTypeInfo (RG.get_info_exp exp) of
                                   NONE =>  StringTree.LEAF "NO-INFO"
                                 | SOME i =>  TypeInfo.layout i)               
             val pop = Env.debug_push2 (fn () => ("\n****infer_exp: ")
                                               :: lines_pp (RG.layoutExp exp))
             val refDecMemo = RefInfo.to_RefDecMemo (RG.get_info_exp exp)
             val memotable = RefInfo.get_INFERABLE refDecMemo (* Will add table if not there. *)
             val memo = RefInfo.lookupMemoVE (Env.VE_of_E (Env.E_of_C C), memotable)
             val res = if !memoizeOn then Comp.memoIn memo (infer_exp0 (C, exp)) errflag
                       else infer_exp0 (C, exp) errflag
             val _ = pop (fn () => "infer_exp:  RESULT: " ::
                                      lines_pp (RO.layoutSort ((fn (redo, _) => 
                                                      #1 (redoToResAndRC redo)) res)  ))
         in   res  end

    and infer_atexp (C, atexp : RG.atexp) errflag
           : RO.Sort Redo Result = 
         let (* val _ = pr_debug ("  C for infer_exp: ", 
                             fn () => Env.layoutC C ) *)
             val _ = pr_debug ("  info for infer_atexp: ", 
                       fn () => case getPostElabTypeInfo (RG.get_info_atexp atexp) of
                                   NONE =>  StringTree.LEAF "NO-INFO"
                                 | SOME i =>  TypeInfo.layout i)
             val pop = Env.debug_push2 (fn () => ("\n****infer_atexp: ")
                                               :: lines_pp (RG.layoutAtexp atexp))
             val refDecMemo = RefInfo.to_RefDecMemo (RG.get_info_atexp atexp)
             val memotable = RefInfo.get_INFERABLE refDecMemo (* Will add table if not there. *)
             val memo = RefInfo.lookupMemoVE (Env.VE_of_E (Env.E_of_C C), memotable)
             val res = if !memoizeOn then Comp.memoIn memo (infer_atexp0 (C, atexp)) errflag
                       else infer_atexp0 (C, atexp) errflag
             val _ = pop (fn () => "infer_atexp:  RESULT: " ::
                                      lines_pp (RO.layoutSort ((fn (redo, _) => 
                                                      #1 (redoToResAndRC redo)) res)  ))

         in   res  end  
     
    and check_exprow_opt(C, exp_row_opt, grecmap : (RO.lab, RO.Sort) SortedFinMap.map) errflag
                        : unit Result = 
      case exp_row_opt of
         NONE =>  noErr ()
       | SOME (RG.EXPROW(i, lab, exp, exprow_opt)) => 
            case SortedFinMap.lookup grecmap lab  of SOME gsrt =>  (* could sort labs in exp *)
            letC (check_exp(C, exp, gsrt))  (fn () =>
            check_exprow_opt(C, exprow_opt, grecmap)  ) errflag

    (* Number of results is the product of the components. *)
    and infer_exprow_opt(C, exp_row_opt) errflag  : RO.RecSort Redo Result = 
        case exp_row_opt of
           NONE =>  noErr (noRedo RO.emptyRecSort)
         | SOME (RG.EXPROW(i, lab, exp, exprow_opt)) => 
              letR (infer_exp (C, exp))  (fn srt =>
              letRV (infer_exprow_opt (C, exprow_opt))  (fn recsrt =>
              RO.addField (lab, srt) recsrt  )) errflag       
              
    and apply_sort_to_atexp(C, srt: RO.Sort, atexp: RG.atexp) errflag
          : RO.Sort Redo Result = 
      case (RO.unSortConj srt)  of
         SOME (srt1, srt2) =>  tryBothR1 (apply_sort_to_atexp(C, srt1, atexp))
                                         (apply_sort_to_atexp(C, srt2, atexp)) errflag
       | NONE => case (RO.unSortArrow srt)  of
                    SOME (srt1, srt2) =>  letCV (check_atexp(C, atexp, srt1))  (fn () =>
                                                noRedo srt2  ) errflag
                  | NONE => Crash.impossible "RefDec.apply_sort_to_atexp:2"

(* UNFINISHED: was in apply_sort_to_atexp above.  Need RO.applySort to return a Redo Result
           if (always_infer_atexp atexp) then   (* optimize when argument is trivial? *)
             letR (infer_atexp (C, atexp))  (fn (T, srt2) => 
             letR (RO.applySort (Env.conjSortNameC C) (srt, srt2))  
             (fn NONE => error((T, RO.bogusSort), get_info_atexp atexp, REI.CANT_APPLY srt)
               | SOME out_srt => noErr (T, out_srt)  )) errflag
           else
*)

   and always_infer_exp (RG.ATEXPexp(_, atexp)) = always_infer_atexp atexp
     | always_infer_exp (RG.APPexp(_, RG.FNexp _, atexp)) = false
     | always_infer_exp (RG.APPexp(_, exp, atexp)) = true  (* Could be false... *)
     | always_infer_exp (RG.SORTEDexp(_, exp, _)) = true
     | always_infer_exp _ = false
   and always_infer_atexp (RG.SCONatexp _) = true
     | always_infer_atexp (RG.IDENTatexp _) = true
     | always_infer_atexp (RG.INSTatexp _) = true
     | always_infer_atexp (RG.PARatexp (_, exp)) = always_infer_exp exp
     | always_infer_atexp _ = false

   and exp_has_var_head (RG.ATEXPexp(_, atexp)) = atexp_has_var_head atexp
     | exp_has_var_head (RG.APPexp(_, exp, _)) = exp_has_var_head exp
     | exp_has_var_head _ = false

   and atexp_has_var_head (RG.IDENTatexp _) = true
     | atexp_has_var_head (RG.INSTatexp _) = true
     | atexp_has_var_head (RG.PARatexp(_, exp)) = exp_has_var_head exp
     | atexp_has_var_head _ = false

   and check_poly_head_exp (C, exp, gsrt, apps) errflag = 
       case exp
         of RG.ATEXPexp (i, atexp) => check_poly_head_atexp (C, atexp, gsrt, apps) errflag
          | RG.APPexp(i, exp1, atexp2) =>
             letR (check_poly_head_exp(C, exp1, gsrt, apps+1))  (fn srt1 =>
             apply_sort_to_atexp(C, srt1, atexp2)  ) errflag

   and check_poly_head_atexp (C, atexp, gsrt, apps) errflag = 
     case atexp
       of RG.PARatexp (i, exp) => check_poly_head_exp (C, exp, gsrt, apps) errflag
        | RG.IDENTatexp (i, RG.OP_OPT(longid, withOp)) =>  (* Based on check_atexp above *)
           let
             val instances = (case (getPostElabTypeInfo i)
                                  of SOME (TypeInfo.VAR_INFO {instances}) => instances
                                   | SOME (TypeInfo.CON_INFO {instances, ...}) => instances
                                   | SOME (TypeInfo.EXCON_INFO _) => []
                                   | _ => impossible "check_atexp0(1)")
             val srtsch =
                case Env.Lookup_longid(C, longid) 
                  of SOME(Env.LONGVAR sscheme) =>  sscheme
                   | SOME(Env.LONGCON sscheme) =>  sscheme
                   | SOME(Env.LONGEXCON srt) =>  RO.Sort_in_SortScheme srt
                   | NONE => impossible ("check_poly_head_atexp:IDENTatexp not found: " ^ 
                                         (Ident.pr_longid longid) )
             val sortInstances = map (RO.MLSortOfTy (TNtoSN C)) instances

             val isInstance =   (* Slightly staged computation. *)
                 if List.all (RO.covariant_sort (covariant_sortname C)) sortInstances then
                     let val full_inst = full_instance (C, srtsch, instances)  in  (* Could raise TooMuch *)
                        fn srt => RO.subSort (Env.conjSortNameC C) (full_inst, srt)
                     end
                 else (* Instead of srtsch, we want to take a parametric instance,
                         perhaps match against some arguments (just variables?),  
                         take the result of "->" apps times, then match against gsrt, 
                         and use the resulting substitution.  *)
                     fn srt => RO.isInstance (Env.conjSortNameC C) srt srtsch
             val out_srt = noRedo gsrt (* This is wrong: it should be an instance of srtsch *)
           in
             if List.all isInstance (RO.list_Conjuncts gsrt) then noErrC out_srt errflag
             else let val (_, srt2) = RO.instance_vars srtsch
                  in  error(out_srt, RG.get_info_atexp atexp, REI.NOT_SUBSORT(gsrt, srt2)) errflag
                  end
           end

      (* | RG.IDENTatexp (i, RG.OP_OPT(longid, withOp)) =>  ??? *)


   and ref_ty0 (C, ty) errflag = 
          ref_ty(C, ty, (true, false)) errflag

(*  An attempt at keeping track of goal sorts for applications via a reference.
   and push_apply_gsrt gsrt_opt = apply_goal_sorts := gsrt_opt :: (!apply_goal_sorts)
   and pop_apply_gsrt () = apply_goal_sorts := (case !apply_goal_sorts of _::tgsrts => tgsrts)
*)

   (* external ref_ty_covar *)
   fun ref_ty_covar(C, ty) errflag = 
          ref_ty0(C, ty_toRG ty) errflag


end  (* of struct *)
end; (* of let *)

