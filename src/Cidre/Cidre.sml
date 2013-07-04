
structure Environment = Environment(fun program_name ()="cidre");
structure MlbProject = MlbProject(Environment);

structure Cidre = struct

   (* structure Flags = Flags *)
   open R

   local
     open InfixingLib
     open Basics Tools
     open Basics.ModuleStatObject Basics.ModuleEnvironments
     open MlbProject.MS
     (* structure B = Basis*)
     structure InfixBasis = Basics.InfixBasis
     structure rEnv = RefinedEnvironments and PP = PrettyPrint
     structure Env = Environments

     type BasId = Bid.bid  (* Could instead create it's own module based on, e.g., SigId *)
     datatype BasEnv = BASENV of (BasId, InfixBasis * BasEnv * Basis) FinMap.map
     type MBasis = InfixBasis * BasEnv * Basis 

     structure BE = struct                  (* MLB Basis Environments *)
       type t = BasEnv
       val empty : t = BASENV FinMap.empty
       val singleton : (Bid.bid * MBasis) -> t = BASENV o FinMap.singleton
       fun plus (BASENV BEm, BASENV BEm') = BASENV (FinMap.plus (BEm, BEm'))
       fun lookup (BASENV BEm) = FinMap.lookup BEm
       fun lookup_long MB [] = SOME MB
         | lookup_long (_,BE,_) (bid::bids) = Option.join (Option.map (fn MB => lookup_long MB bids) (lookup BE bid))

       fun dom (BASENV BE) = FinMap.dom BE
(*       fun layout (BASENV m) =
             let val l = FinMap.Fold (op ::) nil m

             fun format_id basid =
                   concat ["basenv ", Basid.pr_Basid basid, " : "]

             fun layoutPair (basid, MBasis) = 
                   PP.NODE {start=format_id basid, finish="", indent=3,
                            children=[],   (*[MBasis.layout MBasis],*)
                            childsep=PP.NOSEP}
             in 
               (case l of
                 [] => PP.LEAF ""
               | _ =>
                   PP.NODE {start="", finish="", indent=0, 
                            children=map layoutPair l, childsep=PP.RIGHT " "})
             end *)
 (*      fun report (report_basid_Sigma : basid * Bas -> Report, BASENV m) =
             FinMap.reportMapSORTED (Basid.<) report_basid_Sigma m

       fun tynames (BASENV BE) = 
             FinMap.fold
               (fn (Bas, T) =>
                     TyName.Set.union T (Sigma.tynames Sig))
                 TyName.Set.empty  BE

       fun on rea (BE as BASENV m) =
         if Realisation.is_Id rea then BE
         else BASENV (FinMap.composemap (fn Sigma => Sigma.on (rea,Sigma)) m)

       (* val pu = Pickle.convert (BASENV, fn BASENV m => m) 
           (FinMap.pu (Basid.pu,Sigma.pu)) *)
*)

     end (*BE*)

     val initialElabB = !R.currentElabB

     structure MB = struct
       val empty = (InfixBasis.emptyB, BE.empty, B.empty)
       val initial = (!R.currentInfB, BE.empty, B.no_rT initialElabB)
       fun plus ( (infE1,BE1,B1),  (infE2,BE2,B2) ) = 
             (InfixBasis.compose (infE1,infE2), BE.plus (BE1,BE2), B.plus(B1,B2))
     end

     type MLBasisPath = string
     datatype BCache = BCACHE of (MLBasisPath, MBasis) FinMap.map

     structure BC = struct                  (* MLB Basis Caches *)
       val empty = BCACHE FinMap.empty
       val singleton = BCACHE o FinMap.singleton
       fun plus (BCACHE BC, BCACHE BC') = BCACHE (FinMap.plus (BC, BC'))
       fun lookup (BCACHE BC) = FinMap.lookup BC
       fun dom (BCACHE BC) = FinMap.dom BC
       (*fun layout (BCACHE m) =
             let val l = FinMap.Fold (op ::) nil m

             fun format_id mlbpath =
                   concat ["mlbpath ", mlbpath, " : "]

             fun layoutPair (mlbpath, MBasis) = 
                   PP.NODE {start=format_id mlbpath, finish="", indent=3,
                            children=[],   (*[MBasis.layout MBasis],*)
                            childsep=PP.NOSEP}
             in
               (case l of
                 [] => PP.LEAF ""
               | _ =>
                   PP.NODE {start="", finish="", indent=0, 
                            children=map layoutPair l, childsep=PP.RIGHT " "})
             end *)
 (*      fun report (report_basid_Sigma : basid * Bas -> Report, BASENV m) =
             FinMap.reportMapSORTED (Basid.<) report_basid_Sigma m

       fun tynames (BASENV BE) = 
             FinMap.fold
               (fn (Bas, T) =>
                     TyName.Set.union T (Sigma.tynames Sig))
                 TyName.Set.empty  BE

       fun on rea (BE as BASENV m) =
         if Realisation.is_Id rea then BE
         else BASENV (FinMap.composemap (fn Sigma => Sigma.on (rea,Sigma)) m)

       (* val pu = Pickle.convert (BASENV, fn BASENV m => m) 
           (FinMap.pu (Basid.pu,Sigma.pu)) *)
*)

     end (*BC*)



(*  
    type atbdec = string (* path.{sml,sig} *)
    datatype bexp = BASbexp of bdec
                  | LETbexp of bdec * bexp
                  | LONGBIDbexp of Bid.longbid

    bdec = SEQbdec of bdec * bdec
             | EMPTYbdec 
             | LOCALbdec of bdec * bdec
             | BASISbdec of Bid.bid * bexp
             | OPENbdec of Bid.longbid list
             | ATBDECbdec of atbdec
             | MLBFILEbdec of string * string option  (* path.mlb <scriptpath p> *)
             | SCRIPTSbdec of atbdec list
             | ANNbdec of string * bdec
*)
     exception Failed of Report

     infixr @@    fun rT1 @@ rT2 = rEnv.T_plus_T (rT1, rT2)


     fun (*resolveMLBpath "$(SML_LIB)/basis/basis.mlb" = "../../lib/empty.mlb"
       | *) resolveMLBpath p = p (* This is already done by the parser - but the Env needs to be fixed. *)


     val print_flag = false  (* convert this to an argument or flag - maybe. *)

     fun print_error_report report = 
            Report.print' (Report.// (Report.line "\nCIDRE: ***************** Errors *****************",
					report) )
							(!Flags.log)
     fun print_result_report report = (Report.print' report (!Flags.log);
					    Flags.report_warnings ())
     val warned = ref false

     fun checkBdec (rT, MB, BC) (SEQbdec(bdec1, bdec2)) = 
           let val (rT1, MB1,BC1) = checkBdec (rT, MB,BC) bdec1
               val (rT2, MB2, BC2) = checkBdec (rT @@ rT1, MB.plus(MB,MB1), BC1) bdec2
           in  (rT1@@rT2, MB.plus(MB1, MB2), BC2)
           end
       | checkBdec (rT, MB, BC) EMPTYbdec = (rEnv.emptyT, MB.empty, BC)
       | checkBdec (rT, MB, BC) (LOCALbdec (bdec1,bdec2)) =   (* The MLB spec doesn't keep T1 here. (!) *)  
           let val (rT1, MB1,BC1) = checkBdec (rT, MB,BC) bdec1
               val (rT2, MB2, BC2) = checkBdec (rT@@rT1, MB.plus(MB,MB1), BC1) bdec2
           in  (rT1@@rT2, MB2, BC2)
           end
       | checkBdec (rT, MB, BC) (BASISbdec (bid, bexp))  = 
           let val (rT1, MB1, BC1) = checkBexp (rT, MB, BC) bexp in
               (rT1, (InfixBasis.emptyB, BE.singleton (bid, MB1), B.empty), BC1)
           end   
       | checkBdec (rT, MB, BC) (OPENbdec longbids) = 
           let val MB2 = 
             List.foldl (fn (longbid,sofar) => (case BE.lookup_long MB (Bid.explode longbid) of NONE => 
                                                       raise Failed (Report.line ("Can't open \"" ^ 
                                                                                  Bid.pp_longbid longbid ^ "\"") )
                                                   | SOME MB1 => MB.plus (sofar, MB1) ))
                        MB.empty
                        longbids
           in (rEnv.emptyT, MB2, BC)
           end

       | checkBdec ( rT0, (infB0,BE0,B0), BC0 ) (ATBDECbdec atbdec) =   (* FIX: resolve atbdec *)
           (case ParseElab.parse_elab {infB=infB0, elabB=B.plus_rT(B0,rT0), prjid=atbdec, file=atbdec}
              of SUCCESS {report, infB, elabB, topdec} => 
                   (if print_flag then print_result_report report 
                                  else ( warned := ((!warned) orelse (Flags.get_warned())) ;
                                         Flags.report_warnings(); Flags.reset_warnings()   );
                    ( B.to_rT elabB, (infB, BE.empty, B.no_rT elabB), BC0 ) )
               | FAILURE (report, error_codes) =>
                   (Flags.report_warnings();  (* Print warnings even if there are errors.  *)
                    (* Report.print (PP.reportStringTree (B.layout (B.plus_rT(B0,rT0) ))); *)
                    raise Failed report)
            )
       | checkBdec (rT,MB,BC) (MLBFILEbdec(path,NONE)) =
          let (* val _ = print (MlbFileSys.getCurrentDir() ^ " path = " ^ path ^ "\n" ) *)
              val _ = print ("CIDRE: MLB " ^ path ^" \n")
              val {cd_old,file=mlbfile} = MlbFileSys.change_dir (OS.FileSys.fullPath path)
                  handle OS.SysErr _ => 
                       case OS.Path.splitBaseExt path of {base,ext} => 
                            MlbFileSys.change_dir (OS.FileSys.fullPath (base ^ ".cm"))
                            handle OS.SysErr _ => raise Failed (Report.line ("Path not found: " ^ path))
              val pathAbs = OS.FileSys.fullPath mlbfile
              (* val {dir,file} = case OS.Path.splitDirFile path *)
              (*    handle OS.SysErr (errstr, _) => (cd_old(); raise Failed (Report.line ("File not found: " ^ path))) *)
              (* val _ = print (pathAbs ^ "\n") *)
                    (* handle _ => CM2MLB.mlbString (base ^ ".cm") *)
                    (*             handle _ => raise Failed (Report.line ("File not found: " ^ path)) *)

              val res = 
            (case BC.lookup BC pathAbs of SOME MB1 => (rEnv.emptyT, MB1, BC)  (* typenames are already in rT *)
                | NONE => let val {base,ext}= OS.Path.splitBaseExt pathAbs
                              val (base,contents) = case OS.Path.splitBaseExt pathAbs 
                                               of {base, ext=SOME "cm"} => (base,CM2MLB.mlbString pathAbs)
                                                | {base,...} => (base,MlbFileSys.fromFile pathAbs)
                              val bdec = MlbProject.parseContents(base ^ ".mlb", contents)
                              val (rT1, MB1, BC1) = checkBdec (rT,MB.initial,BC) bdec
                          in  ( rT1, MB1, BC.plus (BC1, BC.singleton (pathAbs,MB1)) )
                          end)  handle ex => (cd_old(); raise ex)
              val _ = cd_old()
          in res
          end

       | checkBdec (rT,MB,BC) (MLBFILEbdec(filename,_))= Crash.impossible "checkBdec:MLBFILEbdec"
       | checkBdec _ (SCRIPTSbdec _) = Crash.impossible "checkBdec:SCRIPTSbdec"
       | checkBdec rTMBBC (ANNbdec (ann,bdec)) = (* Fix! *) checkBdec rTMBBC bdec

       | checkBdec (rT,(_,_,B),BC) (STRbdec id_pairs) = 
           let fun B_plus_idpair B0 (id1,id2) = 
                case (B.lookup_strid B (StrId.mk_StrId id2), B.lookup_rstrid B (StrId.mk_StrId id2))
                  of (SOME E, SOME rE) => B.plus_rE ( B.plus_E (B0, Env.E.from_SE(Env.SE.singleton(StrId.mk_StrId id1,E))), 
                                                      rEnv.SE_in_E(rEnv.singleSE(StrId.mk_StrId id1,rE)) )
                   | _ => raise Failed (Report.line ("Structure not found: " ^ id2))
               val newB = List.foldl (fn ((id1,id2), sofar) => B_plus_idpair sofar (id1,id2)) B.empty id_pairs
           in 
              (rEnv.emptyT, (InfixBasis.emptyB, BE.empty, newB), BC)
           end

       | checkBdec (rT,(_,_,B),BC) (SIGbdec id_pairs) = 
           let fun B_plus_idpair B0 (id1,id2) = 
                case B.lookup_sigid B (SigId.mk_SigId id2)
                  of SOME Sig => B.plus_G (B0, G.singleton(SigId.mk_SigId id1,Sig))
                   | _ => raise Failed (Report.line ("Signature not found: " ^ id2))
               val newB = List.foldl (fn ((id1,id2), sofar) => B_plus_idpair sofar (id1,id2)) B.empty id_pairs
           in 
              (rEnv.emptyT, (InfixBasis.emptyB, BE.empty, newB), BC)
           end

       | checkBdec (rT,(_,_,B),BC) (FUNbdec id_pairs) = 
           let fun B_plus_idpair B0 (id1,id2) = 
                case B.lookup_funid B (FunId.mk_FunId id2)
                  of SOME prjidFun => B.plus_F (B0, F.singleton(FunId.mk_FunId id1,prjidFun))
                   | _ => raise Failed (Report.line ("Functor not found: " ^ id2))
               val newB = List.foldl (fn ((id1,id2), sofar) => B_plus_idpair sofar (id1,id2)) B.empty id_pairs
           in 
              (rEnv.emptyT, (InfixBasis.emptyB, BE.empty, newB), BC)
           end

   and checkBexp (rT, MB, BC) (BASbexp bdec) = checkBdec (rT, MB, BC) bdec
     | checkBexp (rT, MB, BC) (LETbexp (bdec1, bexp2)) = 
           let val (rT1, MB1,BC1) = checkBdec (rT, MB,BC) bdec1
               val (rT2, MB2, BC2) = checkBexp (rT@@rT1, MB.plus(MB,MB1), BC1) bexp2
           in  (rT1@@rT2, MB2, BC2)
           end
     | checkBexp (rT, MB, BC) (LONGBIDbexp longbid) =
          (case BE.lookup_long MB (Bid.explode longbid) of NONE => raise Failed (Report.line "Longbid not found")
              | SOME MB1 => (rEnv.emptyT, MB1, BC) )
   in
     val // = Report.//
     infix //
     fun reportErr report = Report.print (Report.line "\nCIDRE: basis construction error: " 
                                                   // Report.indent (4, report) // Report.line "")
     fun checkContents (mlbFileNm,contents) = 
         let val _ = Report.print (Report.line "")
             val _ = Flags.reset_warnings ()
             val _ = warned:=false;
             val bdec = MlbProject.parseContents (mlbFileNm, contents)
             val (rT, (infB, BE, B), BC) = checkBdec (B.to_rT initialElabB, MB.initial, BC.empty) bdec
             val _ = R.currentInfB := InfixBasis.compose (!R.currentInfB, infB);
             val _ =   R.currentElabB:=B.plus (!currentElabB, B.plus_rT(B.erase_TG B, rT))
          in
             print "\nCIDRE: finished checking.\n";
             not (!warned)
          end handle Failed report => (reportErr report; false)

     fun checkcm cmfilenm = 
         let fun doit base = checkContents (base^".mlb",  cmfilenm) in 
             case OS.Path.splitBaseExt cmfilenm 
               of {base, ext=SOME "cm"} => doit base
                | {base, ext=SOME "CM"} => doit base
                | _ =>(print ("\nCIDRE: error: "^cmfilenm^" does not have the extension .cm\n"); 
                       false)
         end

     fun checkmlb mlbFileNm = 
         let val _ = Flags.reset_warnings ()
             val _ = warned:=false;
             val (rT, (infB, BE, B), BC) = 
                    checkBdec (B.to_rT initialElabB, MB.initial, BC.empty) (MLBFILEbdec(mlbFileNm,NONE))
             val _ = R.currentInfB := InfixBasis.compose (!R.currentInfB, infB);
             val _ =   R.currentElabB:=B.plus (!currentElabB, B.plus_rT(B.erase_TG B, rT))
          in
             print "\nCIDRE: finished checking.\n";
             not (!warned)
          end handle Failed report => (reportErr report; false)
     end

     fun check filenm = 
             case OS.Path.splitBaseExt filenm 
               of {base, ext=SOME "mlb"} => checkmlb filenm
                | {base, ext=SOME "cm"} => checkcm filenm
                | {base, ext=SOME "MLB"} => checkmlb filenm
                | {base, ext=SOME "CM"} => checkcm filenm
                | {base, ext=SOME "sml"} => (R.refine_file filenm; false)
                | {base, ext=SOME "sig"} => (R.refine_file filenm; false)
                | {base, ext=SOME "fun"} => (R.refine_file filenm; false)
                | _ => (print ("\nCIDRE: error: "^filenm^" does not have one of the extensions: mlb cm sml sig fun\n");
                        false)

     fun to_cm filenm = case OS.Path.splitBaseExt filenm of {base, ext=_} => base ^ ".cm"

     fun make filenm = 
         if check filenm then CM.make (to_cm filenm)
         else false

end
                  
 
