
(* RefineCheck is the top level for the refinement types implementation.
 * It is originally based on "KitCompiler", with parts of "Manager"
 * merged in.   
 * THIS FILE IS USED IN CURRENT VERSIONS (but not in some previous versions)  *)

functor RefineCheck() (* : sig include PARSE_ELAB
			    structure Flags : FLAGS
			    structure Crash : CRASH
			   end  *) = 
  struct
    structure Tools   = Tools()
    structure Basics  = Basics(structure Tools = Tools)
    structure AllInfo = Basics.AllInfo
    structure TopdecParsing  = TopdecParsing(structure Basics = Basics)      
    structure Elaboration = Elaboration(structure TopdecParsing = TopdecParsing)
    structure Flags = Tools.Flags
    structure Crash = Tools.Crash

    (* I moved the next two structures from the ParseElab args, for debugging.  Rowan 9jan03 *)
    structure ErrorTraverse = ErrorTraverse
	 (structure TopdecGrammar = Elaboration.PostElabTopdecGrammar
	  structure ElabInfo = AllInfo.ElabInfo
	  structure Report = Tools.Report
	  structure PrettyPrint = Tools.PrettyPrint
	  structure Crash = Tools.Crash)

    structure TopLevelReport =
		      TopLevelReport(structure FunId = Basics.FunId
				     structure SigId = Basics.SigId
				     structure StrId = Basics.StrId
				     structure Ident = Basics.Ident
				     structure InfixBasis = TopdecParsing.Basics.InfixBasis
				     structure StatObject = Basics.StatObject
				     structure Environments = Basics.Environments
				     structure ModuleStatObject = Basics.ModuleStatObject
				     structure ModuleEnvironments = Basics.ModuleEnvironments
				     structure RefObject = Basics.RefObject
				     structure RefinedEnvironments = Basics.RefinedEnvironments
				     structure Report = Tools.Report
				     structure Crash = Tools.Crash)


    structure ParseElab = ParseElab
      (structure Parse = TopdecParsing.Parse
       structure Timing = Tools.Timing
       structure ElabTopdec = Elaboration.ElabTopdec
       structure ModuleEnvironments = Basics.ModuleEnvironments
       structure PreElabTopdecGrammar = TopdecParsing.PreElabTopdecGrammar
       structure PostElabTopdecGrammar = Elaboration.PostElabTopdecGrammar
       structure ErrorTraverse = ErrorTraverse
       structure RefineErrorInfo = AllInfo.RefineErrorInfo
       structure InfixBasis = TopdecParsing.Basics.InfixBasis
       structure TopLevelReport = TopLevelReport
       structure BasicIO = Tools.BasicIO
       structure Report = Tools.Report
       structure PP = Tools.PrettyPrint
       structure Flags = Tools.Flags
       structure Crash = Tools.Crash)
      
      open ParseElab


    val cd = OS.FileSys.chDir
    val pwd = OS.FileSys.getDir

  end;


structure RefineCheck = RefineCheck ()

(* The structure R is available at top level.  This is handy for debugging. *)

structure R = RefineCheck



(*                     Previously this was split up
structure R = struct
  open RefineCheck

  val cd = OS.FileSys.chDir
  val pwd = OS.FileSys.getDir

end (* structure for RefineCheck *)
*)
