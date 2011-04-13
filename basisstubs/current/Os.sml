
structure OS:OS =
struct(*[ assumesig OS ]*)
type syserror = unit

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string = fn _ => raise Match

    structure FileSys : OS_FILE_SYS = FileSys
    structure Path : OS_PATH = Path
    structure Process : OS_PROCESS = Process
  end

(*  Various functions for interacting with the operating system.

   [errorMsg err] returns a string explaining the error message system
   error code err, as found in a SysErr exception.  The precise form
   of the strings are operating system dependent.  
*)

structure OS : OS = 
  struct(*[ assumesig OS ]*)
type syserror = unit
    exception SysErr = OS.SysErr
    fun errorMsg (err : int) : string = OS.errorMsg err

    structure FileSys = FileSys
    structure Path = Path
    structure Process = Process
  end

