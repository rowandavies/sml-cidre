
signature OS = 
  sig
    type syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror option

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO
  end

(*  Various functions for interacting with the operating system.

   [errorMsg err] returns a string explaining the error message system
   error code err, as found in a SysErr exception.  The precise form
   of the strings are operating system dependent.  
*)
structure OS:OS =
  struct(*[ assumesig OS ]*)
  end

