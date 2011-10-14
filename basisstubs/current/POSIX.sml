

signature POSIX_ERROR = sig
type syserror = OS.syserror

val toWord   : syserror -> SysWord.word
val fromWord : SysWord.word -> syserror

val errorMsg : syserror -> string
val errorName : syserror -> string
val syserror : string -> syserror option

val acces       : syserror
val again       : syserror
val badf        : syserror
val badmsg      : syserror
val busy        : syserror
val canceled    : syserror
val child       : syserror
val deadlk      : syserror
val dom         : syserror
val exist       : syserror
val fault       : syserror
val fbig        : syserror
val inprogress  : syserror
val intr        : syserror
val inval       : syserror
val io          : syserror
val isdir       : syserror
val loop        : syserror
val mfile       : syserror
val mlink       : syserror
val msgsize     : syserror
val nametoolong : syserror
val nfile       : syserror
val nodev       : syserror
val noent       : syserror
val noexec      : syserror
val nolck       : syserror
val nomem       : syserror
val nospc       : syserror
val nosys       : syserror
val notdir   : syserror
val notempty : syserror
val notsup   : syserror
val notty    : syserror
val nxio     : syserror
val perm     : syserror
val pipe     : syserror
val range    : syserror
val rofs     : syserror
val spipe    : syserror
val srch     : syserror
val toobig   : syserror
val xdev     : syserror
end


signature POSIX_SIGNAL = sig
eqtype signal

val toWord   : signal -> SysWord.word
val fromWord : SysWord.word -> signal

val abrt : signal
val alrm : signal
val bus  : signal
val fpe  : signal
val hup  : signal
val ill  : signal
val int  : signal
val kill : signal
val pipe : signal
val quit : signal
val segv : signal
val term : signal
val usr1 : signal
val usr2 : signal
val chld : signal
val cont : signal
val stop : signal
val tstp : signal
val ttin : signal
val ttou : signal
end


signature POSIX_PROCESS = sig
eqtype signal
eqtype pid
val wordToPid : SysWord.word -> pid
val pidToWord : pid -> SysWord.word
val fork : unit -> pid option
val exec  : string * string list -> 'a
val exece : string * string list * string list -> 'a
val execp : string * string list -> 'a
datatype waitpid_arg
  = W_ANY_CHILD
  | W_CHILD of pid
  | W_SAME_GROUP
  | W_GROUP of pid
datatype exit_status
  = W_EXITED
  | W_EXITSTATUS of Word8.word
  | W_SIGNALED of signal
  | W_STOPPED of signal
val fromStatus : OS.Process.status -> exit_status
structure W : sig
    include BIT_FLAGS
    val untraced : flags
  end
val wait : unit -> pid * exit_status
val waitpid : waitpid_arg * W.flags list
                -> pid * exit_status
val waitpid_nh : waitpid_arg * W.flags list
                   -> (pid * exit_status) option
val exit : Word8.word -> 'a
datatype killpid_arg
  = K_PROC of pid
  | K_SAME_GROUP
  | K_GROUP of pid
val kill : killpid_arg * signal -> unit
val alarm : Time.time -> Time.time
val pause : unit -> unit
val sleep : Time.time -> Time.time
end

signature POSIX_PROC_ENV = sig
eqtype pid
eqtype uid
eqtype gid
eqtype file_desc
val uidToWord : uid -> SysWord.word
val wordToUid : SysWord.word -> uid
val gidToWord : gid -> SysWord.word
val wordToGid : SysWord.word -> gid
val getpid  : unit -> pid
val getppid : unit -> pid
val getuid  : unit -> uid
val geteuid : unit -> uid
val getgid  : unit -> gid
val getegid : unit -> gid
val setuid : uid -> unit
val setgid : gid -> unit
val getgroups : unit -> gid list
val getlogin : unit -> string
val getpgrp : unit -> pid
val setsid : unit -> pid
val setpgid : {pid : pid option, pgid : pid option} -> unit
val uname : unit -> (string * string) list
val time : unit -> Time.time
val times : unit
              -> {
                elapsed : Time.time,
                utime : Time.time,
                stime : Time.time,
                cutime : Time.time,
                cstime : Time.time
              }
val getenv : string -> string option
val environ : unit -> string list
val ctermid : unit -> string
val ttyname : file_desc -> string
val isatty : file_desc -> bool
val sysconf : string -> SysWord.word
end

signature POSIX_FILE_SYS = sig
eqtype uid
eqtype gid
eqtype file_desc

val fdToWord : file_desc -> SysWord.word
val wordToFD : SysWord.word -> file_desc
val fdToIOD : file_desc -> OS.IO.iodesc
val iodToFD : OS.IO.iodesc -> file_desc option

type dirstream

val opendir : string -> dirstream
val readdir : dirstream -> string option
val rewinddir : dirstream -> unit
val closedir : dirstream -> unit

val chdir : string -> unit
val getcwd : unit -> string

val stdin  : file_desc
val stdout : file_desc
val stderr : file_desc

structure S : sig
    eqtype mode
    include BIT_FLAGS
      where type flags = mode
    
    val irwxu : mode
    val irusr : mode
    val iwusr : mode
    val ixusr : mode
    val irwxg : mode
    val irgrp : mode
    val iwgrp : mode
    val ixgrp : mode
    val irwxo : mode
    val iroth : mode
    val iwoth : mode
    val ixoth : mode
    val isuid : mode
    val isgid : mode
  end

structure O : sig
    include BIT_FLAGS
    
    val append : flags
    val excl : flags
    val noctty : flags
    val nonblock : flags
    val sync : flags
    val trunc : flags
  end

datatype open_mode
  = O_RDONLY
  | O_WRONLY
  | O_RDWR

val openf   : string * open_mode * O.flags -> file_desc
val createf : string * open_mode * O.flags * S.mode
                -> file_desc
val creat : string * S.mode -> file_desc

val umask : S.mode -> S.mode
val link : {old : string, new : string} -> unit
val mkdir : string * S.mode -> unit
val mkfifo : string * S.mode -> unit
val unlink : string -> unit
val rmdir : string -> unit
val rename : {old : string, new : string} -> unit
val symlink : {old : string, new : string} -> unit
val readlink : string -> string

eqtype dev

val wordToDev : SysWord.word -> dev
val devToWord : dev -> SysWord.word

eqtype ino

val wordToIno : SysWord.word -> ino
val inoToWord : ino -> SysWord.word

structure ST : sig
    type stat
    
    val isDir  : stat -> bool
    val isChr  : stat -> bool
    val isBlk  : stat -> bool
    val isReg  : stat -> bool
    val isFIFO : stat -> bool
    val isLink : stat -> bool
    val isSock : stat -> bool
    val mode : stat -> S.mode
    val ino : stat -> ino
    val dev : stat -> dev
    val nlink : stat -> int
    val uid : stat -> uid
    val gid : stat -> gid
    val size : stat -> Position.int
    val atime : stat -> Time.time
    val mtime : stat -> Time.time
    val ctime : stat -> Time.time
  end

val stat  : string -> ST.stat
val lstat : string -> ST.stat
val fstat : file_desc -> ST.stat

datatype access_mode = A_READ | A_WRITE | A_EXEC

val access : string * access_mode list -> bool

val chmod : string * S.mode -> unit
val fchmod : file_desc * S.mode -> unit
val chown : string * uid * gid -> unit
val fchown : file_desc * uid * gid -> unit
val utime : string
              * {actime : Time.time, modtime : Time.time} option
              -> unit
val ftruncate : file_desc * Position.int -> unit

val pathconf  : string * string -> SysWord.word option
val fpathconf : file_desc * string -> SysWord.word option
end

signature POSIX_IO = sig
eqtype file_desc
eqtype pid

val pipe : unit -> {infd : file_desc, outfd : file_desc}
val dup : file_desc -> file_desc
val dup2 : {old : file_desc, new : file_desc} -> unit
val close : file_desc -> unit

val readVec : file_desc * int -> Word8Vector.vector
val readArr : file_desc * Word8ArraySlice.slice -> int
val writeVec : file_desc * Word8VectorSlice.slice -> int
val writeArr : file_desc * Word8ArraySlice.slice -> int

datatype whence
  = SEEK_SET
  | SEEK_CUR
  | SEEK_END

structure FD : sig
    include BIT_FLAGS
    val cloexec : flags
  end

structure O : sig
    include BIT_FLAGS
    val append : flags
    val nonblock : flags
    val sync : flags
  end

datatype open_mode
  = O_RDONLY
  | O_WRONLY
  | O_RDWR

val dupfd : {old : file_desc, base : file_desc}
              -> file_desc
val getfd : file_desc -> FD.flags
val setfd : file_desc * FD.flags -> unit
val getfl : file_desc -> O.flags * open_mode
val setfl : file_desc * O.flags -> unit

val lseek : file_desc * Position.int * whence
              -> Position.int
val fsync : file_desc -> unit

datatype lock_type
  = F_RDLCK
  | F_WRLCK
  | F_UNLCK

structure FLock : sig
    type flock
    val flock : {
                    ltype : lock_type,
                    whence : whence,
                    start : Position.int,
                    len : Position.int,
                    pid : pid option
                  } -> flock
    val ltype  : flock -> lock_type
    val whence : flock -> whence
    val start  : flock -> Position.int
    val len    : flock -> Position.int
    val pid    : flock -> pid option
  end

val getlk : file_desc * FLock.flock -> FLock.flock
val setlk : file_desc * FLock.flock -> FLock.flock
val setlkw : file_desc * FLock.flock -> FLock.flock

val mkBinReader  : {
                       fd : file_desc,
                       name : string,
                       initBlkMode : bool
                     } -> BinPrimIO.reader
val mkTextReader : {
                       fd : file_desc,
                       name : string,
                       initBlkMode : bool
                     } -> TextPrimIO.reader
val mkBinWriter  : {
                       fd : file_desc,
                       name : string,
                       appendMode : bool,
                       initBlkMode : bool,
                       chunkSize : int
                     } -> BinPrimIO.writer
val mkTextWriter : {
                       fd : file_desc,
                       name : string,
                       appendMode : bool,
                       initBlkMode : bool,
                       chunkSize : int
                     } -> TextPrimIO.writer
end

signature POSIX_SYS_DB = sig
eqtype uid
eqtype gid

structure Passwd : sig
    type passwd
    val name  : passwd -> string
    val uid   : passwd -> uid
    val gid   : passwd -> gid
    val home  : passwd -> string
    val shell : passwd -> string
  end

structure Group : sig
    type group
    val name    : group -> string
    val gid     : group -> gid
    val members : group -> string list
  end

val getgrgid : gid -> Group.group
val getgrnam : string -> Group.group
val getpwuid : uid -> Passwd.passwd
val getpwnam : string -> Passwd.passwd
end

signature POSIX_TTY = sig
eqtype pid
eqtype file_desc

structure V : sig
    val eof   : int
    val eol   : int
    val erase : int
    val intr  : int
    val kill  : int
    val min   : int
    val quit  : int
    val susp  : int
    val time  : int
    val start : int
    val stop  : int
    val nccs : int
    type cc
    val cc : (int * char) list -> cc
    val update : cc * (int * char) list -> cc
    val sub : cc * int -> char
  end

structure I : sig
    include BIT_FLAGS
    val brkint : flags
    val icrnl  : flags
    val ignbrk : flags
    val igncr  : flags
    val ignpar : flags
    val inlcr  : flags
    val inpck  : flags
    val istrip : flags
    val ixoff  : flags
    val ixon   : flags
    val parmrk : flags
  end

structure O : sig
    include BIT_FLAGS
    val opost : flags
  end

structure C : sig
    include BIT_FLAGS
    val clocal : flags
    val cread  : flags
    val cs5    : flags
    val cs6    : flags
    val cs7    : flags
    val cs8    : flags
    val csize  : flags
    val cstopb : flags
    val hupcl  : flags
    val parenb : flags
    val parodd : flags
  end

structure L : sig
    include BIT_FLAGS
    val echo   : flags
    val echoe  : flags
    val echok  : flags
    val echonl : flags
    val icanon : flags
    val iexten : flags
    val isig   : flags
    val noflsh : flags
    val tostop : flags
  end

eqtype speed

val compareSpeed : speed * speed -> order
val speedToWord : speed -> SysWord.word
val wordToSpeed : SysWord.word -> speed

val b0   : speed
val b50  : speed
val b75  : speed
val b110 : speed
val b134 : speed
val b150 : speed
val b200 : speed
val b300 : speed
val b600 : speed
val b1200 : speed
val b1800 : speed
val b2400 : speed
val b4800 : speed
val b9600 : speed
val b19200 : speed
val b38400 : speed

type termios

val termios : {
                  iflag : I.flags,
                  oflag : O.flags,
                  cflag : C.flags,
                  lflag : L.flags,
                  cc : V.cc,
                  ispeed : speed,
                  ospeed : speed
                } -> termios
val fieldsOf : termios
                 -> {
                   iflag : I.flags,
                   oflag : O.flags,
                   cflag : C.flags,
                   lflag : L.flags,
                   cc : V.cc,
                   ispeed : speed,
                   ospeed : speed
                 }
val getiflag : termios -> I.flags
val getoflag : termios -> O.flags
val getcflag : termios -> C.flags
val getlflag : termios -> L.flags
val getcc    : termios -> V.cc

structure CF : sig
    val getospeed : termios -> speed
    val getispeed : termios -> speed
    val setospeed : termios * speed -> termios
    val setispeed : termios * speed -> termios
  end

structure TC : sig
    eqtype set_action
    
    val sanow   : set_action
    val sadrain : set_action
    val saflush : set_action
    
    eqtype flow_action
    
    val ooff : flow_action
    val oon  : flow_action
    val ioff : flow_action
    val ion  : flow_action
    
    eqtype queue_sel
    
    val iflush  : queue_sel
    val oflush  : queue_sel
    val ioflush : queue_sel
    
    val getattr : file_desc -> termios
    val setattr : file_desc * set_action * termios -> unit
    val sendbreak : file_desc * int -> unit
    val drain : file_desc -> unit
    val flush : file_desc * queue_sel -> unit
    val flow : file_desc * flow_action -> unit
    val getpgrp : file_desc -> pid
    val setpgrp : file_desc * pid -> unit
  end
end

signature POSIX = sig
structure Error : POSIX_ERROR
structure Signal : POSIX_SIGNAL
structure Process : POSIX_PROCESS
  where type signal = Signal.signal
structure ProcEnv : POSIX_PROC_ENV
  where type pid = Process.pid
structure FileSys : POSIX_FILE_SYS
  where type file_desc = ProcEnv.file_desc
  where type uid = ProcEnv.uid
  where type gid = ProcEnv.gid
structure IO : POSIX_IO
  where type pid = Process.pid
  where type file_desc = ProcEnv.file_desc
  where type open_mode = FileSys.open_mode
structure SysDB : POSIX_SYS_DB
  where type uid = ProcEnv.uid
  where type gid = ProcEnv.gid
structure TTY : POSIX_TTY
  where type pid = Process.pid
  where type file_desc = ProcEnv.file_desc
end


structure Posix = struct (*[ assumesig POSIX ]*) end
