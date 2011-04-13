(* Initial allows for other modules to be discharged at link time *)
local Initial.sml
in

(* General: *)
GENERAL.sml General.sml OPTION.sml Option.sml

(* Lists: *)
LIST.sml List.sml LIST_PAIR.sml ListPair.sml LIST_SORT.sml
ListSort.sml

(* Arrays and Vectors: *)
local wordtables.sml 
in VECTOR.sml Vector.sml ARRAY.sml Array.sml
end


MONO_VECTOR.sml MONO_ARRAY.sml ByteVector.sml ByteArray.sml

(* Text: *)
STRING_CVT.sml StringCvt.sml 

local STR_BASE.sml StrBase.sml 
in Char.sml String.sml CHAR.sml STRING.sml SUBSTRING.sml Substring.sml 
end

BOOL.sml
Bool.sml

(* Integers: *)
Word.sml Word8.sml WORD.sml BYTE.sml Byte.sml Int.sml INTEGER.sml

(* Reals: *)
MATH.sml Math.sml REAL.sml Real.sml

(* IO: *)
IO.sml TEXT_IO.sml TextIO.sml BIN_IO.sml BinIO.sml

(* System: *)
TIME.sml Time.sml OS_PATH.sml Path.sml OS_FILE_SYS.sml FileSys.sml
OS_PROCESS.sml Process.sml OS.sml COMMAND_LINE.sml CommandLine.sml
DATE.sml Date.sml TIMER.sml Timer.sml

(* Misc: *)
RANDOM.sml Random.sml SML90.sml

end (*Initial*)




