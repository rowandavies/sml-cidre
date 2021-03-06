(*WORD.sml*)

signature WORD = sig  (*requires StringCvt*)
  eqtype word

  val wordSize : int

  val toLarge      : word -> CidreExtras.LargeWord.word
  val toLargeX     : word -> CidreExtras.LargeWord.word
  val toLargeWord  : word -> CidreExtras.LargeWord.word
  val toLargeWordX : word -> CidreExtras.LargeWord.word
  val fromLarge     : CidreExtras.LargeWord.word -> word
  val fromLargeWord : CidreExtras.LargeWord.word -> word
  val toLargeInt  : word -> LargeInt.int
  val toLargeIntX : word -> LargeInt.int
  val fromLargeInt : LargeInt.int -> word
  val toInt  : word -> int
  val toIntX : word -> int
  val fromInt : int -> word

  val andb : word * word -> word
  val orb  : word * word -> word
  val xorb : word * word -> word
  val notb : word -> word
  val << : word * CidreExtras.Word.word -> word
  val >> : word * CidreExtras.Word.word -> word
  val ~>> : word * CidreExtras.Word.word -> word

  val + : word * word -> word
  val - : word * word -> word
  val * : word * word -> word
  val div : word * word -> word
  val mod : word * word -> word

  val compare : word * word -> order
  val <  : word * word -> bool
  val <= : word * word -> bool
  val >  : word * word -> bool
  val >= : word * word -> bool

  val ~ : word -> word
  val min : word * word -> word
  val max : word * word -> word

  val fmt      : StringCvt.radix -> word -> string
  val toString : word -> string
  val scan       : StringCvt.radix
                     -> (char, 'a) StringCvt.reader
                       -> (word, 'a) StringCvt.reader
  val fromString : string -> word option

end; (*signature WORD*)

structure Word = struct (*[ assumesig  WORD where type word = word ]*) end
structure Word8 = struct (*[ assumesig  WORD ]*) end
structure LargeWord = struct (*[ assumesig  WORD where type word = CidreExtras.LargeWord.word ]*) end
structure Word16 = struct (*[ assumesig  WORD ]*) end
structure Word32 = struct (*[ assumesig  WORD ]*) end
structure Word31 = struct (*[ assumesig  WORD ]*) end
structure Word64 = struct (*[ assumesig  WORD ]*) end
structure Word63 = struct (*[ assumesig  WORD ]*) end
structure SysWord = struct (*[ assumesig  WORD where type word = word ]*) end  
    (* SysWord.word = word isn't in the spec, but some code assumes this, and it seems harmless.  *)


(* [word] is the type of n-bit words, or n-bit unsigned integers.

   [wordSize] is the value of n above.  In Moscow ML, n=31 on 32-bit
   machines and n=63 on 64-bit machines.

   [orb(w1, w2)] returns the bitwise `or' of w1 and w2.

   [andb(w1, w2)] returns the bitwise `and' of w1 and w2.

   [xorb(w1, w2)] returns the bitwise `exclusive or' or w1 and w2.

   [notb w] returns the bitwise negation of w.

   [<<(w, k)] returns the word resulting from shifting w left by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [~>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are replications of the left-most bit:
   the `sign bit', so this is an arithmetical shift.  Consequently,
   for k >= wordSize and wordToInt w >= 0 the result is all 0-bits, and 
   for k >= wordSize and wordToInt w <  0 the result is all 1-bits.

   To make <<, >>, and ~>> infix, use the declaration 
                          infix 5 << >> ~>>

   [+, -, *, div, mod] represent unsigned integer addition,
   subtraction, multiplication, division, and remainder, modulus two
   to wordSize.  The operations (i div j) and (i mod j) raise Div when
   j=0.  Otherwise no exceptions are raised.

   [w1 > w2] returns true if the unsigned integer represented by w1
   is larger than that of w2, and similarly for <, >=, <=.  

   [compare(w1, w2)] returns LESS, EQUAL, or GREATER, according 
   as w1 is less than, equal to, or greater than w2 (as unsigned integers).

   [min(w1, w2)] returns the smaller of w1 and w2 (as unsigned integers).

   [max(w1, w2)] returns the larger of w1 and w2 (as unsigned integers).

   [fmt radix w] returns a string representing w, in the radix (base)
   specified by radix.

     radix    description                     output format  
     ------------------------------------------------------  
      BIN     unsigned binary      (base  2)  [01]+         
      OCT     unsigned octal       (base  8)  [0-7]+          
      DEC     unsigned decimal     (base 10)  [0-9]+          
      HEX     unsigned hexadecimal (base 16)  [0-9A-F]+       

   [toString w] returns a string representing w in unsigned
   hexadecimal format.  Equivalent to (fmt HEX w).
   
   [fromString s] returns SOME(w) if a hexadecimal unsigned numeral
   can be scanned from a prefix of string s, ignoring any initial
   whitespace; returns NONE otherwise.  Raises Overflow if the scanned 
   number cannot be represented as a word.  An unsigned hexadecimal
   numeral must have form, after possible initial whitespace:
        [0-9a-fA-F]+

   [scan radix getc charsrc] attempts to scan an unsigned numeral from
   the character source charsrc, using the accessor getc, and ignoring
   any initial whitespace.  The radix argument specifies the base of
   the numeral (BIN, OCT, DEC, HEX).  If successful, it returns
   SOME(w, rest) where w is the value of the numeral scanned, and rest
   is the unused part of the character source.  Raises Overflow if the
   scanned number cannot be represented as a word.  A numeral must
   have form, after possible initial whitespace:

     radix    input format 
     -------------------------------------
      BIN     (0w)?[0-1]+
      OCT     (0w)?[0-7]+
      DEC     (0w)?[0-9]+
      HEX     (0wx|0wX|0x|0X)?[0-9a-fA-F]+

   [toInt w] returns the (signed) integer represented by bit-pattern w.
   [toIntX w] returns the (signed) integer represented by bit-pattern w.
   [fromInt i] returns the word representing integer i.

   [toLargeInt w] returns the (signed) integer represented by bit-pattern w.
   [toLargeIntX w] returns the (signed) integer represented by bit-pattern w.
   [fromLargeInt i] returns the word representing integer i.

   [toLargeWord w] returns w.
   [toLargeWordX w] returns w.
   [fromLargeWord w] returns w.
*)
