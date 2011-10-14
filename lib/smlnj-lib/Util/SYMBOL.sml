signature SYMBOL = 
  sig
    val varInt : word
    val sigInt : word
    val strInt : word
    val fsigInt : word
    val fctInt : word
    val tycInt : word
    val labInt : word
    val tyvInt : word
    val fixInt : word
    datatype symbol = SYMBOL of word * string
    datatype namespace
      = FCTspace
      | FIXspace
      | FSIGspace
      | LABspace
      | SIGspace
      | STRspace
      | TYCspace
      | TYVspace
      | VALspace
    val eq : symbol * symbol -> bool
    val symbolGt : symbol * symbol -> bool
    val symbolCMLt : symbol * symbol -> bool
    val compare : symbol * symbol -> order
    val varSymbol : string -> symbol
    val tycSymbol : string -> symbol
    val fixSymbol : string -> symbol
    val labSymbol : string -> symbol
    val tyvSymbol : string -> symbol
    val sigSymbol : string -> symbol
    val strSymbol : string -> symbol
    val fctSymbol : string -> symbol
    val fsigSymbol : string -> symbol
    val var'n'fix : string -> symbol * symbol
    val name : symbol -> string
    val number : symbol -> word
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val describe : symbol -> string
    val symbolToString : symbol -> string
  end

structure Symbol = struct (*[ assumesig SYMBOL ]*) end
