(*$REFTOPDEC*)

signature REFTOPDEC =
sig
    structure Comp : COMP

    type Basis
    type PostElabTopdec

    val ref_topdec: Basis * PostElabTopdec -> Basis Comp.Comp

end;
