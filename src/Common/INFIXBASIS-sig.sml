(*$INFIX_BASIS*)
signature INFIX_BASIS =
  sig
    eqtype id
    type Basis

    val emptyB: Basis

    datatype InfixEntry = NONFIX | INFIX of int | INFIXR of int

    val new: id list * InfixEntry -> Basis
    val lookup: Basis -> id -> InfixEntry

    val compose: Basis * Basis -> Basis
    val eq: Basis * Basis -> bool

    type Report
    val reportBasis: Basis -> Report

    (* type StringTree *)
    val layoutBasis: Basis -> StringTree.t
  end;
