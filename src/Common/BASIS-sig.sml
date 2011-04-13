
signature BASIS =
  sig

    structure TyName : TYNAME

    type ElabEnv

    type longstrid

    type Basis and InfixBasis and ElabBasis
    structure Basis :
      sig
	val empty : Basis
	val mk : InfixBasis * ElabBasis -> Basis
	val un : Basis -> InfixBasis * ElabBasis
	val plus : Basis * Basis -> Basis
	val layout : Basis -> StringTree

	val agree : longstrid list * Basis * (Basis * TyName.Set.Set) -> bool
	val enrich : Basis * (Basis * TyName.Set.Set) -> bool

	val initial : Basis
      end

    type name

  end