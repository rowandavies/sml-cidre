functor DictTable (structure Dict : DICT
                  type value)
  :> TABLE (*[ sharing sort key = Nat.even ]*) where type key = Dict.key and type value = value
       
  =
  struct

    (*[ table :> Dict ref ]*)
    val table = ref Dict.empty
  end
