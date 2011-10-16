functor DictTable (structure Dict : DICT
                  type value)
  :> TABLE where type key = Dict.key and type value = value (*[ where sort key = Nat.even ]*) 
       
  =
  struct

    (*[ table <: Dict.t ref ]*)
    val table = ref Dict.empty
  end
