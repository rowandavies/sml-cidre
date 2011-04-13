
  local
    datatype myBool = myTrue | myFalse
    (*[ datasort myTT = myTrue
             and myFF = myFalse ]*)
  in
    (*[ val myNot :>  myTT -> myFF 
                    & myFF -> myTT ]*)
    fun myNot myTrue = myFalse
      | myNot myFalse = myTrue
    val myBoolVal = myTrue
  end
  val myResult = myNot myBoolVal
