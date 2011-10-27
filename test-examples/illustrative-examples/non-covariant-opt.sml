structure opt :> 
    sig type 'a t (*[ sortdef 'a nn < t ]*)  (*[ sortdef 'a ss |: t ]*)  
        val none : 'a t and some : 'a -> 'a t
        (*[ none <: 'a nn and some <: 'a -> 'a ss ]*) 
end = 
struct type 'a t = 'a option (*[ sortdef 'a nn = 'a none and 'a ss = 'a some ]*)   
       val none = NONE and some = SOME 
end

(*[ whatSort <: unit opt.ss opt.t ]*)
val whatSort = opt.some (opt.some ()) 

val newSort = (whatSort (*[ <: unit opt.t ]*) )
