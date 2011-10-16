

functor F (type 'a t) =
struct
  datatype d = D1 of d t | D2
end

structure S = F (type 'a t = 'a)
