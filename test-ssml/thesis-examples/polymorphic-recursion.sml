
  datatype 'a hyperList = Nil
                        | Cons of 'a * ('a hyperList) hyperList

  (*[ datasort 'a even = Nil | Cons of 'a * ('a hyperList) odd
           and 'a odd = Cons of 'a * ('a hyperList) even
  ]*)


  datatype 'a hyperTree = Leaf of 'a
                        | Node of  ('a hyperTree) hyperTree 
                                 * ('a hyperTree) hyperTree

  (*[ datasort 'a even = Leaf of 'a 
                       | Node of  ('a odd) odd
                                * ('a even) even
           and 'a odd = Leaf of 'a 
                       | Node of  ('a odd) even
                                * ('a even) odd
  ]*)
