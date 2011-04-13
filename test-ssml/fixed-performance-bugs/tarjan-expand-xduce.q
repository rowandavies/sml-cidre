
(*** Implementations of Mihaescu/Tarjan real-time purely functional
     lists.

     As far as possible the implementations are checked by datasort
     refinements as implemented by Davies.


     Kevin Watkins
     10 May 2004
 ***)
type bal = leaf[String] | pair [(bal, bal)]
type balList = nil [] | cons [(bal, balList)]
type word = wleaf [] | wpair [(balList, word)]
type sentence = sleaf[] | spair [((word, word), sentence)]
type paragraph = pleaf[] | ppair [(sentence, paragraph)]


  type  zero = nil[]
  type  one = cons [(bal, zero)]
  type  two = cons [(bal,  one)]

  type  ones_w = wleaf[] | wpair [(  one,  ones_w)]
  type  zero_ones_w = wpair [(  zero,  ones_w)]
  type  two_ones_w = wpair [(  two,  ones_w)]

  type switch_w = wpair [(  zero,  ones_w)]
                       | wpair [(  two,  ones_w)]

  type empty_s = sleaf[]
  type short_s = spair [( ( word,  word),  empty_s)]
  type left_s  = spair [( ( switch_w,  word),  sentence)]
  type right_s = spair [( ( word,  switch_w),  sentence)]

  type lzero_end_s = spair [( ( zero_ones_w,  word)       ,  empty_s)]
                          | spair [( ( word       ,  word)       ,  lzero_end_s)]
  type ltwo_end_s  = spair [( ( two_ones_w ,  word)       ,  empty_s)]
                          | spair [( ( word       ,  word)       ,  ltwo_end_s)]
  type rzero_end_s = spair [( ( word       ,  zero_ones_w),  empty_s)]
                          | spair [( ( word       ,  word)       ,  rzero_end_s)]
  type rtwo_end_s  = spair [( ( word       ,  zero_ones_w),  empty_s)]
                          | spair [( ( word       ,  word)       ,  rtwo_end_s)]

  type lzero_s = sleaf[] | spair [( ( zero_ones_w,  word)       ,  ltwo_s)]
  type       ltwo_s  = sleaf[] | spair [( ( two_ones_w ,  word)       ,  lzero_s)]
  type Lone_s  = sleaf[] | spair [( ( ones_w     ,  word)       ,  Lone_s)]
  type rzero_s = sleaf[] | spair [( ( word       ,  zero_ones_w),  rtwo_s)]
  type       rtwo_s  = sleaf[] | spair [( ( word       ,  two_ones_w) ,  rzero_s)]
  type Rone_s  = sleaf[] | spair [( ( word       ,  ones_w)     ,  Rone_s)]

  type switch_p = pleaf[]
                       | ppair [(  short_s,  switch_p)]
                       | ppair [(  left_s ,  switch_p)]
                       | ppair [(  right_s,  switch_p)]

  type  left_s_Lzero_end_s_Ltwo_s = 
            spair [ ( two_ones_w ,  word) ,  left_s_Lzero_end_s_Lzero_s]

  type  left_s_Lzero_end_s_Lzero_s = 
            spair [ ( zero_ones_w ,  word) ,  empty_s ] | 
            spair [ ( zero_ones_w ,  word) ,  left_s_Lzero_end_s_Ltwo_s]

  type  Ltwo_s_Lzero_end_s =  left_s_Lzero_end_s_Ltwo_s

  type  Ltwo_end_s_Ltwo_s = 
            spair [ ( two_ones_w ,  word) ,  empty_s ] | 
            spair [ ( two_ones_w ,  word) ,  Ltwo_end_s_Lzero_s]

  type  Ltwo_end_s_Lzero_s = 
            spair [ ( zero_ones_w ,  word) ,  Ltwo_end_s_Ltwo_s]

  type  Ltwo_s_Ltwo_end_s =  Ltwo_end_s_Ltwo_s 

  type  left_s_Lzero_end_s_Lzero_s = 
            spair [ ( zero_ones_w ,  word) ,  empty_s ] | 
            spair [ ( zero_ones_w ,  word) ,  left_s_Lzero_end_s_Ltwo_s]

  type  left_s_Lzero_end_s_Ltwo_s = 
        spair [ ( two_ones_w ,  word) ,  left_s_Lzero_end_s_Lzero_s]

  type  Lzero_s_Lzero_end_s =   left_s_Lzero_end_s_Lzero_s

  type  Ltwo_end_s_Lzero_s = 
           spair [ ( zero_ones_w ,  word) ,  Ltwo_end_s_Ltwo_s]

  type  Ltwo_end_s_Ltwo_s = 
            spair [ ( two_ones_w ,  word) ,  empty_s ]| 
            spair [ ( two_ones_w ,  word) ,  Ltwo_end_s_Lzero_s]

  type  Lzero_s_Ltwo_end_s =  Ltwo_end_s_Lzero_s

        type  right_s___Rzero_end_s___Rzero_s = 
            spair [ ( word ,  zero_ones_w) ,  empty_s ]| 
            spair [ ( word ,  zero_ones_w) ,  right_s___Rzero_end_s___Rtwo_s]

        type  right_s___Rzero_end_s___Rtwo_s = 
        spair [ ( word ,  two_ones_w) ,  right_s___Rzero_end_s___Rzero_s]

        type  short_s___Rone_s = spair [ ( word ,  ones_w) ,  empty_s]
        type  left_s___Rone_s = spair [ ( switch_w ,  ones_w) ,  Rone_s]


        type  switch_p___Rzero_p = 
            ppair [  right_s___Rzero_end_s___Rzero_s ,  switch_p___Rtwo_p ]| 
            ppair [  right_s___Rzero_end_s___Rzero_s ,  switch_p___Rzero_p ]| 
            ppair [  short_s___Rone_s ,  switch_p___Rzero_p ]| 
            ppair [  left_s___Rone_s ,  switch_p___Rzero_p]

        type  switch_p___Rtwo_p = 
            pleaf[]
          | ppair [  right_s___Rzero_end_s___Rtwo_s ,  switch_p___Rtwo_p ] | 
            ppair [  right_s___Rzero_end_s___Rtwo_s ,  switch_p___Rzero_p ]| 
            ppair [  short_s___Rone_s ,  switch_p___Rtwo_p ]| 
            ppair [  left_s___Rone_s ,  switch_p___Rtwo_p]




  (* Swapped the order of the following. *)
    type       Ltwo_p  = pleaf[]
                      | ppair [ ( Ltwo_s_Lzero_end_s) ,  Ltwo_p ]
                      | ppair [ ( Ltwo_s_Ltwo_end_s)  ,  Lzero_p ]
                      | ppair [   Lone_s                    ,  Ltwo_p ]
       type  Lzero_p = ppair [ ( Lzero_s_Lzero_end_s) ,  Ltwo_p ]
                      | ppair [ ( Lzero_s_Ltwo_end_s)  ,  Lzero_p ]
                      | ppair [   Lone_s                    ,  Lzero_p ]

(*
  (* Swapped the order of the following. *)
  type       Rtwo_p  = pleaf
                      | ppair [ ( Rtwo_s  &  Rzero_end_s) ,  Rtwo_p ]
                      | ppair [ ( Rtwo_s  &  Rtwo_end_s)  ,  Rzero_p ]
                      | ppair [   Rone_s                    ,  Rtwo_p ]
       type  Rzero_p = ppair [ ( Rzero_s &  Rzero_end_s) ,  Rtwo_p ]
                      | ppair [ ( Rzero_s &  Rtwo_end_s)  ,  Rzero_p ]
                      | ppair [   Rone_s                    ,  Rzero_p ]
*)

