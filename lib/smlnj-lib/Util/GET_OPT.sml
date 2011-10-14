signature GET_OPT =
  sig
    datatype 'a arg_order
      = Permute | RequireOrder | ReturnInOrder of string -> 'a
    datatype 'a arg_descr
      = NoArg of unit -> 'a
      | OptArg of (string option -> 'a) * string
      | ReqArg of (string -> 'a) * string
    type 'a opt_descr =
      {desc:'a arg_descr, help:string, long:string list, short:string}
    val usageInfo : {header:string, options:'a opt_descr list} -> string
    val getOpt : {argOrder:'a arg_order, errFn:string -> unit,
                  options:'a opt_descr list}
                 -> string list -> 'a list * string list
  end

structure GetOpt = struct (*[ assumesig GET_OPT ]*) end
