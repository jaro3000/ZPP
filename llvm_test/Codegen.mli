val gen_code : Parse.expr -> Llvm.llbuilder -> (string -> Llvm.llvalue) -> Llvm.llvalue
val gen_fun : string -> int -> Parse.expr -> Llvm.llvalue
val dump : unit -> unit                                               
val write_to_file : string -> bool
