type expr = Value of int | Id of string | Call of (string*(expr list));;
exception ParseError
val parse_expr : Genlex.token Stream.t -> expr
val parse_expr_list : Genlex.token Stream.t -> expr list
val symbolStream : in_channel -> Genlex.token Stream.t
val string_of_expr : expr -> string
