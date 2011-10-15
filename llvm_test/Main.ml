open Genlex;;
open Parse;;
open Codegen;;

let mySS = Parse.symbolStream stdin;;
type t = int;;
let expr_list = Parse.parse_expr_list mySS;;

let lookatthis expr =
  match expr with
      Call("defun", [Id(name); Value(argcount); body]) ->
        ignore(Codegen.gen_fun name argcount body)
    | _ -> raise Parse.ParseError;;

List.iter lookatthis expr_list ;;

Codegen.dump();
Codegen.write_to_file "out.bc";

