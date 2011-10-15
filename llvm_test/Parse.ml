open Genlex;;

let keywords = ["("; ")"];;
let symbolStream channel = make_lexer keywords (Stream.of_channel channel);;

type expr = Value of int | Id of string | Call of (string*(expr list));;
exception ParseError;;

let rec string_of_expr = 
  function Value t -> string_of_int t
    | Id s -> s
    | Call (s, elist) -> 
        (List.fold_left (fun a b -> a^(string_of_expr b)^" ") 
           ("Call "^s^"(") 
           elist)  ^  ")";;

let rec parse_expr_list ss =
  match Stream.peek ss with
    | Some Kwd ")" -> (ignore (Stream.next ss); [])
    | Some _ -> 
        let head = parse_expr ss in
        let tail = parse_expr_list ss in
          head::tail
    | None -> [];
and parse_expr ss =
  match Stream.next ss with
    | Kwd "(" ->
        (match Stream.next ss with
          | Ident t -> Call (t, parse_expr_list ss)
          | _ -> raise ParseError)
    | Int t -> Value t
    | Ident t -> Id t
    | _ -> raise ParseError;;
