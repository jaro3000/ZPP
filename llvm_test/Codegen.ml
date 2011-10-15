open Llvm;;
open Llvm_bitwriter;;

exception CompError of string;;
let context = global_context();;
let tmodule = create_module context "MyApp";;

let cint v = const_int (i32_type context) v;;

let gen_code expr builder (ctx : string -> llvalue) =

  let rec gencode expr = 
    match expr with
      | Parse.Value t -> cint t
      | Parse.Id t -> ctx t
      | Parse.Call ("if", [condition; expr1; expr2]) ->
          let cond = gencode condition in
          let fct = block_parent (insertion_block builder) in
          let true_block = append_block context "True" fct in
          let false_block = append_block context "False" fct in
          let fin_block = append_block context "Fin" fct in
          let true_value = ref cond and false_value = ref cond in
            begin
              ignore (build_cond_br cond true_block false_block builder);
              
              position_at_end true_block builder;
              true_value := gencode expr1;
              ignore (build_br fin_block builder);

              position_at_end false_block builder;
              false_value := gencode expr2;
              ignore (build_br fin_block builder);

              position_at_end fin_block builder;
              let tvb = if is_constant !true_value then true_block else instr_parent !true_value
              and fvb = if is_constant !false_value then false_block else instr_parent !false_value
              in
                build_phi [(!true_value, tvb); (!false_value, fvb)] "ifexprres" builder;
            end
      | Parse.Call ("=", [arg1; arg2;]) ->
          let a1 = gencode arg1
          and a2 = gencode arg2 in
            build_icmp Icmp.Eq a1 a2 "cmpres" builder
      | Parse.Call ("+", [arg1; arg2;]) ->
          let a1 = gencode arg1
          and a2 = gencode arg2 in
            build_add a1 a2 "" builder
      | Parse.Call ("*", [arg1; arg2]) ->
          let a1 = gencode arg1
          and a2 = gencode arg2 in
            build_mul a1 a2 "" builder
      | Parse.Call ("-", [arg1; arg2;]) ->
          let a1 = gencode arg1
          and a2 = gencode arg2 in
            build_sub a1 a2 "" builder
      | Parse.Call (fname, arglist) ->
          let args = Array.of_list (List.map gencode arglist) in
          let Some s = (lookup_function fname tmodule) in
            build_call s args (fname^"res") builder
  in
    gencode expr;;

let gen_fun name pc expr =
  let itype = i32_type context in
  let params_type = Array.make pc itype in
  let func = define_function name (function_type itype params_type) tmodule in
  let builder = builder_at_end context (entry_block func) in
  let ctx = fun s -> param func (int_of_string (String.sub s 1 (String.length s - 1))) 
  in
    begin
      ignore (build_ret (gen_code expr builder ctx) builder);
      func;
    end;;

let dump () = dump_module tmodule;;

let write_to_file filename = write_bitcode_file tmodule filename;;
