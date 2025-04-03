open Interpreter
open Ast
open Typechecker

let () =
  let filename = "tc.txt" in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let prog = Parser.program Lexer.token lexbuf in
    close_in in_channel;
    try
      typecheck_program prog;
      let env = Hashtbl.create 100 in
      let final_env = List.fold_left (fun env stmt -> eval_statement stmt env) env prog in
      print_endline "Program executed successfully.";
      print_endline "Final Environment:";
      Hashtbl.iter (fun var value ->
        Printf.printf "%s = %s\n" var (string_of_value value)
      ) final_env;
    with
    | TypeMismatch msg ->
        prerr_endline ("Type mismatch: " ^ msg);
        exit 1
    | UndefinedVariable var ->
        prerr_endline ("Undefined variable: " ^ var);
        exit 1
    | DimensionError msg ->
        prerr_endline ("Dimension error: " ^ msg);
        exit 1
  with e ->
    close_in_noerr in_channel;
    prerr_endline ("Error during parsing or evaluation: " ^ Printexc.to_string e);
    exit 1