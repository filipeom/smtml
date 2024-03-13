(* open Lexer *)
open Lexing
open Format

let _pp_pos fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

(* let parse_with_error lexbuf = *)
(*   try Parser.script Lexer.token lexbuf with *)
(*   | SyntaxError msg -> *)
(*     fprintf err_formatter "%a: %s\n" pp_pos lexbuf msg; *)
(*     [] *)
(*   | Parser.Error -> *)
(*     fprintf err_formatter "%a: syntax error\n" pp_pos lexbuf; *)
(*     exit 1 *)

let from_file ~filename =
  let chan = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let lexbuf = Lexing.from_channel chan in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      assert false )
(* parse_with_error lexbuf ) *)

let from_string _contents =
  (* parse_with_error (Lexing.from_string contents) *)
  assert false
