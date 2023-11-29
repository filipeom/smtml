open Encoding
open Cmdliner
module Z3_batch = Batch.Make (Z3_mappings)
module Z3_incremental = Incremental.Make (Z3_mappings)
module Interpret = Interpret.Make (Z3_batch)

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename ->
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> In_channel.input_all chan)

let parse_file file = get_contents file |> Run.parse_string

let main files =
  match files with
  | [] ->
    let ast = parse_file "-" in
    ignore @@ Interpret.start ast
  | _ ->
    ignore
    @@ List.fold_left
         (fun state file ->
           let ast = Run.parse_file file in
           Some (Interpret.start ?state ast) )
         None files

let files =
  let doc = "source files" in
  Arg.(value & pos_all non_dir_file [] & info [] ~doc)

let cli =
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" in
  Cmd.v info Term.(const main $ files)

let () = exit @@ Cmd.eval cli
