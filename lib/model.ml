type t = (Symbol.symbol, Value.value) Hashtbl.t

let create () = Hashtbl.create 0

let add (model : t) (k : Symbol.symbol) (v : Value.value) : unit =
  Hashtbl.add model k v

let get_symbols (model : t) : Symbol.symbol list =
  Hashtbl.to_seq_keys model |> List.of_seq

let get_bindings (model : t) : (Symbol.symbol * Value.value) list =
  Hashtbl.to_seq model |> List.of_seq

let evaluate (model : t) (symb : Symbol.symbol) : Value.value option =
  Hashtbl.find_opt model symb

let to_string (model : t) : string =
  let open Symbol in
  let bindings =
    Hashtbl.fold
      (fun (Sym key) data accum ->
        let x = Symbol.Pp.pp key
        and t = Type.Pp.pp_ty (Symbol.type_of key)
        and v = Value.Pp.pp_value data in
        Format.sprintf "%s  (%s %s %s)\n" accum x t v )
      model ""
  in
  Format.sprintf "(model\n%s)" bindings
