(* TODO: put this in some other more appropriate module? *)
type solver_type =
  | Z3_solver
  | Bitwuzla_solver
  | Colibri2_solver
  | Cvc5_solver

let mappings_of_solver : solver_type -> (module Mappings_intf.S_with_make) =
  function
  | Z3_solver -> (module Z3_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Cvc5_solver -> (module Cvc5_mappings)

let solver_type_of_string (s : string) :
  (solver_type, [> `Msg of string ]) result =
  match String.map Char.lowercase_ascii s with
  | "z3" -> Ok Z3_solver
  | "bitwuzla" -> Ok Bitwuzla_solver
  | "colibri2" -> Ok Colibri2_solver
  | "cvc5" -> Ok Cvc5_solver
  | s -> Error (`Msg (Fmt.str "unknown solver %s" s))

let is_available : solver_type -> bool = function
  | Z3_solver -> Z3_mappings.is_available
  | Bitwuzla_solver -> Bitwuzla_mappings.is_available
  | Colibri2_solver -> Colibri2_mappings.is_available
  | Cvc5_solver -> Cvc5_mappings.is_available

(** List of all available solvers *)
let available_solvers : solver_type list =
  List.filter is_available
    [ Z3_solver; Bitwuzla_solver; Colibri2_solver; Cvc5_solver ]

(** Returns first available solver or errors when none exist *)
let solver : ((module Mappings_intf.S_with_make), [> `Msg of string ]) result =
  match available_solvers with
  | [] -> Error (`Msg "no available solver")
  | solver :: _ -> Ok (mappings_of_solver solver)

let pp_solver_type fmt = function
  | Z3_solver -> Fmt.pf fmt "Z3"
  | Bitwuzla_solver -> Fmt.pf fmt "Bitwuzla"
  | Colibri2_solver -> Fmt.pf fmt "Colibri2"
  | Cvc5_solver -> Fmt.pf fmt "cvc5"
