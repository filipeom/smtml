open Format

type t =
  | Assert of term
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model

and term =
  | E of Expr.t
  | Let of binding list * term

and binding = string * term

let rec pp_term fmt = function
  | E e -> Expr.pp fmt e
  | Let (binds, t) ->
    fprintf fmt "@[<v>(let@[<hov>@ (%a)@]@ %a)@]"
      (pp_print_list ~pp_sep:pp_print_space pp_binding)
      binds pp_term t

and pp_binding fmt (x, t) = fprintf fmt "(%s %a)" x pp_term t

let pp fmt (instr : t) =
  match instr with
  | Assert t -> fprintf fmt "@[<hov 2>(assert@ %a)@]" pp_term t
  | Check_sat -> pp_print_string fmt "(check-sat)"
  | Push -> pp_print_string fmt "(push)"
  | Pop n -> fprintf fmt "(pop %d)" n
  | Let_const s ->
    fprintf fmt "(let-const %a %a)" Symbol.pp s Ty.pp (Symbol.ty s)
  | Get_model -> pp_print_string fmt "(get-model)"

let to_string (instr : t) : string = asprintf "%a" pp instr
