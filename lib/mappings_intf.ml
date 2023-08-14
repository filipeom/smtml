type satisfiability = Satisfiable | Unsatisfiable | Unknown

module type S = sig
  type expr
  type model
  type solver
  type status
  type optimize
  type handle

  exception Error of string

  val encode_expr : _ Expr.t -> expr
  val expr_to_smtstring : _ Expr.t list -> Bool.t -> string
  val mk_solver : unit -> solver
  val interrupt : unit -> unit
  val translate : solver -> solver
  val push : solver -> unit
  val pop : solver -> int -> unit
  val reset : solver -> unit
  val add_solver : solver -> bool Expr.t List.t -> unit
  val check : solver -> bool Expr.t List.t -> status
  val get_model : solver -> model Option.t
  val mk_opt : unit -> optimize
  val add_opt : optimize -> _ Expr.t List.t -> unit
  val maximize : optimize -> _ Expr.t -> handle
  val minimize : optimize -> _ Expr.t -> handle
  val get_opt_model : optimize -> model Option.t
  val value_of_const : model -> _ Expr.t -> _ Value.t option
(*  val value_binds : ?symbols:Symbol.t list -> model -> Model.t*)
  val satisfiability : status -> satisfiability
end
