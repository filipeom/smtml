module type S = sig
  type t
  type solver

  val solver_time : float ref
  val solver_count : int ref
  val create : unit -> t
  val interrupt : unit -> unit
  val clone : t -> t
  val push : t -> unit
  val pop : t -> int -> unit
  val reset : t -> unit
  val add : t -> bool Expr.t list -> unit
  val get_assertions : t -> bool Expr.t list
  val check : t -> bool Expr.t list -> bool
(*  val model : ?symbols:Symbol.t list -> t -> Model.t option*)
end
