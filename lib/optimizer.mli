open Z3_mappings

type t

exception Unknown

val solver_time : float ref
val create : unit -> t
val push : t -> unit
val pop : t -> unit
val add : t -> _ Expression.t list -> unit

val check :
     t
  -> 'a Expression.t
  -> _ Expression.t list
  -> (t -> 'a Expression.t -> Z3.Optimize.handle)
  -> model Option.t

val maximize : t -> 'a Expression.t -> _ Expression.t list -> 'a Value.t option
val minimize : t -> 'a Expression.t -> _ Expression.t list -> 'a Value.t option
