open Type

type t

val mk_symbol : expr_type -> string -> t
val equal : t -> t -> bool
val rename : t -> string -> t
val type_of : t -> expr_type
val to_string : t -> string
