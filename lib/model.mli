type t

val create : unit -> t
val add : t -> Symbol.symbol -> Value.value -> unit
val get_symbols : t -> Symbol.symbol list
val get_bindings : t -> (Symbol.symbol * Value.value) list
val evaluate : t -> Symbol.symbol -> Value.value option
val to_string : t -> string
