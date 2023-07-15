open Core

type 'a t = (Symbol.t, 'a Value.t) Hashtbl.t

val get_symbols : 'a t -> Symbol.t List.t
val get_bindings : 'a t -> (Symbol.t * 'a Value.t) List.t
val evaluate : 'a t -> Symbol.t -> 'a Value.t Option.t
val to_string : 'a t -> String.t
