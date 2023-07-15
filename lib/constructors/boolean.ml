open Expression
open Types

let const (b : bool) : bool t = Val (Bool b)
let mk_not (e : bool t) : bool t = Unop (Bool B.Not, e)
let mk_and (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool B.And, e1, e2)
let mk_or (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool B.Or, e1, e2)
let mk_xor (e1 : bool t) (e2 : bool t) : bool t = Binop (Bool B.Xor, e1, e2)
let mk_eq (e1 : bool t) (e2 : bool t) : bool t = Relop (Bool B.Eq, e1, e2)
let mk_ne (e1 : bool t) (e2 : bool t) : bool t = Relop (Bool B.Ne, e1, e2)

let mk_ite (e1 : bool t) (e2 : bool t) (e3 : bool t) =
  Triop (Bool B.ITE, e1, e2, e3)
