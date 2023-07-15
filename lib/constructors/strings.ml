open Expression
open Types

let mk_val (s : string) : string t = Val (Str s)
let mk_len (s : string t) : int t = Unop (Str S.Len, s)
let mk_nth (s : string t) (i : string t) : string t = Binop (Str S.Nth, s, i)

let mk_concat (s1 : string t) (s2 : string t) : string t =
  Binop (Str S.Concat, s1, s2)

let mk_eq (s1 : string t) (s2 : string t) : bool t = Relop (Str S.Eq, s1, s2)
let mk_ne (s1 : string t) (s2 : string t) : bool t = Relop (Str S.Ne, s1, s2)
let mk_trim (s : string t) : string t = Unop (Str S.Trim, s)

let mk_substr (s : string t) ~(pos : int t) ~(len : int t) : string t =
  Triop (Str S.SubStr, s, pos, len)
