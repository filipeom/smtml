let run e =
  let%view Cvtop (_, ToString, r) = e in
  Format.printf "Got r: %a@." Expr.pp r
