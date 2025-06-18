open Ppxlib

(* Extension: let%view pat = expr in body *)

let expand_view_pattern ~loc pat expr body =
  let open Ast_builder.Default in
  let view_expr =
    pexp_apply ~loc
      (pexp_ident ~loc (Located.lident ~loc "Expr.view"))
      [ (Nolabel, expr) ]
  in
  let case1 = case ~lhs:pat ~guard:None ~rhs:body in
  let fail_rhs =
    pexp_apply ~loc
      (pexp_ident ~loc (Located.lident ~loc "raise"))
      [ ( Nolabel
        , pexp_construct ~loc
            (Located.lident ~loc "Match_failure")
            (Some
               (pexp_tuple ~loc
                  [ estring ~loc __LOC__; eint ~loc 0; eint ~loc 0 ] ) ) )
      ]
  in
  let case2 = case ~lhs:(ppat_any ~loc) ~guard:None ~rhs:fail_rhs in
  pexp_match ~loc view_expr [ case1; case2 ]

let rule =
  Extension.V3.declare "view" Extension.Context.Expression
    Ast_pattern.(
      single_expr_payload
        (pexp_let nonrecursive (value_binding ~pat:__ ~expr:__ ^:: nil) __) )
    (fun ~ctxt:_ pat expr body ->
      expand_view_pattern ~loc:__LOC__ pat expr body)

let () = Driver.register_transformation "smtml" ~extensions:[ rule ]
