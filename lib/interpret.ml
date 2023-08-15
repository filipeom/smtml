module Make (Solver : Solver_intf.S) = struct
  open Core
  open Ast

  type exec_state =
    { stmts : Ast.t list
    ; smap : (string, string) Hashtbl.t
    ; pc : bool Expr.t list
    ; solver : Solver.t
    }

  let exec_stmt (stmt : Ast.t) ({ solver; pc; _ } as state : exec_state) :
    exec_state =
    let st pc = { state with pc } in
    match stmt with
    | Declare _x ->
      (* Hashtbl.add_exn smap ~key:(Symbol.to_string x) ~data:(Symbol.type_of x); *)
      st pc
    | Assert e ->
      Solver.add solver [ e ];
      st @@ (e :: pc)
    | CheckSat ->
      if Solver.check solver [] then printf "sat\n" else printf "unsat\n";
      st pc
    | GetModel ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      printf "%s" (Model.to_string (Option.value_exn model));
      st pc

  let rec loop (state : exec_state) : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts -> loop (exec_stmt stmt { state with stmts })

  let start (stmts : Ast.t list) : unit =
    let c =
      { stmts
      ; smap = Hashtbl.create (module String)
      ; solver = Solver.create ()
      ; pc = []
      }
    in
    ignore (loop c)
end
