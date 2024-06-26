(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

include Solver_intf

module Base (M : Mappings_intf.S) = struct
  type t =
    { solver : M.solver
    ; mutable mode : solver_mode
    }

  let solver_time = ref 0.0

  let solver_count = ref 0

  let pp_statistics _fmt _solver = ()

  let create ?params ?logic () =
    { solver = M.Solver.make ?params ?logic (); mode = Assert_mode }

  (* let clone { solver; mode } = { solver = M.Solver.clone solver; mode } *)

  let reset s =
    M.Solver.reset s.solver;
    s.mode <- Start_mode

  let reset_assertions (_solver : t) : unit = ()

  let set_logic _ = assert false

  let set_option _ = assert false

  let push { solver; _ } _n = M.Solver.push solver

  let pop { solver; _ } n = M.Solver.pop solver n

  let assert_ { solver; _ } htes = M.Solver.add solver htes

  let add = assert_

  let get_assertions _ = assert false

  let get_statistics { solver; _ } = M.Solver.get_statistics solver

  let check_sat { solver; _ } ~assumptions =
    incr solver_count;
    solver_count := !solver_count + 1;
    Utils.run_and_time_call
      ~use:(fun time -> solver_time := !solver_time +. time)
      (fun () -> M.Solver.check solver ~assumptions)

  let check s assumptions = check_sat s ~assumptions

  let get_value { solver; _ } e =
    match M.Solver.model solver with
    | Some m -> Expr.make @@ Val (M.value m e)
    | None ->
      Fmt.failwith "get_value: Trying to get a value from an unsat solver"

  let get_model ?symbols { solver; mode } =
    match mode with
    | Sat_mode -> (
      match M.Solver.model solver with
      | Some m -> M.values_of_model ?symbols m
      | None -> assert false )
    | Unsat_mode -> assert false
    | Assert_mode | Start_mode -> assert false

  let model ?symbols s = Some (get_model ?symbols s)
end

module Make_batch (Mappings : Mappings_intf.S) = struct
  module Backend = Base (Mappings)

  type solver = Backend.t

  type t =
    { solver : solver
    ; mutable top : Expr.t list
    ; stack : Expr.t list Stack.t
    }

  let solver_time = Backend.solver_time

  let solver_count = Backend.solver_count

  let pp_statistics fmt s = Backend.pp_statistics fmt s.solver

  let create ?params ?logic () =
    { solver = Backend.create ?params ?logic ()
    ; top = []
    ; stack = Stack.create ()
    }

  (* let clone { solver; top; stack } = *)
  (*   { solver = Backend.clone solver; top; stack = Stack.copy stack } *)

  let reset s =
    Backend.reset s.solver;
    Stack.clear s.stack;
    s.top <- []

  let reset_assertions _ = assert false

  let set_logic _ = assert false

  let set_option _ = assert false

  let push { top; stack; solver } n =
    Backend.push solver n;
    Stack.push top stack

  let rec pop s n =
    assert (n <= Stack.length s.stack);
    if n <= 0 then ()
    else begin
      Backend.pop s.solver 1;
      s.top <- Stack.pop s.stack;
      pop s (n - 1)
    end

  let assert_ s es = s.top <- es @ s.top

  let add = assert_

  let get_assertions s = s.top [@@inline]

  let get_statistics s = Backend.get_statistics s.solver

  let check_sat s ~assumptions =
    Backend.check_sat s.solver ~assumptions:(assumptions @ s.top)

  let check s assumptions = check_sat s ~assumptions

  let get_value s hte = Backend.get_value s.solver hte

  let get_model ?symbols s = Backend.get_model ?symbols s.solver

  let model ?symbols s = Some (get_model ?symbols s)
end

(* TODO: Our base solver can be incrmental itself? *)
module Batch (M : Mappings_intf.S) : Solver_intf.S = Make_batch (M)

module Cached (M : Mappings_intf.S) : sig
  include Solver_intf.S

  module Cache : Cache_intf.S
end = struct
  include Make_batch (M)
  module Cache = Cache.Strong

  let cache = Cache.create 256

  let check s es =
    match Cache.find_opt cache es with
    | Some res -> res
    | None ->
      let result = check s es in
      Cache.add cache es result;
      result
end

module Incremental (M : Mappings_intf.S) : Solver_intf.S = Base (M)
