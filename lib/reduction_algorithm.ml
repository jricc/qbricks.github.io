(**************************************************************************)
(*  This file is part of SQbricks.                                        *)
(*                                                                        *)
(*  Copyright (C) 2022-2026                                               *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*  Université Paris-Saclay                                               *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Path_sum
module PSS = Path_sum.String
open Rules
open Printf

(* Temporary profiler enabled by [SQBRICKS_PROFILE_REDUCTION_FILE]. Number
   successive reductions inside one process; for example, Equiv.parallel
   reduces its two circuits in calls 1 and 2. *)
let reduction_profile_call = ref 0

type profile_step = { name : string; mutable calls : int; mutable seconds : float }

let make_profile_step name = { name; calls = 0; seconds = 0.0 }

let measure profile_enabled step operation =
  if not profile_enabled then operation ()
  else (
    step.calls <- step.calls + 1;
    let start_time = Unix.gettimeofday () in
    let result = operation () in
    step.seconds <- step.seconds +. (Unix.gettimeofday () -. start_time);
    result)

let path_sum_profile_size (state : Path_sum.t) =
  ( List.length state.path_var,
    Poly.size state.phase,
    Ket.number_of_sum state.ket )

let _condition_to_continue ?(debug = false) (input : Path_sum.t)
    (output : Path_sum.t) =
  let len_in = List.length input.path_var in
  let len_out = List.length output.path_var in
  let nb_of_sum_in = Ket.number_of_sum input.ket + Poly.size input.phase in
  let nb_of_sum_out = Ket.number_of_sum output.ket + Poly.size output.phase in
  if debug then
    printf
      "Reduction_algorithm._condition_to_continue, len_in = %d, len_out = %d, \
       nb_of_sum_in = %d, nb_of_sum_out = %d\n\n"
      len_in len_out nb_of_sum_in nb_of_sum_out;
  (len_out < len_in && 0 < len_out) || nb_of_sum_out < nb_of_sum_in

let reduction_algorithm ?(debug = false) input =
  let profile_file = Sys.getenv_opt "SQBRICKS_PROFILE_REDUCTION_FILE" in
  let profile_enabled =
    match profile_file with Some _ -> true | None -> false
  in
  let profile_call =
    if profile_enabled then (
      incr reduction_profile_call;
      !reduction_profile_call)
    else 0
  in
  let profile_start =
    if profile_enabled then Unix.gettimeofday () else 0.0
  in
  let outer_iterations = ref 0 in
  let simplification = make_profile_step "simplification" in
  let hh = make_profile_step "hh" in
  let variable_replacement = make_profile_step "variable_replacement" in
  let factorisation = make_profile_step "factorisation" in
  let replace_not_path_var = make_profile_step "replace_not_path_var" in
  let condition = make_profile_step "condition" in
  let rename = make_profile_step "rename" in
  let profile_steps =
    [ simplification; hh; variable_replacement; factorisation;
      replace_not_path_var; condition; rename ]
  in
  let input_path_vars, input_phase_monomes, input_ket_sums =
    if profile_enabled then path_sum_profile_size input else (0, 0, 0)
  in
  let write_profile status final_state =
    match profile_file with
    | None -> ()
    | Some file ->
        let output_path_vars, output_phase_monomes, output_ket_sums =
          match final_state with
          | Some state -> path_sum_profile_size state
          | None -> (-1, -1, -1)
        in
        let total_seconds = Unix.gettimeofday () -. profile_start in
        let measured_seconds =
          List.fold_left
            (fun total step -> total +. step.seconds)
            0.0 profile_steps
        in
        let profile_channel =
          open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ] 0o644
            file
        in
        fprintf profile_channel
          "REDUCTION_PROFILE pid=%d call=%d status=%s total_s=%.6f \
           unmeasured_s=%.6f outer_iterations=%d input_path_vars=%d \
           input_phase_monomes=%d input_ket_sums=%d output_path_vars=%d \
           output_phase_monomes=%d output_ket_sums=%d\n"
          (Unix.getpid ()) profile_call status total_seconds
          (total_seconds -. measured_seconds)
          !outer_iterations input_path_vars input_phase_monomes input_ket_sums
          output_path_vars output_phase_monomes output_ket_sums;
        List.iter
          (fun step ->
            fprintf profile_channel
              "REDUCTION_STEP pid=%d call=%d step=%s calls=%d seconds=%.6f\n"
              (Unix.getpid ()) profile_call step.name step.calls step.seconds)
          profile_steps;
        close_out profile_channel
  in
  if debug then printf "Reduction_algorithm, input =\n%s\n\n" (PSS.pretty input);

  let rec aux acc =
    if profile_enabled then incr outer_iterations;
    if debug then printf "Reduction_algorithm, acc =\n%s\n\n" (PSS.pretty acc);

    let state_simpl =
      measure profile_enabled simplification (fun () ->
          Rules.Simplification.simplify acc)
    in
    if debug then
      printf "Reduction_algorithm, state_simpl =\n%s\n\n"
        (PSS.pretty state_simpl);
    match
      measure profile_enabled hh (fun () -> Rules.HH.hh ~debug state_simpl)
    with
    | Error reduction_error -> Error reduction_error
    | Ok state_hh ->
        if debug then
          printf "Reduction_algorithm, state_hh =\n%s\n\n" (PSS.pretty state_hh);
        match measure profile_enabled variable_replacement (fun () ->
                  Rules.Variable_replacement.variable_replacement ~debug
                    state_hh)
        with
        | Error reduction_error -> Error reduction_error
        | Ok (Some state_replace) ->
            if debug then
              printf "Reduction_algorithm, state_replace =\n%s\n\n"
                (PSS.pretty state_replace);
            aux state_replace
        | Ok None ->
            let state_fact =
              let rec aux state_in =
                let state_out =
                  measure profile_enabled factorisation (fun () ->
                      Rules.Variable_replacement
                      .variable_replacement_factorisation state_in ~debug)
                in
                if debug then
                  printf "Reduction_algorithm.aux, state_out =\n%s\n\n"
                    (PSS.pretty state_out);
                let condition =
                  measure profile_enabled condition (fun () ->
                      _condition_to_continue state_in state_out ~debug)
                in
                if debug then
                  printf "Reduction_algorithm.aux, condition = %b\n\n" condition;
                if condition then aux state_out else state_in
              in
              aux state_hh
            in
            if debug then
              printf "Reduction_algorithm, state_fact =\n%s\n\n"
                (PSS.pretty state_fact);
            let state_repl =
              measure profile_enabled replace_not_path_var (fun () ->
                  Rules.Variable_replacement.replace_not_path_var_by_var
                    state_fact)
            in
            if debug then
              printf "Reduction_algorithm, state_repl =\n%s\n\n"
                (PSS.pretty state_repl);
            if
              measure profile_enabled condition (fun () ->
                  _condition_to_continue acc state_repl)
            then aux state_repl
            else Ok state_repl
  in
  match aux input with
  | Ok output ->
      let renamed_output =
        measure profile_enabled rename (fun () -> Rename.rename output)
      in
      write_profile "ok" (Some renamed_output);
      Ok renamed_output
  | Error reduction_error ->
      write_profile "error" None;
      Error reduction_error
