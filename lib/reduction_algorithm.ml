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

(* Temporary cost profile: one line per stage, no renamed copy or full
   expressions. HH logs its substitutions to the same file. *)
let reduction_profile_calls = ref 0

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
  let profile_channel =
    match Sys.getenv_opt "SQBRICKS_PROFILE_HH_COST_FILE" with
    | None -> None
    | Some file ->
        Some
          (open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ]
             0o644 file)
  in
  let call =
    match profile_channel with
    | None -> 0
    | Some _ ->
        incr reduction_profile_calls;
        !reduction_profile_calls
  in
  let iteration = ref 0 in
  let profile_state stage (state : Path_sum.t) =
    match profile_channel with
    | None -> ()
    | Some channel ->
        (* This is the existing reduction metric: sums below a product are
           not counted by Ket.number_of_sum. HH also logs full ket node counts. *)
        fprintf channel
          "REDUCTION_STATE pid=%d call=%d iteration=%d stage=%s width=%d \
           path_vars=%d phase_terms=%d ket_sums=%d\n%!"
          (Unix.getpid ()) call !iteration stage (Array.length state.ket)
          (List.length state.path_var) (Poly.size state.phase)
          (Ket.number_of_sum state.ket)
  in
  let profile_step stage operation =
    match profile_channel with
    | None -> operation ()
    | Some channel ->
        fprintf channel
          "REDUCTION_STEP_BEGIN pid=%d call=%d iteration=%d stage=%s\n%!"
          (Unix.getpid ()) call !iteration stage;
        let wall_start = Unix.gettimeofday () in
        let cpu_start = Sys.time () in
        let result = operation () in
        let cpu_s = Sys.time () -. cpu_start in
        let wall_s = Unix.gettimeofday () -. wall_start in
        fprintf channel
          "REDUCTION_STEP_END pid=%d call=%d iteration=%d stage=%s \
           wall_s=%.6f cpu_s=%.6f\n%!"
          (Unix.getpid ()) call !iteration stage wall_s cpu_s;
        result
  in
  let rec aux acc =
    (match profile_channel with None -> () | Some _ -> incr iteration);
    if debug then printf "Reduction_algorithm, acc =\n%s\n\n" (PSS.pretty acc);

    let state_simpl =
      profile_step "simplification" (fun () -> Rules.Simplification.simplify acc)
    in
    if debug then
      printf "Reduction_algorithm, state_simpl =\n%s\n\n"
        (PSS.pretty state_simpl);
    profile_state "before_hh" state_simpl;
    match profile_step "hh" (fun () -> Rules.HH.hh ~debug state_simpl) with
    | Error reduction_error -> Error reduction_error
    | Ok state_hh ->
        profile_state "after_hh" state_hh;
        if debug then
          printf "Reduction_algorithm, state_hh =\n%s\n\n" (PSS.pretty state_hh);
        match
          profile_step "variable_replacement" (fun () ->
              Rules.Variable_replacement.variable_replacement ~debug state_hh)
        with
        | Error reduction_error -> Error reduction_error
        | Ok (Some state_replace) ->
            profile_state "after_variable_replacement" state_replace;
            if debug then
              printf "Reduction_algorithm, state_replace =\n%s\n\n"
                (PSS.pretty state_replace);
            aux state_replace
        | Ok None ->
            let state_fact =
              let rec aux state_in =
                let state_out =
                  profile_step "factorization" (fun () ->
                      Rules.Variable_replacement.variable_replacement_factorisation
                        state_in ~debug)
                in
                profile_state "factorization_candidate" state_out;
                if debug then
                  printf "Reduction_algorithm.aux, state_out =\n%s\n\n"
                    (PSS.pretty state_out);
                let condition =
                  profile_step "factorization_condition" (fun () ->
                      _condition_to_continue state_in state_out ~debug)
                in
                if debug then
                  printf "Reduction_algorithm.aux, condition = %b\n\n" condition;
                if condition then aux state_out else state_in
              in
              aux state_hh
            in
            profile_state "after_factorization" state_fact;
            if debug then
              printf "Reduction_algorithm, state_fact =\n%s\n\n"
                (PSS.pretty state_fact);
            let state_repl =
              profile_step "affine_replacement" (fun () ->
                  Rules.Variable_replacement.replace_not_path_var_by_var
                    state_fact)
            in
            profile_state "after_affine_replacement" state_repl;
            if debug then
              printf "Reduction_algorithm, state_repl =\n%s\n\n"
                (PSS.pretty state_repl);
            if
              profile_step "continue_condition" (fun () ->
                  _condition_to_continue acc state_repl)
            then aux state_repl
            else Ok state_repl
  in
  Fun.protect
    ~finally:(fun () ->
      match profile_channel with
      | None -> ()
      | Some channel -> close_out_noerr channel)
    (fun () ->
      (match profile_channel with
      | None -> ()
      | Some channel ->
          fprintf channel "REDUCTION_BEGIN pid=%d call=%d\n%!"
            (Unix.getpid ()) call);
      (* The total includes profiling overhead; stage clocks exclude their
         own log writes. HH stage times include the nested HH instrumentation. *)
      let wall_start, cpu_start =
        match profile_channel with
        | None -> (0., 0.)
        | Some _ -> (Unix.gettimeofday (), Sys.time ())
      in
      profile_state "input" input;
      if debug then
        printf "Reduction_algorithm, input =\n%s\n\n" (PSS.pretty input);
      let result =
        match aux input with
        | Ok output ->
            let output =
              profile_step "rename" (fun () -> Rename.rename output)
            in
            profile_state "output" output;
            Ok output
        | Error reduction_error -> Error reduction_error
      in
      (match profile_channel with
      | None -> ()
      | Some channel ->
          let cpu_s = Sys.time () -. cpu_start in
          let wall_s = Unix.gettimeofday () -. wall_start in
          let status = match result with Ok _ -> "ok" | Error _ -> "error" in
          fprintf channel
            "REDUCTION_END pid=%d call=%d iterations=%d status=%s \
             wall_s=%.6f cpu_s=%.6f\n%!"
            (Unix.getpid ()) call !iteration status wall_s cpu_s);
      result)
