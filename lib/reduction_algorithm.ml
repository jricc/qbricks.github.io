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
  if debug then printf "Reduction_algorithm, input =\n%s\n\n" (PSS.pretty input);

  let rec aux acc =
    if debug then printf "Reduction_algorithm, acc =\n%s\n\n" (PSS.pretty acc);

    let state_simpl = Rules.Simplification.simplify acc in
    if debug then
      printf "Reduction_algorithm, state_simpl =\n%s\n\n"
        (PSS.pretty state_simpl);
    match Rules.HH.hh ~debug state_simpl with
    | Error reduction_error -> Error reduction_error
    | Ok state_hh ->
        if debug then
          printf "Reduction_algorithm, state_hh =\n%s\n\n" (PSS.pretty state_hh);
        match
          Rules.Variable_replacement.variable_replacement ~debug
            state_hh
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
                  Rules.Variable_replacement.variable_replacement_factorisation
                    state_in ~debug
                in
                if debug then
                  printf "Reduction_algorithm.aux, state_out =\n%s\n\n"
                    (PSS.pretty state_out);
                let condition =
                  _condition_to_continue state_in state_out ~debug
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
              Rules.Variable_replacement.replace_not_path_var_by_var state_fact
            in
            if debug then
              printf "Reduction_algorithm, state_repl =\n%s\n\n"
                (PSS.pretty state_repl);
            if _condition_to_continue acc state_repl then aux state_repl
            else Ok state_repl
  in
  match aux input with
  | Ok output -> Ok (Rename.rename output)
  | Error reduction_error -> Error reduction_error
