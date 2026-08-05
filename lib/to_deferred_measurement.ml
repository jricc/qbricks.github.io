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

open Common
include Rational
open Printf
include Program
module ProgS = Program.String
include Program.Macros

type deferred_measurement_error =
  | InvalidClassicalBit of int * int
  | InvalidQubitIndex of int * int
  | ResetOfUsedQubitUnsupported of int
  | MeasuredQubitUsedAfterMeasurement of Program.t
  | UnsupportedConditionalProgram of Program.t

let deferred_measurement_error_message = function
  | InvalidClassicalBit (width, bit) ->
      sprintf
        "Translation.to_deferred_measurements, invalid classical bit %d for \
         width %d"
        bit width
  | InvalidQubitIndex (width, qubit) ->
      sprintf
        "Translation.to_deferred_measurements, invalid qubit %d for width %d"
        qubit width
  | ResetOfUsedQubitUnsupported qubit ->
      sprintf
        "Translation.to_deferred_measurements, reset/init of already used qubit \
         %d is not supported"
        qubit
  | MeasuredQubitUsedAfterMeasurement p ->
      sprintf
        "Translation.to_deferred_measurements, measured qubit used after \
         measurement in p = %s"
        (ProgS.pretty p)
  | UnsupportedConditionalProgram p ->
      sprintf
        "Translation.to_deferred_measurements, unsupported conditional p = %s"
        (ProgS.pretty p)

let to_deferred_measurements_result ?(debug = false) p =
  let p = Program.format p in
  let wc, wq = Program.widths p in
  if debug then printf "1. Translation.to_deferred_measurements, wq = %d\n\n" wq;
  if debug then
    printf "2. Translation.to_deferred_measurements, p =\n%s\n\n"
      (ProgS.pretty p);

  (* Classical bits start at zero. Once measured, [bit_to_qubit] identifies the
     qubit that carries their deferred value instead. *)
  let bit_to_qubit = Array.make wc (-1) in
  let constant_bit_values = Array.make wc false in

  let check_classical_bit bit =
    if bit < 0 || wc <= bit then Error (InvalidClassicalBit (wc, bit))
    else Ok ()
  in

  let check_qubit qubit =
    if qubit < 0 || wq <= qubit then Error (InvalidQubitIndex (wq, qubit))
    else Ok ()
  in

  let rec check_qubits = function
    | [] -> Ok ()
    | qubit :: qubits -> (
        match check_qubit qubit with
        | Ok () -> check_qubits qubits
        | Error error -> Error error)
  in

  let classical_control_of_bit bit =
    match check_classical_bit bit with
    | Error error -> Error error
    | Ok () ->
        let qubit = bit_to_qubit.(bit) in
        if qubit = -1 then Ok (`Constant constant_bit_values.(bit))
        else Ok (`Measured qubit)
  in

  let is_wires_available measured_qubits wires_to_check =
    let rec aux wires_to_check =
      match wires_to_check with
      | [] -> true
      | wire :: _ when List.mem wire measured_qubits -> false
      | _ :: wires_to_check_remain -> aux wires_to_check_remain
    in
    aux wires_to_check
  in

  let add_used_qubits qubits used_qubits =
    List.sort_uniq Int.compare (qubits @ used_qubits)
  in

  let rec aux ?(debug = false) p inits meas used_qubits =
    match p with
    | Apply (_, co, ta) -> (
        match check_qubits (co @ ta) with
        | Error error -> Error error
        | Ok () ->
            if is_wires_available meas (co @ ta) then
              Ok (p, inits, meas, add_used_qubits (co @ ta) used_qubits)
            else Error (MeasuredQubitUsedAfterMeasurement p))
    | Measure (qubit_indice, bit_indice) ->
        if debug then
          printf
            "Translation.to_deferred_measurements.Measure, qubit_indice = %d, \
             bit_indice = %d\n\n"
            qubit_indice bit_indice;
        (match (check_qubit qubit_indice, check_classical_bit bit_indice) with
        | Error error, _ | _, Error error -> Error error
        | Ok (), Ok () ->
            bit_to_qubit.(bit_indice) <- qubit_indice;
            constant_bit_values.(bit_indice) <- false;
            Ok
              ( E,
                inits,
                qubit_indice :: meas,
                add_used_qubits [ qubit_indice ] used_qubits ))
    | It (bits_indices, Apply (g, qubits_indices_co, qubits_indices_ta)) -> (
        if debug then
          printf
            "Translation.to_deferred_measurements.It, bits_indices = %s\n\n"
            (ListBis.string_int bits_indices);

        let rec bits_to_qubits bits_indices =
          match bits_indices with
          | [] -> Ok (Some [])
          | bit_indice :: bits_indices_remain -> (
              match classical_control_of_bit bit_indice with
              | Error error -> Error error
              | Ok (`Constant false) -> Ok None
              | Ok (`Constant true) -> bits_to_qubits bits_indices_remain
              | Ok (`Measured qubit_indice) -> (
                  if debug then
                    printf
                      "Translation.to_deferred_measurements.bits_to_qubits, \
                       qubit_indice = %d\n\n"
                      qubit_indice;
                  match bits_to_qubits bits_indices_remain with
                  | Ok (Some qubits_indices_remain) ->
                      Ok (Some (qubit_indice :: qubits_indices_remain))
                  | Ok None -> Ok None
                  | Error error -> Error error))
        in

        match check_qubits (qubits_indices_co @ qubits_indices_ta) with
        | Error error -> Error error
        | Ok () ->
            if
              not
                (is_wires_available meas
                   (qubits_indices_co @ qubits_indices_ta))
            then Error (MeasuredQubitUsedAfterMeasurement p)
            else
              (match bits_to_qubits bits_indices with
              | Error error -> Error error
              | Ok None -> Ok (E, inits, meas, used_qubits)
              | Ok (Some qubits_indices) ->
                  (if debug then
                    printf
                      "Translation.to_deferred_measurements.It, qubits_indices \
                       = %s\n\n"
                      (ListBis.string_int qubits_indices));
                  Ok
                    ( Apply
                        ( g,
                          List.sort_uniq Int.compare
                            (qubits_indices @ qubits_indices_co),
                          qubits_indices_ta ),
                      inits,
                      meas,
                      add_used_qubits
                        (qubits_indices @ qubits_indices_co @ qubits_indices_ta)
                        used_qubits )))
    | It (_, E) -> Ok (E, inits, meas, used_qubits)
    | It ([ bit_indice ], Sequence (p1, p2)) ->
        aux ~debug
          (Sequence (It ([ bit_indice ], p1), It ([ bit_indice ], p2)))
          inits meas used_qubits
    | It _ -> Error (UnsupportedConditionalProgram p)
    | Sequence (p1, p2) -> (
        match aux ~debug p1 inits meas used_qubits with
        | Error error -> Error error
        | Ok (p1', l1, l1', used_qubits') -> (
            match aux ~debug p2 l1 l1' used_qubits' with
            | Error error -> Error error
            | Ok (p2', l2, l2', used_qubits'') ->
                (* Translation can turn either side into [E], notably when a
                   condition is decided from constant classical bits. *)
                let deferred_sequence =
                  match (p1', p2') with
                  | E, program | program, E -> program
                  | _ -> Sequence (p1', p2')
                in
                Ok (deferred_sequence, l2, l2', used_qubits'')))
    | E -> Ok (E, inits, meas, used_qubits)
    | Not bit_indice -> (
        match classical_control_of_bit bit_indice with
        | Error error -> Error error
        | Ok (`Constant value) ->
            constant_bit_values.(bit_indice) <- not value;
            Ok (E, inits, meas, used_qubits)
        | Ok (`Measured qubit_indice) ->
            Ok
              ( x qubit_indice,
                inits,
                meas,
                add_used_qubits [ qubit_indice ] used_qubits ))
    | InitQ ta -> (
        match check_qubit ta with
        | Error error -> Error error
        | Ok () ->
            if List.mem ta used_qubits then
              Error (ResetOfUsedQubitUnsupported ta)
            else Ok (E, ta :: inits, meas, used_qubits))
  in
  match aux ~debug p [] [] [] with
  | Error error -> Error error
  | Ok (deferred_measurement, inits, meas, _) ->
      let dm = Program.format deferred_measurement in
      let inits = List.rev (List.sort_uniq Int.compare inits) in
      let meas = List.sort_uniq Int.compare meas in
      Ok (dm, inits, meas)

let to_deferred_measurements ?debug p =
  match to_deferred_measurements_result ?debug p with
  | Ok result -> result
  | Error error -> failwith (deferred_measurement_error_message error)
