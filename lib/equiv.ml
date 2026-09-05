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

module ProgS = Program.String
include Program.Macros
module QS = Qubit.String
module PSS = Path_sum.String
module Ket = Path_sum.Ket
module Monome = Poly.Monome
module PS = Poly.String
open Printf
open Common
open Rules

type result =
  | SubCircuitEquivalent
  | FullCircuitEquivalent
  | GlobalPhaseEquivalent
  | NotEquivDiffMeasurements
  | NotEquivDiffOutputs
  | NotEquivDiffInputs
  | NotEquivDiffInputsOutputs
  | SubCircuitInconclusive
  | Entanglement1
  | Entanglement2
  | GlobalPhaseInconclusive
  | FullCircuitInconclusive
  | FullCircuitInconclusiveAmp
  | FullCircuitInconclusiveKet
  | ErrorCircuitNotUnitary
  | ErrorInvalidQubitIndex
  | ErrorInvalidProgram
  | ErrorFullCircuitNotImplemented
  | ErrorBothCircuitsHaveInits
  | ErrorMalformedPathSum

let result_to_string = function
  | SubCircuitEquivalent -> "SubCircuitEquivalent"
  | FullCircuitEquivalent -> "FullCircuitEquivalent"
  | GlobalPhaseEquivalent -> "GlobalPhaseEquivalent"
  | NotEquivDiffMeasurements -> "NotEquivDiffMeasurements"
  | NotEquivDiffOutputs -> "NotEquivDiffOutputs"
  | NotEquivDiffInputs -> "NotEquivDiffInputs"
  | NotEquivDiffInputsOutputs -> "NotEquivDiffInputsOutputs"
  | SubCircuitInconclusive -> "SubCircuitInconclusive"
  | Entanglement1 -> "Entanglement1"
  | Entanglement2 -> "Entanglement2"
  | GlobalPhaseInconclusive -> "GlobalPhaseInconclusive"
  | FullCircuitInconclusive -> "FullCircuitInconclusive"
  | FullCircuitInconclusiveAmp -> "FullCircuitInconclusiveAmp"
  | FullCircuitInconclusiveKet -> "FullCircuitInconclusiveKet"
  | ErrorCircuitNotUnitary -> "ErrorCircuitNotUnitary"
  | ErrorInvalidQubitIndex -> "ErrorInvalidQubitIndex"
  | ErrorInvalidProgram -> "ErrorInvalidProgram"
  | ErrorFullCircuitNotImplemented -> "ErrorFullCircuitNotImplemented"
  | ErrorBothCircuitsHaveInits -> "ErrorBothCircuitsHaveInits"
  | ErrorMalformedPathSum -> "ErrorMalformedPathSum"

type equivalence = SubCircuit | FullCircuit | GlobalPhase

let reduction_for_equiv ?(debug = false) state =
  match Reduction_algorithm.reduction_algorithm ~debug state with
  | Ok reduced_state -> Ok reduced_state
  | Error (Rules.MalformedPathSum _) -> Error ErrorMalformedPathSum

let path_sum_equal_for_equiv ?(debug = false) ?(outputs1 = []) ?(outputs2 = [])
    ?(global_phase = false) state1 state2 =
  match
    Path_sum.equal_result ~debug ~outputs1 ~outputs2 ~global_phase state1 state2
  with
  | Ok are_equal -> Ok are_equal
  | Error Path_sum.DifferentOutputLengths -> Error NotEquivDiffOutputs
  | Error Path_sum.InvalidOutputIndex -> Error ErrorInvalidQubitIndex
  | Error Path_sum.IncompatiblePhaseWidths
  | Error Path_sum.IncompletePhasePathVariableMap ->
      Error ErrorMalformedPathSum

(* Path-sum initialization failures come from invalid user-facing qubit indices. *)
let initialized_path_sum_for_equiv ?(debug = false) width inits =
  match Path_sum.ofSize_init_result ~debug width inits with
  | Ok path_sum -> Ok path_sum
  | Error Path_sum.InvalidWidth | Error Path_sum.InvalidInitIndex ->
      Error ErrorInvalidQubitIndex

let apply_swap_for_equiv different_lengths_result program targets1 targets2 =
  match Program.Macros.apply_swap_result program targets1 targets2 with
  | Ok swapped_program -> Ok swapped_program
  | Error Program.Macros.DifferentSwapLengths ->
      Error different_lengths_result
  | Error Program.Macros.InvalidSwapPlace -> Error ErrorInvalidProgram

let inverse_for_equiv program =
  match Program.inverse_result program with
  | Ok inverse -> Ok inverse
  | Error (Program.NonReversibleProgram _) -> Error ErrorCircuitNotUnitary

let execution_for_equiv ?debug ?input_state program =
  match Program.execution_result ?debug ?input_state program with
  | Ok state -> Ok state
  | Error (Program.EmptyTargetList _)
  | Error (Program.InvalidGateApplication _)
  | Error (Program.InputStateTooSmall _)
  | Error (Program.NonDyadicRotationAngle _) ->
      Error ErrorInvalidProgram
  | Error (Program.HybridProgram _) -> Error ErrorCircuitNotUnitary

let program_has_valid_gate_applications width program =
  let gate_indices_are_valid controls targets =
    ListBis.valid_indices width controls && ListBis.valid_indices width targets
  in
  let controls_are_distinct_from_targets controls targets =
    not
      (List.exists
         (fun control -> ListBis.member control targets Int.equal)
         controls)
  in
  let rec aux = function
    | Program.Apply (Gates.GP _, controls, targets) ->
        gate_indices_are_valid controls targets
        && controls_are_distinct_from_targets controls targets
    | Program.Apply (Gates.U1 _, controls, targets) ->
        (not (List.is_empty targets))
        && gate_indices_are_valid controls targets
        && controls_are_distinct_from_targets controls targets
    | Program.Apply (_, controls, targets) ->
        (not (List.is_empty targets))
        && gate_indices_are_valid controls targets
        && controls_are_distinct_from_targets controls targets
    | Program.Sequence (program1, program2) -> aux program1 && aux program2
    | Program.E -> true
    | Program.Measure _ | Program.It _ | Program.InitQ _ | Program.Not _ ->
        false
  in
  aux program

let compare_inputs_with_identity ?(debug = false) inputs
    (output_state : Path_sum.t) (identity_state : Path_sum.t) =
  if List.is_empty inputs then
    path_sum_equal_for_equiv ~debug output_state identity_state
  else
    let width = Array.length identity_state.ket in
    let output_width = Array.length output_state.ket in
    if
      not
        (ListBis.valid_indices width inputs
        && ListBis.valid_indices output_width inputs)
    then Error ErrorInvalidQubitIndex
    else
      let rec aux = function
        | input :: inputs' -> (
            let q_expect = identity_state.ket.(input) in
            let q_greet = output_state.ket.(input) in
            if debug then
              printf
                "Equiv.compare_inputs_with_identity, q_expect = %s, q_greet = \
                 %s\n\n\
                 %!"
                (QS.pretty q_expect width) (QS.pretty q_greet width);

            (* A qubit comparison error means the reduced path-sum metadata is
               malformed. *)
            match Qubit.equal_result ~debug q_greet q_expect with
            | Error Qubit.IncompatibleWidths
            | Error Qubit.IncompletePathVariableMap ->
                Error ErrorMalformedPathSum
            | Ok true -> aux inputs'
            | Ok false -> Ok false)
        | _ -> Ok true
      in
      aux inputs

let separability_states ?(debug = false) (state : Path_sum.t) outputs wq =
  if debug then
    printf "Equiv.separability_states, state =\n%s\n\n" (PSS.pretty state);
  let width = Array.length state.ket in
  if wq < 0 || width < wq || not (ListBis.valid_indices wq outputs) then
    Error ErrorInvalidQubitIndex
  else
    let garbages = ListBis.missing_in_range outputs wq in
    let var_output =
      List.sort_uniq Int.compare (Ket.extract_var state.ket outputs)
    in
    let var_garbage =
      List.sort_uniq Int.compare (Ket.extract_var state.ket garbages)
    in

    (* If an external variable is in garbage, outputs and garbages are not separable. *)
    let var_garbage_contents_external_variables =
      List.exists (fun i -> i < width) var_garbage
    in
    if debug then
      printf
        "Equiv.separability, var_garbage_contents_external_variables = %b\n\n%!"
        var_garbage_contents_external_variables;
    if var_garbage_contents_external_variables then Ok false
    else (
      if debug then
        printf "Equiv.separability, garbages = %s\n\n%!"
          (ListBis.string_int garbages);
      if debug then
        printf "Equiv.separability, outputs = %s\n\n%!"
          (ListBis.string_int outputs);
      if debug then
        printf "Equiv.separability, var_output = %s\n\n%!"
          (ListBis.string_int var_output);
      if debug then
        printf "Equiv.separability, var_garbage = %s\n\n%!"
          (ListBis.string_int var_garbage);
      let rec aux = function
        | hd :: tl ->
            if ListBis.member hd var_garbage Int.equal then false else aux tl
        | [] -> true
      in
      if not (aux var_output) then Ok false
      else
        let poly_sep =
          Poly.separable_in_poly state.phase var_output var_garbage
        in
        if debug then printf "Equiv.separability, poly_sep = %b\n\n%!" poly_sep;
        Ok poly_sep)

let parameters_preparation ?(debug = false) inputs1 inputs2 outputs1 outputs2
    unitary1 unitary2 =
  let inputs1 = List.sort_uniq Int.compare inputs1 in
  let inputs2 = List.sort_uniq Int.compare inputs2 in
  let outputs1 = List.sort_uniq Int.compare outputs1 in
  let outputs2 = List.sort_uniq Int.compare outputs2 in
  if debug then
    printf "Equiv.parameters_preparation, inputs1 = %s\n\n%!"
      (ListBis.string_int inputs1);
  if debug then
    printf "Equiv.parameters_preparation, inputs2 = %s\n\n%!"
      (ListBis.string_int inputs2);
  if debug then
    printf "Equiv.parameters_preparation, outputs1 = %s\n\n%!"
      (ListBis.string_int outputs1);
  if debug then
    printf "Equiv.parameters_preparation, outputs2 = %s\n\n%!"
      (ListBis.string_int outputs2);

  let wc1, wq1 = Program.widths unitary1 in
  let wc2, wq2 = Program.widths unitary2 in
  if debug then
    printf
      "Equiv.parameters_preparation, wc1 = %d, wc2 = %d, wq1 = %d, wq2 = %d\n\n\
       %!"
      wc1 wc2 wq1 wq2;

  let length_inputs1 = List.length inputs1 in
  let length_inputs2 = List.length inputs2 in
  let max_inputs = Int.max length_inputs1 length_inputs2 in
  let max_wqs = Int.max wq1 wq2 in

  let wq1, no_inits1 =
    if wq1 = length_inputs1 then
      ((if max_inputs = 0 then max_wqs else max_inputs), true)
    else (wq1, false)
  in
  let wq2, no_inits2 =
    if wq2 = length_inputs2 then
      ((if max_inputs = 0 then max_wqs else max_inputs), true)
    else (wq2, false)
  in

  if debug then
    printf
      "Equiv.parameters_preparation, max_inputs = %d, wq1 = %d, wq2 = %d\n\n%!"
      max_inputs wq1 wq2;

  let inputs1 =
    List.rev (if List.is_empty inputs1 then ListBis.range 0 wq1 else inputs1)
  in
  let inputs2 =
    List.rev (if List.is_empty inputs2 then ListBis.range 0 wq2 else inputs2)
  in
  let outputs1 =
    if List.is_empty outputs1 then ListBis.range 0 wq1 else outputs1
  in
  let outputs2 =
    if List.is_empty outputs2 then ListBis.range 0 wq2 else outputs2
  in
  if debug then
    printf "Equiv.parameters_preparation, inputs1 fixed = %s\n\n%!"
      (ListBis.string_int inputs1);
  if debug then
    printf "Equiv.parameters_preparation, inputs2 fixed = %s\n\n%!"
      (ListBis.string_int inputs2);
  if debug then
    printf "Equiv.parameters_preparation, outputs1 fixed = %s\n\n%!"
      (ListBis.string_int outputs1);
  if debug then
    printf "Equiv.parameters_preparation, outputs2 fixed = %s\n\n%!"
      (ListBis.string_int outputs2);

  let length_inputs1 = List.length inputs1 in
  let length_inputs2 = List.length inputs2 in
  let length_outputs1 = List.length outputs1 in
  let length_outputs2 = List.length outputs2 in
  (* Shape mismatches are reported before invalid indices to keep parameter
     errors classified by the user-visible mismatch. *)
  if length_inputs1 <> length_inputs2 then Error NotEquivDiffInputs
  else if length_outputs1 <> length_outputs2 then Error NotEquivDiffOutputs
  else if length_outputs1 <> length_inputs1 then Error NotEquivDiffInputsOutputs
  else if
    not
      (ListBis.valid_indices wq1 inputs1 && ListBis.valid_indices wq2 inputs2
     && ListBis.valid_indices wq1 outputs1
     && ListBis.valid_indices wq2 outputs2)
  then Error ErrorInvalidQubitIndex
  else (
      if debug then
        printf "Equiv.parameters_preparation, unitary_1 =\n%s\n\n"
          (ProgS.pretty unitary1);
      if debug then
        printf "Equiv.parameters_preparation, unitary_2 =\n%s\n\n"
          (ProgS.pretty unitary2);

      (* Hybrid-only constructs such as InitQ are not executable as unitary circuits. *)
      if (not (Program.unitary unitary1)) || not (Program.unitary unitary2) then
        Error ErrorCircuitNotUnitary
      else if
        not
          (program_has_valid_gate_applications wq1 unitary1
          && program_has_valid_gate_applications wq2 unitary2)
      then Error ErrorInvalidProgram
      else if 0 < wc1 || 0 < wc2 then Error ErrorCircuitNotUnitary
      else
        let inits1 =
          if List.is_empty inputs1 || no_inits1 then []
          else ListBis.missing_in_range inputs1 wq1
        in
        let inits2 =
          if List.is_empty inputs2 || no_inits2 then []
          else ListBis.missing_in_range inputs2 wq2
        in
        Ok
          ( wq1,
            wq2,
            inits1,
            inits2,
            inputs1,
            inputs2,
            outputs1,
            outputs2,
            length_inputs1 ))

let check_observable_measurement outputs1 outputs2 meas1 meas2 =
  (* Output lists define the logical correspondence between circuits. Physical
     wire indices may differ, so compare whether each corresponding output is
     measured rather than comparing the measured indices themselves. *)
  let measurement_status outputs measurements =
    List.map (fun output -> List.mem output measurements) outputs
  in
  List.equal Bool.equal
    (measurement_status outputs1 meas1)
    (measurement_status outputs2 meas2)

type phase_equality =
  | SubCircuitEquality
  | GlobalPhaseEquality
  | ConditionalEquality

let phase_equality_to_string = function
  | SubCircuitEquality -> "SubCircuitEquality"
  | GlobalPhaseEquality -> "GlobalPhaseEquality"
  | ConditionalEquality -> "ConditionalEquality"

let seq ?(debug = false) ?(inputs1 = []) ?(inputs2 = []) ?(outputs1 = [])
    ?(outputs2 = []) ?(meas1 = []) ?(meas2 = []) ?(equivalence = SubCircuit)
    unitary1 unitary2 =
  if equivalence = FullCircuit then ErrorFullCircuitNotImplemented
  else
    match
      parameters_preparation ~debug inputs1 inputs2 outputs1 outputs2 unitary1
        unitary2
    with
  | Error result -> result
  | Ok
      ( wq1,
        wq2,
        inits1,
        inits2,
        inputs1,
        inputs2,
        outputs1,
        outputs2,
        length_inputs1 ) ->
      (* Check if the lists of measured qubits are equal in the two circuits for the observable part *)
      if
        not (ListBis.valid_indices wq1 meas1 && ListBis.valid_indices wq2 meas2)
      then
        ErrorInvalidQubitIndex
      else if not (check_observable_measurement outputs1 outputs2 meas1 meas2)
      then (
        if debug then printf "Equiv.seq, list of measurements differents\n\n";
        NotEquivDiffMeasurements)
      else if (not (List.is_empty inits1)) && not (List.is_empty inits2) then
        ErrorBothCircuitsHaveInits
      else
        let unitary1, wq1, unitary2, wq2 =
          if not (List.is_empty inits1) then
            (Program.format unitary1, wq1, Program.format unitary2, wq2)
          else (Program.format unitary2, wq2, Program.format unitary1, wq1)
        in
        if debug then
          printf "Equiv.seq, unitary_1 =\n%s\n\n" (ProgS.pretty unitary1);
        if debug then
          printf "Equiv.seq, unitary_2 =\n%s\n\n" (ProgS.pretty unitary2);

        let width = Int.max wq1 wq2 in
        if debug then printf "Equiv.seq, width = %d\n\n%!" width;

        match initialized_path_sum_for_equiv ~debug width inits1 with
        | Error result -> result
        | Ok input_state ->
            if debug then
              printf "Equiv.seq, input_state =\n%s\n\n%!"
                (PSS.pretty input_state);

            match
              apply_swap_for_equiv NotEquivDiffOutputs unitary1 outputs1
                outputs2
            with
            | Error result -> result
            | Ok unitary1_swap ->
                if debug then
                  printf "Equiv.seq, good order unitary1_swap =\n%s\n\n"
                    (ProgS.pretty unitary1_swap);

                (* Check Separability just after 1st circuit *)
                match execution_for_equiv ~debug ~input_state unitary1_swap with
                | Error result -> result
                | Ok state1 -> (
                    if debug then
                      printf "Equiv.seq, state1 =\n%s\n\n%!" (PSS.pretty state1);

                    match reduction_for_equiv ~debug state1 with
                    | Error result -> result
                    | Ok state1_reduced ->
                    if debug then
                      printf "Equiv.seq, state1_reduced =\n%s\n\n"
                        (PSS.pretty (Rename.rename state1_reduced));

                    (* Use outputs2 because unitary1 has been swapped to the second output order. *)
                    match
                      separability_states ~debug state1_reduced outputs2 width
                    with
                    | Error result -> result
                    | Ok separability ->
                        if debug then
                          printf "Equiv.seq, separability = %b\n\n%!"
                            separability;

                        if separability then (
                          match inverse_for_equiv unitary2 with
                          | Error result -> result
                          | Ok unitary2_inv -> (
                              if debug then
                                printf
                                  "Equiv.seq, good order unitary2_inv =\n%s\n\n"
                                  (ProgS.pretty unitary2_inv);
                              (* [unitary2_inv] returns values to [inputs2].
                                 Move them from [inputs2] to the physical wires
                                 corresponding to [inputs1]. *)
                              match
                                apply_swap_for_equiv NotEquivDiffInputs
                                  unitary2_inv inputs2 inputs1
                              with
                              | Error result -> result
                              | Ok unitary2_swap ->
                                  if debug then
                                    printf
                                      "Equiv.seq, good order unitary2_swap =\n%s\n\n"
                                      (ProgS.pretty unitary2_swap);

                                  (* `[|unit1--unit2^(-1)|] : |x>|0>_init1 -> |output_state>` *)
                                  match
                                    if length_inputs1 = 0 then
                                      execution_for_equiv ~debug
                                        ~input_state:state1 unitary2_inv
                                    else
                                      execution_for_equiv ~debug
                                        ~input_state:state1 unitary2_swap
                                  with
                                  | Error result -> result
                                  | Ok output_state ->
                                      if debug then
                                        printf
                                          "Equiv.seq, output_state =\n%s\n\n%!"
                                          (PSS.pretty output_state);

                                      match
                                        reduction_for_equiv ~debug output_state
                                      with
                                      | Error result -> result
                                      | Ok output_state_reduced ->
                                      if debug then
                                        printf
                                          "Equiv.seq, output_state_reduced =\n%s\n\n"
                                          (PSS.pretty output_state_reduced);

                                      match
                                        initialized_path_sum_for_equiv ~debug
                                          width inits1
                                      with
                                      | Error result -> result
                                      | Ok identity_state ->
                                          let var_inputs =
                                            Ket.extract_var
                                              output_state_reduced.ket inputs1
                                          in
                                          if debug then
                                            printf
                                              "Equiv.seq, var_inputs =\n%s\n\n%!"
                                              (ListBis.string_int var_inputs);

                                          (* Determine the type of phase equality for the reduced output state. *)
                                          let condition_zero_phase_result =
                                            match output_state_reduced.phase with
                                            | phase
                                              when Poly.is_constant phase -> (
                                                (* A constant phase is either zero or a global phase. *)
                                                match
                                                  Poly.equal_result phase
                                                    Poly.zero
                                                with
                                                | Ok true ->
                                                    Ok SubCircuitEquality
                                                | Ok false ->
                                                    Ok GlobalPhaseEquality
                                                | Error _ ->
                                                    Error ErrorMalformedPathSum)
                                            | phase
                                              when not
                                                     (Poly.member_list
                                                        var_inputs phase) ->
                                                (* Phase depends only on path variables. *)
                                                Ok SubCircuitEquality
                                            | _ -> Ok ConditionalEquality
                                          in

                                          match condition_zero_phase_result with
                                          | Error result -> result
                                          | Ok condition_zero_phase -> (
                                              (* Debug display *)
                                              if debug then (
                                                printf
                                                  "Equiv.seq, output_state_reduced.phase = %s\n\n%!"
                                                  (PS.exact
                                                     output_state_reduced.phase);

                                                printf
                                                  "Equiv.seq, condition_zero_phase = %s\n\n%!"
                                                  (phase_equality_to_string
                                                     condition_zero_phase));

                                              (* Evaluate result according to the phase condition. *)
                                              match condition_zero_phase with
                                              | SubCircuitEquality -> (
                                                  match
                                                    compare_inputs_with_identity ~debug
                                                      inputs1
                                                      output_state_reduced
                                                      identity_state
                                                  with
                                                  | Error result -> result
                                                  | Ok true ->
                                                      SubCircuitEquivalent
                                                  | Ok false ->
                                                      SubCircuitInconclusive)
                                              | GlobalPhaseEquality -> (
                                                  match
                                                    compare_inputs_with_identity ~debug
                                                      inputs1
                                                      output_state_reduced
                                                      identity_state
                                                  with
                                                  | Error result -> result
                                                  | Ok true ->
                                                      GlobalPhaseEquivalent
                                                  | Ok false ->
                                                      SubCircuitInconclusive)
                                              | ConditionalEquality -> (
                                                  match equivalence with
                                                  | SubCircuit ->
                                                      SubCircuitInconclusive
                                                  | GlobalPhase ->
                                                      GlobalPhaseInconclusive
                                                  | FullCircuit ->
                                                      ErrorFullCircuitNotImplemented))))
                        else Entanglement1)

let parallel ?(debug = false) ?(inputs1 = []) ?(inputs2 = []) ?(outputs1 = [])
    ?(outputs2 = []) ?(meas1 = []) ?(meas2 = []) ?(equivalence = SubCircuit)
    unitary1 unitary2 =
  (* Temporary profile in the HH log: e.g. normalize_1 is measured separately
     from separation_1. Reduction calls already have their own timers. *)
  let profile_file = Sys.getenv_opt "SQBRICKS_PROFILE_HH_COST_FILE" in
  let profile_step stage operation =
    match profile_file with
    | None -> operation ()
    | Some file ->
        let channel =
          open_out_gen [ Open_wronly; Open_creat; Open_append; Open_text ]
            0o644 file
        in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            fprintf channel "EQUIV_STEP_BEGIN pid=%d stage=%s\n%!"
              (Unix.getpid ()) stage;
            let wall_start = Unix.gettimeofday () in
            let cpu_start = Sys.time () in
            let result = operation () in
            let cpu_s = Sys.time () -. cpu_start in
            let wall_s = Unix.gettimeofday () -. wall_start in
            fprintf channel
              "EQUIV_STEP_END pid=%d stage=%s wall_s=%.6f cpu_s=%.6f\n%!"
              (Unix.getpid ()) stage wall_s cpu_s;
            result)
  in
  if equivalence = FullCircuit then ErrorFullCircuitNotImplemented
  else
    match
      profile_step "parameters" (fun () ->
          parameters_preparation inputs1 inputs2 outputs1 outputs2 unitary1
            unitary2)
    with
  | Error result -> result
  | Ok
      ( wq1,
        wq2,
        inits1,
        inits2,
        _inputs1,
        _inputs2,
        outputs1,
        outputs2,
        _ ) ->
      if
        not (ListBis.valid_indices wq1 meas1 && ListBis.valid_indices wq2 meas2)
      then
        ErrorInvalidQubitIndex
      else if
        (* observable check *)
        not (check_observable_measurement outputs1 outputs2 meas1 meas2)
      then NotEquivDiffMeasurements
      else
        match
          profile_step "initialize_1" (fun () ->
              initialized_path_sum_for_equiv ~debug (Int.max 1 wq1) inits1)
        with
        | Error result -> result
        | Ok input_state1 -> (
            match
              profile_step "initialize_2" (fun () ->
                  initialized_path_sum_for_equiv ~debug (Int.max 1 wq2) inits2)
            with
            | Error result -> result
            | Ok input_state2 ->

                match
                  profile_step "execution_1" (fun () ->
                      execution_for_equiv ~debug ~input_state:input_state1
                        unitary1)
                with
                | Error result -> result
                | Ok output_state1 -> (
                    match
                      profile_step "execution_2" (fun () ->
                          execution_for_equiv ~debug ~input_state:input_state2
                            unitary2)
                    with
                    | Error result -> result
                    | Ok output_state2 -> (
                        match reduction_for_equiv ~debug output_state1 with
                        | Error result -> result
                        | Ok output_state_reduced1 -> (
                            match reduction_for_equiv ~debug output_state2 with
                            | Error result -> result
                            | Ok output_state_reduced2 ->
                match
                  profile_step "normalize_1" (fun () ->
                      Rules.Variable_replacement.poly_normalized
                        ~debug output_state_reduced1)
                with
                | Error (Rules.MalformedPathSum _) -> ErrorMalformedPathSum
                | Ok output_path_var_norm1 -> (
                    match
                      profile_step "normalize_2" (fun () ->
                          Rules.Variable_replacement.poly_normalized
                            ~debug output_state_reduced2)
                    with
                    | Error (Rules.MalformedPathSum _) -> ErrorMalformedPathSum
                    | Ok output_path_var_norm2 ->

                        if debug then
                          printf
                            "Equiv.parallel,\noutput_path_var_norm1 =\n%s\n\n"
                            (PSS.pretty output_path_var_norm1);
                        if debug then
                          printf
                            "Equiv.parallel,\noutput_path_var_norm2 =\n%s\n\n"
                            (PSS.pretty output_path_var_norm2);

                        let check_separability () =
                          match
                            profile_step "separation_1" (fun () ->
                                separability_states output_path_var_norm1
                                  outputs1 wq1)
                          with
                          | Error result -> Error result
                          | Ok false -> Ok (Some Entanglement1)
                          | Ok true -> (
                              match
                                profile_step "separation_2" (fun () ->
                                    separability_states output_path_var_norm2
                                      outputs2 wq2)
                              with
                              | Error result -> Error result
                              | Ok false -> Ok (Some Entanglement2)
                              | Ok true -> Ok None)
                        in

                        match equivalence with
                        | SubCircuit -> (
                            match check_separability () with
                            | Error result -> result
                            | Ok (Some res) ->
                                (* Entanglement of out and disc*)
                                res
                            | Ok None ->
                                (* Entanglement of out and disc*)
                                (match
                                   profile_step "comparison" (fun () ->
                                       path_sum_equal_for_equiv ~debug ~outputs1
                                         ~outputs2 output_path_var_norm1
                                         output_path_var_norm2)
                                 with
                                | Error result -> result
                                | Ok true -> SubCircuitEquivalent
                                | Ok false -> SubCircuitInconclusive))
                        | GlobalPhase -> (
                            match check_separability () with
                            | Error result -> result
                            | Ok (Some res) ->
                                (* Entanglement of out and disc*)
                                res
                            | Ok None ->
                                (match
                                   profile_step "comparison" (fun () ->
                                       path_sum_equal_for_equiv ~debug ~outputs1
                                         ~outputs2 ~global_phase:true
                                         output_path_var_norm1
                                         output_path_var_norm2)
                                 with
                                | Error result -> result
                                | Ok true -> GlobalPhaseEquivalent
                                | Ok false -> GlobalPhaseInconclusive))
                        | FullCircuit -> ErrorFullCircuitNotImplemented)))))

(* Defines the type 'algo' representing the algorithm type to use. *)
type algo = Parallel | Sequence

(* Function SQV (Sequence or Parallel Verification) to verify the partial-unitary-equivalence of two quantum circuits *)
let sqv ?(debug = false) ?(inputs1 = []) ?(inputs2 = []) ?(outputs1 = [])
    ?(outputs2 = []) ?(meas1 = []) ?(meas2 = []) ?(algo = Sequence)
    ?(equivalence = SubCircuit) p1 p2 : result =
  (* Determines the equivalence result based on the algorithm and the requested equivalence type *)
  let result =
    match (algo, equivalence) with
    | _, FullCircuit -> ErrorFullCircuitNotImplemented
    | Parallel, _ ->
        parallel p1 p2 ~debug ~equivalence ~inputs1 ~inputs2 ~outputs1 ~outputs2
          ~meas1 ~meas2
    | Sequence, _ ->
        seq p1 p2 ~debug ~equivalence ~inputs1 ~inputs2 ~outputs1 ~outputs2
          ~meas1 ~meas2
  in
  result

let sqv_simple ?(debug = false) ?(inputs1 = []) ?(inputs2 = [])
    ?(outputs1 = []) ?(outputs2 = []) ?(meas1 = []) ?(meas2 = [])
    ?(algo = Sequence) ?(equivalence = SubCircuit) ?(not_equiv = false) p1 p2 =
  let is_equivalent =
    let result =
      sqv ~debug ~inputs1 ~inputs2 ~outputs1 ~outputs2 ~meas1 ~meas2 ~algo
        ~equivalence p1 p2
    in
    match (equivalence, result) with
    | SubCircuit, SubCircuitEquivalent -> true
    | GlobalPhase, (SubCircuitEquivalent | GlobalPhaseEquivalent) -> true
    | FullCircuit, FullCircuitEquivalent -> true
    | _ -> false
  in
  if not_equiv then not is_equivalent else is_equivalent
