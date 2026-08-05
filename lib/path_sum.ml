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

open Printf
open Common
include Rational
open Poly.Monome
module PS = Poly.String
open Qubit
module QS = Qubit.String

module Ket = struct
  type t = Qubit.t array
  type equality_error = DifferentOutputLengths | InvalidOutputIndex

  let copy input = Array.copy input

  (* Keep unchanged kets physically shared; allocate one copy only when at least
     one qubit really changes. *)
  let map_qubits_copy_on_change transform_qubit ket =
    let rec map_qubits changed_ket qubit_index =
      if qubit_index = Array.length ket then
        match changed_ket with None -> ket | Some changed_ket -> changed_ket
      else
        let original_qubit = ket.(qubit_index) in
        let transformed_qubit = transform_qubit original_qubit in
        if Qubit.equal original_qubit transformed_qubit then
          map_qubits changed_ket (qubit_index + 1)
        else
          let changed_ket =
            match changed_ket with
            | Some changed_ket -> changed_ket
            | None -> Array.copy ket
          in
          changed_ket.(qubit_index) <- transformed_qubit;
          map_qubits (Some changed_ket) (qubit_index + 1)
    in
    map_qubits None 0

  let equal_result ?(debug = false) ?(outputs1 = []) ?(outputs2 = []) (k1 : t)
      (k2 : t) : (bool * int IntMap.t * int IntMap.t, equality_error) result =
    if debug then
      printf "Ket.equal, outputs1 = %s\n\n%!" (ListBis.string_int outputs1);
    if debug then
      printf "Ket.equal, outputs2 = %s\n\n%!" (ListBis.string_int outputs2);

    let map_path_var1 = IntMap.empty in
    let map_path_var2 = IntMap.empty in
    if List.length outputs1 <> List.length outputs2 then
      Error DifferentOutputLengths
    else
      let wq1 = Array.length k1 in
      let wq2 = Array.length k2 in
      if
        not (ListBis.valid_indices wq1 outputs1)
        || not (ListBis.valid_indices wq2 outputs2)
      then Error InvalidOutputIndex
      else
        (* A path-variable pair creates a direct mapping and its inverse. Later
           occurrences must preserve both directions, which makes the
           renaming bijective. *)
        let compare_path_variables m1 m2 variable1 variable2 =
          match
            (IntMap.find_opt variable1 m1, IntMap.find_opt variable2 m2)
          with
          | Some expected2, Some expected1 ->
              ( Int.equal expected2 variable2 && Int.equal expected1 variable1,
                m1,
                m2 )
          | None, None ->
              ( true,
                IntMap.add variable1 variable2 m1,
                IntMap.add variable2 variable1 m2 )
          | Some _, None | None, Some _ -> (false, m1, m2)
        in

        let rec compare_qubits m1 m2 q1 q2 =
          match (q1, q2) with
          | Zero, Zero | One, One -> (true, m1, m2)
          | Var variable1, Var variable2 ->
              if variable1 < wq1 && variable2 < wq2 then
                (Int.equal variable1 variable2, m1, m2)
              else if wq1 <= variable1 && wq2 <= variable2 then
                compare_path_variables m1 m2 variable1 variable2
              else (false, m1, m2)
          | Prod (left1, right1), Prod (left2, right2)
          | SumMod2 (left1, right1), SumMod2 (left2, right2) ->
              let left_equal, m1, m2 =
                compare_qubits m1 m2 left1 left2
              in
              if left_equal then compare_qubits m1 m2 right1 right2
              else (false, m1, m2)
          | _ -> (false, m1, m2)
        in

        let rec aux m1 m2 = function
          | hd1 :: tl1, hd2 :: tl2 ->
              if debug then printf "Ket.equal, hd1 = %d, hd2 = %d\n%!" hd1 hd2;
              if debug then
                printf "Ket.equal, k1.(hd1) = %s, k2.(hd2) = %s\n\n%!"
                  (QS.pretty k1.(hd1) wq1)
                  (QS.pretty k2.(hd2) wq2);

              let equal, new_m1, new_m2 =
                compare_qubits m1 m2 k1.(hd1) k2.(hd2)
              in
              if debug then printf "Ket.equal, qubits_equal = %b\n\n%!" equal;
              if equal then aux new_m1 new_m2 (tl1, tl2)
              else (false, new_m1, new_m2)
          | [], [] -> (true, m1, m2)
          | _ -> (false, m1, m2)
        in
        (* Without explicit outputs, compare every component in order. *)
        let compared_outputs =
          if List.is_empty outputs1 then
            if Int.equal wq1 wq2 then
              Some (ListBis.range 0 wq1, ListBis.range 0 wq2)
            else None
          else Some (outputs1, outputs2)
        in
        let result, out_m1, out_m2 =
          match compared_outputs with
          | None -> (false, map_path_var1, map_path_var2)
          | Some output_pairs ->
              aux map_path_var1 map_path_var2 output_pairs
        in
        if debug then printf "Ket.equal, result = %b\n\n%!" result;
        if debug then
          printf "Ket.equal.IntMap, out_m1 = %s, out_m2 = %s\n%!"
            (Common.to_string_int_map out_m1)
            (Common.to_string_int_map out_m2);
        Ok (result, out_m1, out_m2)

  let equal ?(debug = false) ?(outputs1 = []) ?(outputs2 = []) (k1 : t) (k2 : t)
      : bool * int IntMap.t * int IntMap.t =
    let empty_map = IntMap.empty in
    match equal_result ~debug ~outputs1 ~outputs2 k1 k2 with
    | Ok result -> result
    | Error DifferentOutputLengths | Error InvalidOutputIndex ->
        (false, empty_map, empty_map)

  (* if \( v \in k \) then `true` else `false` *)
  (* Don't look in `k.(except)`. *)
  let member ?(except = -1) v k =
    let width = Array.length k in
    let rec aux i =
      if Int.equal i width then false
      else if Int.equal i except then aux (i + 1)
      else if Qubit.member v k.(i) then true
      else aux (i + 1)
    in
    aux 0

  let simplify k =
    let n = Array.length k in
    let result = Array.make n Qubit.Zero in
    for i = 0 to n - 1 do
      result.(i) <- Qubit.simplify k.(i)
    done;
    result

  let extract_path_var ?(debug = false) ?(outputs = []) ket =
    let width = Array.length ket in
    let l = if outputs = [] then ListBis.range 0 width else outputs in
    if debug then
      printf "Path_sum.Ket.extract_path_var, l = %s\n\n%!"
        (ListBis.string_int l);
    let rec aux acc = function
      | hd :: tl ->
          if debug then
            printf "Path_sum.Ket.extract_path_var, hd = %d, tl = %s\n%!" hd
              (ListBis.string_int tl);
          if debug then
            printf "Path_sum.Ket.extract_path_var, ket.(hd) = %s\n%!"
              (QS.pretty ket.(hd) width);
          let qubit_extract_path_var =
            Qubit.extract_path_var ket.(hd) (width - 1)
          in
          if debug then
            printf
              "Path_sum.Ket.extract_path_var, qubit_extract_path_var = %s\n\n%!"
              (ListBis.string_int qubit_extract_path_var);
          aux (List.sort_uniq Int.compare (qubit_extract_path_var @ acc)) tl
      | [] -> acc
    in
    aux [] l

  let extract_var ket l =
    let rec aux = function
      | hd :: tl -> Qubit.extract_var ket.(hd) @ aux tl
      | [] -> []
    in
    aux l

  module String = struct
    module QS = Qubit.String

    let pretty (k : t) : string =
      let s = ref "" in
      let n = Array.length k in
      s := !s ^ " |";
      for i = 0 to n - 1 do
        if Int.equal i (n - 1) then s := !s ^ QS.pretty k.(i) n
        else s := !s ^ QS.pretty k.(i) n ^ ","
      done;
      !s ^ ">"

    let exact (k : t) =
      let w = Array.length k in
      let s = ref "" in
      s := !s ^ "[|";
      for i = 0 to w - 1 do
        if Int.equal i (w - 1) then s := !s ^ QS.exact k.(i)
        else s := !s ^ QS.exact k.(i) ^ ";"
      done;
      !s ^ "|]"
  end

  let substitute_variables_in_qubit ?(debug = false) qubit_substitutions =
    let substitution_map =
      List.fold_left
        (fun substitution_map (variable_to_replace, replacement_qubit) ->
          IntMap.add variable_to_replace replacement_qubit substitution_map)
        IntMap.empty qubit_substitutions
    in
    let rec substitute_in_qubit (qubit : Qubit.t) : Qubit.t =
      if debug then
        printf "Path_sum.Ket.substitute_in_qubit, qubit = %s\n"
          (QS.exact qubit);
      match qubit with
      | Qubit.SumMod2 (left_qubit, right_qubit) ->
          Qubit.SumMod2
            (substitute_in_qubit left_qubit, substitute_in_qubit right_qubit)
      | Qubit.Prod (left_qubit, right_qubit) ->
          Qubit.Prod
            (substitute_in_qubit left_qubit, substitute_in_qubit right_qubit)
      | Qubit.Var variable_to_replace -> (
          match IntMap.find_opt variable_to_replace substitution_map with
          | Some replacement_qubit -> replacement_qubit
          | None -> qubit)
      | _ -> qubit
    in
    fun qubit -> Qubit.simplify (substitute_in_qubit qubit)

  (* ket[variable_indice <- qubit_to_substitute] *)
  let substitute ?(debug = false) (ket : t) (variable_to_replace : int)
      replacement_qubit =
    if debug then
      printf "Path_sum.Ket.substitute, ket = %s\n" (String.pretty ket);
    let substituted_ket =
      map_qubits_copy_on_change
        (substitute_variables_in_qubit ~debug
           [ (variable_to_replace, replacement_qubit) ])
        ket
    in
    if debug then
      printf "Path_sum.Ket.substitute.end, substituted_ket = %s\n\n"
        (String.pretty substituted_ket);
    substituted_ket

  let substitute_many ?(debug = false) ket qubit_substitutions =
    if debug then
      printf "Path_sum.Ket.substitute_many, ket = %s\n" (String.pretty ket);
    let substituted_ket =
      if List.is_empty qubit_substitutions then ket
      else
        map_qubits_copy_on_change
          (substitute_variables_in_qubit ~debug qubit_substitutions)
          ket
    in
    if debug then
      printf "Path_sum.Ket.substitute_many.end, substituted_ket = %s\n\n"
        (String.pretty substituted_ket);
    substituted_ket

  (* 
Need to have an input "Renamed" 
Return (tmp_array_pvs * final_array_pvs) 
Compute the ordering of path variables in a given ket.
*)
  type path_var_order_error = InvalidPathVariableCount | InvalidPathVariableIndex

  let path_var_order_result ?(debug = false) (ket : Qubit.t array)
      (nb_pvs : int) : (int array * int array, path_var_order_error) result =
    if nb_pvs < 0 then Error InvalidPathVariableCount
    else (
      let wq = Array.length ket in
      (* Initialization *)
      let pvs = Array.make nb_pvs Int.min_int in
      let tmp_pvs = Array.make nb_pvs Int.max_int in

      if debug then (
        printf "Path_sum.path_var_order, nb_pvs = %d\n%!" nb_pvs;
        printf "Path_sum.path_var_order, path_vars = %s\n%!"
          (ArrayBis.string_int pvs);
        printf "Path_sum.path_var_order, tmp_path_vars = %s\n%!"
          (ArrayBis.string_int tmp_pvs));

      (* Update the path-variable order given a list of variable indices. *)
      let rec update_pvs pv_curr tmp_pvs pvs = function
        | [] -> Ok (pv_curr, tmp_pvs, pvs)
        | hd :: tl ->
            let i = hd - wq in
            if i < 0 || i >= nb_pvs then Error InvalidPathVariableIndex
            else if pvs.(i) = Int.min_int && tmp_pvs.(i) = Int.max_int then (
              pvs.(i) <- pv_curr;
              tmp_pvs.(i) <- -pv_curr;
              update_pvs (pv_curr + 1) tmp_pvs pvs tl)
            else update_pvs pv_curr tmp_pvs pvs tl
      in

      (* Traverse all qubits and accumulate variable ordering. *)
      let rec traverse pv_curr i tmp_pvs pvs =
        if i >= wq then Ok (tmp_pvs, pvs)
        else
          let path_vars = Qubit.extract_path_var ket.(i) (wq - 1) in
          match update_pvs pv_curr tmp_pvs pvs path_vars with
          | Error error -> Error error
          | Ok (pv_curr, tmp_pvs, pvs) ->
              traverse pv_curr (i + 1) tmp_pvs pvs
      in

      match traverse wq 0 tmp_pvs pvs with
      | Error error -> Error error
      | Ok (tmp_pvs, pvs) ->
          if debug then
            printf "Path_sum.path_var_order, tmp_pvs = %s, pvs = %s\n\n%!"
              (ArrayBis.string_int tmp_pvs)
              (ArrayBis.string_int pvs);

          Ok (tmp_pvs, pvs))

  let path_var_order ?(debug = false) (ket : Qubit.t array) (nb_pvs : int) :
      int array * int array =
    match path_var_order_result ~debug ket nb_pvs with
    | Ok order -> order
    | Error InvalidPathVariableCount ->
        invalid_arg "Path_sum.Ket.path_var_order: negative path-variable count"
    | Error InvalidPathVariableIndex ->
        invalid_arg "Path_sum.Ket.path_var_order: path-variable index overflow"

  let list_of_qubits_to_ket list =
    let ket = Array.make (List.length list) Qubit.Zero in
    let _ =
      List.fold_left
        (fun i q ->
          ket.(i) <- q;
          i + 1)
        0 list
    in
    ket

  let number_of_sum k =
    let w = Array.length k in
    let rec aux nb i =
      if i < w then aux (nb + Qubit.number_of_sum k.(i)) (i + 1) else nb
    in
    aux 0 0
end

module KS = Ket.String

type t = { phase : Poly.t; ket : Ket.t; path_var : int list }

let copy input : t =
  let phase = input.phase in
  let ket = Ket.copy input.ket in
  let path_var = input.path_var in
  { phase; ket; path_var }

module String = struct
  let exact (ps : t) =
    sprintf "phase = %s;" (PS.exact ps.phase)
    ^ sprintf "ket = %s;" (KS.exact ps.ket)
    ^ sprintf "path_var = %s;" (ListBis.string_int ps.path_var)

  let pretty (ps : t) =
    let n = Array.length ps.ket in
    sprintf "  phase = e^{2.π.i.%s};\n" (PS.pretty ps.phase n)
    ^ sprintf "  ket = %s;\n" (KS.pretty ps.ket)
    ^ sprintf "  path_var = %s;" (ListBis.y_string_int ps.path_var n)

  let path_var pvs w = ListBis.string_int (List.map (Int.add (Int.neg w)) pvs)
end

type equality_error =
  | DifferentOutputLengths
  | InvalidOutputIndex
  | IncompatiblePhaseWidths
  | IncompletePhasePathVariableMap

let equality_error_of_ket = function
  | Ket.DifferentOutputLengths -> DifferentOutputLengths
  | Ket.InvalidOutputIndex -> InvalidOutputIndex

let equality_error_of_poly = function
  | Poly.IncompatibleWidths -> IncompatiblePhaseWidths
  | Poly.IncompletePathVariableMap -> IncompletePhasePathVariableMap

let equal_result ?(debug = false) ?(outputs1 = []) ?(outputs2 = [])
    ?(global_phase = false) ps1 ps2 =
  let wq1, wq2 = (Array.length ps1.ket, Array.length ps2.ket) in
  let width_outputs1 = List.length outputs1 in
  let width_outputs2 = List.length outputs2 in
  if width_outputs1 <> width_outputs2 then Error DifferentOutputLengths
  else if width_outputs1 = 0 && not (Int.equal wq1 wq2) then
    (* Without explicit outputs, equality compares the complete kets. *)
    Ok false
  else
    let p1 = ps1.phase in
    let p2 = ps2.phase in

    if debug then printf "Path_sum.equal, ps1 =\n%s\n%!" (String.pretty ps1);
    if debug then printf "Path_sum.equal, ps2 =\n%s\n\n%!" (String.pretty ps2);

    let outputs1, outputs2 =
      let tmp = if width_outputs1 = 0 then ListBis.range 0 wq1 else [] in
      if debug then
        printf "Path_sum.equal, tmp = %s\n\n%!" (ListBis.string_int tmp);
      if List.is_empty tmp then (outputs1, outputs2) else (tmp, tmp)
    in

    if debug then
      printf "Path_sum.equal, outputs1 = %s\n\n%!" (ListBis.string_int outputs1);
    if debug then
      printf "Path_sum.equal, outputs2 = %s\n\n%!" (ListBis.string_int outputs2);

    match Ket.equal_result ~debug ~outputs1 ~outputs2 ps1.ket ps2.ket with
    | Error error -> Error (equality_error_of_ket error)
    | Ok (kets_equal, map_path_var1, map_path_var2) ->
        if debug then
          printf
            "Path_sum.equal.IntMap, map_path_var1 = %s, map_path_var2 = %s\n%!"
            (Common.to_string_int_map map_path_var1)
            (Common.to_string_int_map map_path_var2);

        if debug then printf "Path_sum.equal, kets_equals = %b\n" kets_equal;

        if kets_equal then (
          let var_outputs1 =
            List.sort_uniq Int.compare (Ket.extract_var ps1.ket outputs1)
          in
          let var_outputs2 =
            List.sort_uniq Int.compare (Ket.extract_var ps2.ket outputs2)
          in

          if debug then printf "Path_sum.equal, p1 = %s\n%!" (PS.pretty p1 wq1);
          if debug then printf "Path_sum.equal, p2 = %s\n%!" (PS.pretty p2 wq2);

          let extract_poly p var_outputs wq =
            if Poly.is_constant_superior_zero p then p
            else
              let m = Poly.find p in
              let p =
                Poly.extract p
                  (List.sort_uniq Int.compare
                     (var_outputs @ ListBis.range 0 wq))
              in
              match m with
              | Poly.Monome.Scal x when not (Q.equal x Q.zero) -> Poly.insert m p
              | _ -> p
          in

          let poly_output1 = extract_poly p1 var_outputs1 wq1 in
          let poly_output2 = extract_poly p2 var_outputs2 wq2 in

          (* Sub-Circuit-Partial-Equivalence *)
          match
            Poly.equal_result ~global_phase ~debug ~wq1 ~wq2 ~map_path_var1
              ~map_path_var2 poly_output1 poly_output2
          with
          | Error Poly.IncompletePathVariableMap ->
              (* Ket equality builds a consistent partial bijection. A
                 one-sided phase lookup therefore means that the phases differ,
                 not that either path sum is malformed. *)
              Ok false
          | Error error -> Error (equality_error_of_poly error)
          | Ok polys_equal ->
              if debug then
                printf "Path_sum.equal, polys_equal = %b\n" polys_equal;
              Ok polys_equal)
        else Ok false

let equal ?(debug = false) ?(outputs1 = []) ?(outputs2 = [])
    ?(global_phase = false) ps1 ps2 =
  match equal_result ~debug ~outputs1 ~outputs2 ~global_phase ps1 ps2 with
  | Ok are_equal -> are_equal
  | Error DifferentOutputLengths
  | Error InvalidOutputIndex
  | Error IncompatiblePhaseWidths
  | Error IncompletePhasePathVariableMap ->
      false

let zero = Poly.zero

let ofSize w : t =
  let k = Array.make w (Qubit.Var 0) in
  for i = 0 to w - 1 do
    k.(i) <- Qubit.Var i
  done;
  { phase = zero; ket = k; path_var = [] }

type initialization_error = InvalidWidth | InvalidInitIndex

let ofSize_init_result ?(debug = false) width inits_0 =
  if width < 0 then Error InvalidWidth
  else if not (ListBis.valid_indices width inits_0) then Error InvalidInitIndex
  else
    let set_zero_inits ket =
      List.iter
        (fun target ->
          if debug then
            printf "Path_sum.ofSize_init_result, zero init target = %d\n\n"
              target;
          ket.(target) <- Qubit.Zero)
        inits_0
    in
    let set_symbolic_inputs ket inputs =
      List.iteri
        (fun input_value target ->
          if debug then
            printf
              "Path_sum.ofSize_init_result, symbolic input target = %d\n\n"
              target;
          ket.(target) <- Var input_value)
        inputs
    in

  if debug then
    printf "Path_sum.ofSize_init, width = %d, inits_0 = %s\n\n" width
      (ListBis.string_int inits_0);
  let ps = ofSize width in
  if debug then printf "Path_sum.ofSize_init, ps =\n%s\n\n" (String.pretty ps);
  let ket = ps.ket in

  let inputs = ListBis.missing_in_range inits_0 width in

  (* Initialization by 0 *)
  set_zero_inits ket;
  (* Normalization of inputs variables *)
  set_symbolic_inputs ket inputs;

  if debug then printf "Path_sum.ofSize_init, ket = %s\n\n" (KS.pretty ket);

  let output = { phase = ps.phase; ket; path_var = ps.path_var } in

  if debug then
    printf "Path_sum.ofSize_init, output =\n%s\n\n" (String.pretty output);

  Ok output

let ofSize_init ?(debug = false) width inits_0 =
  match ofSize_init_result ~debug width inits_0 with
  | Ok path_sum -> path_sum
  | Error InvalidWidth -> invalid_arg "Path_sum.ofSize_init: invalid width"
  | Error InvalidInitIndex ->
      invalid_arg "Path_sum.ofSize_init: invalid initialization index"

let remove_path_var ps y =
  { phase = ps.phase; ket = ps.ket; path_var = ListBis.remove y ps.path_var }

type substitution_error = CannotSubstitutePathVariable

let substitute_result ?(debug = false) ?(except_path_var = false) path_sum
    indice_to_subst qubit_to_subst =
  if ListBis.member indice_to_subst path_sum.path_var Int.equal then
    if except_path_var then Ok path_sum else Error CannotSubstitutePathVariable
  else
    let new_phase =
      Poly.substitute ~debug indice_to_subst path_sum.phase qubit_to_subst
    in
    let new_ket =
      Ket.substitute ~debug path_sum.ket indice_to_subst qubit_to_subst
    in

    let output : t =
      { phase = new_phase; ket = new_ket; path_var = path_sum.path_var }
    in
    if debug then
      printf "Path_sum.substitute, output =\n%s\n\n" (String.pretty output);
    Ok output

let substitute ?(debug = false) ?(except_path_var = false) path_sum
    indice_to_subst qubit_to_subst =
  match
    substitute_result ~debug ~except_path_var path_sum indice_to_subst
      qubit_to_subst
  with
  | Ok path_sum -> path_sum
  | Error CannotSubstitutePathVariable ->
      invalid_arg "Path_sum.substitute: cannot substitute a path variable"

module Path_sum_library = struct
  (*
In order to keep phase polynomials with positive coefficients,
we use the following transformation:
e^{-2πi(s/2^k)} = e^{2πi((2^k - s)/2^k)}
*)

  let ( ++ ) = Poly.( ++ )
  let empty = Poly.empty

  type gate_error = TargetIndexOutOfWidth | OverlappingGateWires

  let target_is_valid target width = 0 <= target && target < width

  let xx ta w : (Qubit.t, gate_error) result =
    if target_is_valid ta w then Ok (Var ta) else Error TargetIndexOutOfWidth

  let invalid_target_failure ta w =
    failwith
      (sprintf "Path_sum.Library.xx, target %d outside width %d\n" ta w)

  (* Keep the old left-to-right validation order for wrapper failures. *)
  let rec invalid_targets_failure targets w =
    match targets with
    | target :: _ when not (target_is_valid target w) ->
        invalid_target_failure target w
    | _ :: remaining_targets -> invalid_targets_failure remaining_targets w
    | [] -> invalid_target_failure w w

  let overlapping_gate_wires_failure targets =
    failwith
      (sprintf "Path_sum.Library, gate wires must be distinct: %s\n"
         (ListBis.string_int targets))

  let gate_failure targets width = function
    | TargetIndexOutOfWidth -> invalid_targets_failure targets width
    | OverlappingGateWires -> overlapping_gate_wires_failure targets

  let targets2 target1 target2 w =
    match xx target1 w with
    | Error error -> Error error
    | Ok qubit1 -> (
        match xx target2 w with
        | Error error -> Error error
        | Ok qubit2 ->
            if target1 = target2 then Error OverlappingGateWires
            else Ok (qubit1, qubit2))

  let targets3 target1 target2 target3 w =
    (* Validate all indices before checking overlap, so an invalid index keeps
       the same priority as before overlap errors were introduced. *)
    match xx target1 w with
    | Error error -> Error error
    | Ok qubit1 -> (
        match xx target2 w with
        | Error error -> Error error
        | Ok qubit2 -> (
            match xx target3 w with
            | Error error -> Error error
            | Ok qubit3 ->
                if
                  target1 = target2 || target1 = target3 || target2 = target3
                then Error OverlappingGateWires
                else Ok (qubit1, qubit2, qubit3)))

  let yy n w : Qubit.t = Var (n + w)

  (* Build the complete output ket: untouched wires keep their input variable,
     while the selected target receives the gate's transformed expression. *)
  let ket_with_target target_index width transformed_target =
    Array.init width (fun index ->
        if index = target_index then transformed_target else Var index)

  (* \( 1 / \sqrt{2} \sum_{y_0 \in \{0,1\}} e^{2 \pi i (x_0 y_0) / 2} \ket{y_0} \) *)
  let h_result ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        Ok
          {
            phase =
              Prod (Scal div2, Prod (Qubit target, Qubit (yy 0 w))) ++ empty;
            ket = ket_with_target ta w (yy 0 w);
            path_var = [ w ];
          }

  let h ta w =
    match h_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let x_result ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        Ok
          {
            phase = Scal Q.zero ++ empty;
            ket = ket_with_target ta w (SumMod2 (One, target));
            path_var = [];
          }

  let x ta w =
    match x_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let apply_angle sQ k = if sQ < Q.zero then Q.add (pow2Q k) sQ else sQ

  (* Since s is an integer, k < 0 makes each rotation below an exact identity.
     Handle it before the dyadic arithmetic, which requires non-negative shifts. *)

  (* \( u1 s k  : |x0> -> e^{2.pi.i. x0.s / 2^k} \) |x0> *)
  let u1_result ?(s = 1) k ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        let p =
          if s = 0 || k <= 0 then Scal Q.zero ++ empty
          else
            Prod
              ( Scal (Q.div_2exp (apply_angle (Q.of_int s) k) k),
                Qubit target )
            ++ empty
        in
        Ok { phase = p; ket = ket_with_target ta w target; path_var = [] }

  let u1 ?(s = 1) k ta w =
    match u1_result ~s k ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let z_result ta w = u1_result 1 ta w

  let z ta w =
    match z_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let s_result ta w = u1_result 2 ta w

  let s ta w =
    match s_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let t_result ta w = u1_result 3 ta w

  let t ta w =
    match t_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let zinv_result ta w = u1_result ~s:(-1) 1 ta w

  let zinv ta w =
    match zinv_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let sinv_result ta w = u1_result ~s:(-1) 2 ta w

  let sinv ta w =
    match sinv_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  let tinv_result ta w = u1_result ~s:(-1) 3 ta w

  let tinv ta w =
    match tinv_result ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  (* \( rz s k  : |x0> -> e^{2.pi.i. (x0.s/2^k - s/2^{k+1})} |x0> \) *)
  let rz_result ?(s = 1) k ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        let sQ = Q.of_int s in
        let is_identity = s = 0 || k < 0 in
        let p =
          if is_identity then Scal Q.zero ++ empty
          else
            Scal (Q.div_2exp (apply_angle (Q.neg sQ) (k + 1)) (k + 1))
            ++ (Prod (Scal (Q.div_2exp (apply_angle sQ k) k), Qubit target)
               ++ empty)
        in
        Ok { phase = p; ket = ket_with_target ta w target; path_var = [] }

  let rz ?(s = 1) k ta w =
    match rz_result ~s k ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  (* \( rx s k : |x0> -> e^{2.pi.i. (x0.y0/2 + s.y0/2^k - s/2^{k+1} + y0.y1/2)} |y1> \) *)
  let rx_result ?(s = 1) k ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        let sQ = Q.of_int s in
        let is_identity = s = 0 || k < 0 in
        let p =
          if is_identity then Scal Q.zero ++ empty
          else if Q.zero < sQ then
            let pow2kp1 = pow2Q (k + 1) in
            Prod (Scal div2, Prod (Qubit target, Qubit (yy 0 w)))
            ++ (Prod (Scal (Q.div_2exp sQ k), Qubit (yy 0 w))
               ++ (Scal (Q.div (Q.sub pow2kp1 sQ) pow2kp1)
                  ++ (Prod (Scal div2, Prod (Qubit (yy 0 w), Qubit (yy 1 w)))
                     ++ empty)))
          else
            let pow2k = pow2Q k in
            Prod (Scal div2, Prod (Qubit target, Qubit (yy 0 w)))
            ++ (Prod
                  ( Scal (Q.div (Q.sub pow2k (Q.neg sQ)) pow2k),
                    Qubit (yy 0 w) )
               ++ (Scal (Q.div_2exp (Q.neg sQ) (k + 1))
                  ++ (Prod (Scal div2, Prod (Qubit (yy 0 w), Qubit (yy 1 w)))
                     ++ empty)))
        in

        let q = if is_identity || k = 0 then target else yy 1 w in
        let pv = if is_identity then [] else [ w; w + 1 ] in
        Ok { phase = p; ket = ket_with_target ta w q; path_var = pv }

  let rx ?(s = 1) k ta w =
    match rx_result ~s k ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  (* \( ry s k : |x0> -> e^{2.pi.i.
     (-x0/4 + y0/2 - x0.y0/2 + s.y0/2^k - s/2^{k+1} + y0.y1/2 + y1/2)}
     |y1> \) *)
  let ry_result ?(s = 1) k ta w =
    match xx ta w with
    | Error error -> Error error
    | Ok target ->
        let sQ = Q.of_int s in
        let is_identity = s = 0 || k < 0 in
        let p =
          if is_identity then Scal Q.zero ++ empty
          else if Q.zero < sQ then
            let pow2kp1 = pow2Q (k + 1) in
            Scal (7 /// 4)
            ++ (Prod (Scal div4, Qubit target)
               ++ (Prod (Scal div2, Qubit (yy 0 w))
                  ++ (Prod (Scal (3 /// 2), Prod (Qubit target, Qubit (yy 0 w)))
                     ++ (Prod (Scal (Q.div_2exp sQ k), Qubit (yy 0 w))
                        ++ (Scal (Q.div (Q.sub pow2kp1 sQ) pow2kp1)
                           ++ (Prod
                                 ( Scal div2,
                                   Prod (Qubit (yy 0 w), Qubit (yy 1 w)) )
                              ++ (Prod (Scal div4, Qubit (yy 1 w)) ++ empty)))))))
          else
            let pow2k = pow2Q k in
            Scal (7 /// 4)
            ++ (Prod (Scal div4, Qubit target)
               ++ (Prod (Scal div2, Qubit (yy 0 w))
                  ++ (Prod (Scal (3 /// 2), Prod (Qubit target, Qubit (yy 0 w)))
                     ++ (Prod
                           ( Scal (Q.div (Q.sub pow2k (Q.neg sQ)) pow2k),
                             Qubit (yy 0 w) )
                        ++ (Scal (Q.div_2exp (Q.neg sQ) (k + 1))
                           ++ (Prod
                                 ( Scal div2,
                                   Prod (Qubit (yy 0 w), Qubit (yy 1 w)) )
                              ++ (Prod (Scal div4, Qubit (yy 1 w)) ++ empty)))))))
        in

        let q =
          if is_identity || k = 0 then target else SumMod2 (One, yy 1 w)
        in
        let pv = if is_identity then [] else [ w; w + 1 ] in
        Ok { phase = p; ket = ket_with_target ta w q; path_var = pv }

  let ry ?(s = 1) k ta w =
    match ry_result ~s k ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ ta ] w error

  (* \( (1 ++ x_0) (1 - 2 y_0) / 8 =
        (1 - x_0) (1 - 2 y_0) / 8 =
        ( 1     -   x_0     + 2 x_0 y_0     - 2 y_0) / 8 =
          1 / 8 -   x_0 / 8 +   x_0 y_0 / 4 -   y_0 / 4  =
          1 / 8 + 7 x_0 / 8 +   x_0 y_0 / 4 + 3 y_0 / 4\) *)
  let normalisation_factor co w : (Poly.t, gate_error) result =
    match xx co w with
    | Error error -> Error error
    | Ok control ->
        Ok
          (Scal div8
          ++ (Prod (Scal divm8, Qubit control)
             ++ (Prod (Scal div4, Prod (Qubit control, Qubit (yy 0 w)))
                ++ (Prod (Scal divm4, Qubit (yy 0 w)) ++ empty))))

  (* \( x_0 y_0 ++ (1 ++ x_0) x_1 = x_0 x_1 ++ x_0 y_0 ++ x_1} \) *)
  let q2 co ta w : (Qubit.t, gate_error) result =
    match targets2 co ta w with
    | Error error -> Error error
    | Ok (control, target) ->
        Ok
          (SumMod2
             ( Prod (control, target),
               SumMod2 (Prod (control, yy 0 w), target) ))

  (* \( 1 / \sqrt{2}
     \sum_{y_0 \in \{0,1\}}
     e^{2 \pi i ((x_0 x_1 y_0) / 2 + ((1 ++ x_0) (1 - 2 y_0)) / 8)}
     \ket{x_0, x_0 y_0 ++ (1 ++ x_0) x_1} \) *)
  let ch_result co ta w =
    match targets2 co ta w with
    | Error error -> Error error
    | Ok (control, target) -> (
        match normalisation_factor co w with
        | Error error -> Error error
        | Ok normalisation -> (
            match q2 co ta w with
            | Error error -> Error error
            | Ok target_qubit ->
                Ok
                  {
                    phase =
                      Poly.simplify
                        (Prod
                           ( Scal div2,
                             Prod (Qubit control, Prod (Qubit target, Qubit (yy 0 w)))
                           )
                        ++ normalisation);
                    ket = Ket.simplify (ket_with_target ta w target_qubit);
                    path_var = [ w ];
                  }))

  let ch co ta w =
    match ch_result co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let cx_result co ta w =
    match targets2 co ta w with
    | Error error -> Error error
    | Ok (control, target) ->
        Ok
          {
            phase = Scal Q.zero ++ empty;
            ket = ket_with_target ta w (SumMod2 (control, target));
            path_var = [];
          }

  let cx co ta w =
    match cx_result co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let crz_result k co ta w =
    match targets2 co ta w with
    | Error error -> Error error
    | Ok (control, target) ->
        Ok
          {
            phase =
              Prod (Scal (divk k), Prod (Qubit control, Qubit target)) ++ empty;
            ket = ket_with_target ta w target;
            path_var = [];
          }

  let crz k co ta w =
    match crz_result k co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let cz_result co ta w = crz_result 1 co ta w

  let cz co ta w =
    match cz_result co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let cs_result co ta w = crz_result 2 co ta w

  let cs co ta w =
    match cs_result co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let ct_result co ta w = crz_result 3 co ta w

  let ct co ta w =
    match ct_result co ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co; ta ] w error

  let ccx_result co1 co2 ta w =
    match targets3 co1 co2 ta w with
    | Error error -> Error error
    | Ok (control1, control2, target) ->
        Ok
          {
            phase = Scal Q.zero ++ empty;
            ket =
              ket_with_target ta w
                (SumMod2 (Prod (control1, control2), target));
            path_var = [];
          }

  let ccx co1 co2 ta w =
    match ccx_result co1 co2 ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co1; co2; ta ] w error

  let ccrz k co1 co2 ta w =
    match targets3 co1 co2 ta w with
    | Error error -> Error error
    | Ok (control1, control2, target) ->
        Ok
          {
            phase =
              Prod
                ( Scal (divk k),
                  Prod (Qubit control1, Prod (Qubit control2, Qubit target)) )
              ++ empty;
            ket = ket_with_target ta w target;
            path_var = [];
          }

  let ccz_result co1 co2 ta w = ccrz 1 co1 co2 ta w

  let ccz co1 co2 ta w =
    match ccz_result co1 co2 ta w with
    | Ok path_sum -> path_sum
    | Error error -> gate_failure [ co1; co2; ta ] w error
  let sh3 = { phase = Scal div8 ++ empty; ket = [| Var 0 |]; path_var = [] }
end
