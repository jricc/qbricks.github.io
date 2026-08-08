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
module PS = Poly.String
module QS = Qubit.String
module Ket = Path_sum.Ket
module KS = Ket.String
module PSS = Path_sum.String
module Monome = Poly.Monome

type reduction_error = MalformedPathSum of string

module Simplification = struct
  let simplify ?(debug = false) (ps : Path_sum.t) : Path_sum.t =
    {
      path_var = ps.path_var;
      ket = Path_sum.Ket.simplify ps.ket;
      phase = Poly.simplify ~debug ps.phase;
    }
end

module Elim = struct
  let elim ?(debug = false) ps =
    if debug then printf "Reduction_rule.elim,\nps =\n%s\n" (PSS.pretty ps);
    let p, k = (ps.phase, ps.ket) in
    let rec aux path_var acc =
      match path_var with
      | y :: path_var' ->
          let condition_ket = lazy (not (Path_sum.Ket.member y k)) in
          let condition_poly = lazy (not (Poly.member y p)) in
          if Lazy.force condition_ket then
            if Lazy.force condition_poly then aux path_var' acc
            else aux path_var' (y :: acc)
          else aux path_var' (y :: acc)
      | [] -> acc
    in
    let ps' : Path_sum.t =
      {
        phase = ps.phase;
        ket = ps.ket;
        path_var = List.rev (aux ps.path_var []);
      }
    in
    if debug then printf "Reduction_rule.elim,\nps' =\n%s\n" (PSS.pretty ps');
    ps'
end

module HH = struct
  let empty = Poly.empty
  let find = Poly.find
  let del = Poly.del
  let ( ++ ) = Poly.( ++ )
  let member = Monome.member
  let remove = Monome.remove
  let simplify p = Poly.simplify p
  let qsimplify p = Qubit.simplify p

  (* Validate y0 and select yi in one phase traversal. For example, with
     [1/2*y0*y1 + 1/2*y0*y2], the traversal counts both pairs and keeps y1,
     the first valid candidate in the canonical phase order. *)
  let analyze_y0 y0 ?(debug = false) (ps : Path_sum.t) :
      (int option, reduction_error) result =
    if Path_sum.Ket.member y0 ps.ket then Ok None
    else
      let width = Array.length ps.ket in
      let rec monome_variables (monome : Monome.t) variables =
        match monome with
        | Scal _ -> variables
        | Qubit qubit -> List.rev_append (Qubit.extract_var qubit) variables
        | Prod (m1, m2) ->
            monome_variables m2 (monome_variables m1 variables)
      in
      let add_occurrence occurrences variable =
        let count =
          match IntMap.find_opt variable occurrences with
          | Some count -> count
          | None -> 0
        in
        IntMap.add variable (count + 1) occurrences
      in
      let candidate (monome : Monome.t) =
        match monome with
        | Prod (Scal coefficient, Prod (Qubit (Var v1), Qubit (Var v2)))
          when Q.equal coefficient div2 && Int.equal v1 y0 && width <= v2 ->
            Some (v2, monome, true)
        | Prod (Scal coefficient, Prod (Qubit (Var v1), Qubit (Var v2)))
          when Q.equal coefficient div2 && Int.equal v2 y0 && width <= v1 ->
            Some (v1, monome, false)
        | _ -> None
      in
      let rec aux phase candidates occurrences =
        if Poly.equal phase empty then
          if width <= 0 then
            Error
              (MalformedPathSum
                 (sprintf "Rule_hh.analyze_y0, n must be > 0, n = %d"
                    width))
          else
            let selected =
              List.find_opt
                (fun (yi, _, _) ->
                  match IntMap.find_opt yi occurrences with
                  | Some count -> Int.equal count 1
                  | None -> false)
                (List.rev candidates)
            in
            (match selected with
            | Some (yi, monome, y0_first) ->
                if debug then
                  if y0_first then
                    printf
                      "1. Rule_hh.extract_yi\np =%s\nv1 = %d, v2 = %d\n%!"
                      (Monome.String.exact monome) y0 yi
                  else
                    printf
                      "2. Rule_hh.extract_yi\np =%s\nv2 = %d, v1 = %d\n%!"
                      (Monome.String.exact monome) y0 yi;
                Ok (Some yi)
            | None -> Ok None)
        else
          let monome, remaining_phase = (find phase, del phase) in
          match monome with
          | Prod (Scal coefficient, m1)
            when coefficient <> div2 && member y0 m1 ->
              Ok None
          | _ ->
              let occurrences =
                match monome with
                | Prod (_, m1) when member y0 m1 ->
                    monome_variables m1 []
                    |> List.filter (fun variable -> width <= variable)
                    |> List.sort_uniq Int.compare
                    |> List.fold_left add_occurrence occurrences
                | _ -> occurrences
              in
              let candidates =
                match candidate monome with
                | Some candidate -> candidate :: candidates
                | None -> candidates
              in
              aux remaining_phase candidates occurrences
      in
      aux ps.phase [] IntMap.empty

  (* For example:
       phase = 1/2*y0*yi + 1/2*y0*x0 + 1/4*yi + 1/8*x1
     becomes Q = x0, R_with_yi = 1/4*yi, R_without_yi = 1/8*x1. *)
  let partition_hh_phase ?(debug = false) (phase : Poly.t) width y0 yi :
      (Poly.t * Poly.t * Poly.t, reduction_error) result =
    if width <= 0 then
      Error
        (MalformedPathSum
           (sprintf "Rule_hh.partition_hh_phase, width must be > 0, width = %d"
              width))
    else
      let rec aux phase q r_with_yi r_without_yi =
        if Poly.is_empty phase then (q, r_with_yi, r_without_yi)
        else
          let monome, remaining_phase = (find phase, del phase) in
          if member y0 monome then
            let q =
              match monome with
              | Prod (Scal coefficient, m1)
                when Q.equal coefficient div2 && not (member yi m1) -> (
                  match remove y0 m1 with
                  | Some m1_without_y0 -> m1_without_y0 ++ q
                  | None -> q)
              | _ -> q
            in
            aux remaining_phase q r_with_yi r_without_yi
          else if member yi monome then
            aux remaining_phase q (monome ++ r_with_yi) r_without_yi
          else aux remaining_phase q r_with_yi (monome ++ r_without_yi)
      in
      let q, r_with_yi, r_without_yi = aux phase empty empty empty in
      if debug then
        printf "Rule_hh.partition_hh_phase, Q = %s\n%!" (PS.pretty q width);
      if debug then
        printf "Rule_hh.partition_hh_phase, R_with_yi = %s\n%!"
          (PS.pretty r_with_yi width);
      if debug then
        printf "Rule_hh.partition_hh_phase, R_without_yi = %s\n%!"
          (PS.pretty r_without_yi width);
      Ok (q, r_with_yi, r_without_yi)

  let path_variables_with_possible_yi (phase : Poly.t) width =
    let rec aux phase candidates =
      if Poly.equal phase Poly.empty then List.sort_uniq Int.compare candidates
      else
        let monome, remaining_phase = (Poly.find phase, Poly.del phase) in
        let candidates =
          match monome with
          | Prod (Scal coefficient, Prod (Qubit (Var v1), Qubit (Var v2)))
            when Q.equal coefficient div2 ->
              (* This is only a prefilter for [analyze_y0]: every path variable can
                 be tried as y0, but a successful match also needs a path
                 variable yi. For example, [1/2*y0*y1] provides a possible yi
                 for both variables, whereas [1/2*x0*y0] provides none for y0. *)
              let candidates =
                if width <= v2 then v1 :: candidates else candidates
              in
              if width <= v1 then v2 :: candidates else candidates
          | _ -> candidates
        in
        aux remaining_phase candidates
    in
    aux phase []

  (* HH removes the summation variable y0 and the constrained variable yi in
     one canonical-to-canonical transformation. *)
  let remove_matched_path_variables path_variables y0 yi =
    List.filter
      (fun path_variable ->
        not (Int.equal path_variable y0 || Int.equal path_variable yi))
      path_variables

  let hh_aux y0 yi ?(debug = false) (ps : Path_sum.t) :
      (Path_sum.t option, reduction_error) result =
    if debug then
      printf "Rule_hh.hh_aux, y0 = y%d\n%!" (y0 - Array.length ps.ket);
    if debug then printf "Rule_hh.hh_aux, ps =\n%!%s\n%!" (PSS.pretty ps);
    let n = Array.length ps.ket in
    if debug then
      printf "Rule_hh.hh_aux, yi = y%d\n%!" (yi - Array.length ps.ket);
    match partition_hh_phase ~debug ps.phase n y0 yi with
    | Error reduction_error -> Error reduction_error
    | Ok (q, r_with_yi, r_without_yi) ->
        if debug then printf "Rule_hh.hh_aux, q = %s\n%!" (PS.pretty q n);
        if debug then
          printf "Rule_hh.hh_aux, ps.phase = %s\n%!" (PS.pretty ps.phase n);
        let substituted_r_with_yi =
          if Poly.is_empty r_with_yi then empty
          else Poly.substitute_rules_hh r_with_yi yi q ~debug
        in
        let ps_output : Path_sum.t =
          {
            (* Simplifying after the merge also combines terms that become
               equal across both parts. For example, substituting yi <- x0 in
               [1/4*yi] and merging [1/4*x0] produces [1/2*x0]. *)
            phase = simplify (Poly.merge substituted_r_with_yi r_without_yi);
            ket =
              Path_sum.Ket.substitute ps.ket yi
                (qsimplify (Poly.to_qubit q));
            path_var = remove_matched_path_variables ps.path_var y0 yi;
          }
        in
        Ok (Some ps_output)

  let hh ?(debug = false) ?(y0_to_remove = -1) (ps : Path_sum.t) :
      (Path_sum.t, reduction_error) result =
    let width = Array.length ps.ket in
    if Int.equal y0_to_remove (-1) then
      (* Try y0 in order of arrival *)
      let rec aux (acc : Path_sum.t) candidates = function
        | y0 :: y0_remain ->
            if debug then
              printf "Rule_hh.hh.accepted, y0 candidate = %d\n\n%!" (y0 - width);
            if List.mem y0 candidates then
              match analyze_y0 y0 acc ~debug with
              | Error reduction_error -> Error reduction_error
              | Ok (Some yi) ->
                  if debug then
                    printf "Rule_hh.hh.accepted, y0 = %d\n\n%!" (y0 - width);
                  (match hh_aux y0 yi acc ~debug with
                  | Error reduction_error -> Error reduction_error
                  | Ok (Some acc_reduced) ->
                      (if debug then
                         printf "Rule_hh.hh.accepted.match hh_aux, y0 = %d\n%!"
                           (y0 - width);
                       if debug then
                         printf
                           "Rule_hh.hh.accepted.match hh_aux, acc_reduced =\n\
                           %s\n\n\
                            %!"
                           (PSS.pretty acc_reduced);
                       (* [hh_aux] already simplifies its phase and ket. For
                          example, [1/2*y0*y1 + 1/2*y2*y3] becomes the already
                          simplified [1/2*y2*y3], so do not simplify it again. *)
                       aux acc_reduced
                         (path_variables_with_possible_yi acc_reduced.phase width)
                         y0_remain)
                  | Ok None -> aux acc candidates y0_remain)
              | Ok None -> aux acc candidates y0_remain
            else aux acc candidates y0_remain
        | _ -> Ok acc
      in
      (* Keep malformed zero-width inputs on the error path through
         [analyze_y0]; candidate filtering is only valid for positive widths. *)
      let candidates =
        if width <= 0 then ps.path_var
        else path_variables_with_possible_yi ps.phase width
      in
      aux ps candidates ps.path_var
    else
      (* The user proposes y0. *)
      match analyze_y0 y0_to_remove ps with
      | Error reduction_error -> Error reduction_error
      | Ok (Some yi) -> (
          match hh_aux y0_to_remove yi ps with
          | Error reduction_error -> Error reduction_error
          | Ok (Some ps_output) -> Ok ps_output
          | Ok None -> Ok ps)
      | Ok None -> Ok ps

end

module Rename = struct
  (** [_string_update_pvs substitutions] converts a list of path variable
      substitutions to a string representation. Format:
      "(old,new);(old,new);..." Example:
      [_string_update_pvs [(5, 2); (7, 3); (9, 4)]] returns
      ["(5,2);(7,3);(9,4)"] *)
  let _string_update_pvs update_pvs =
    let rec string_update_pvs_rec update_pvs =
      match update_pvs with
      | (pv, pv') :: [] ->
          "(" ^ string_of_int pv ^ "," ^ string_of_int pv' ^ ")"
      | (pv, pv') :: update_pvs' ->
          "(" ^ string_of_int pv ^ "," ^ string_of_int pv' ^ ");"
          ^ string_update_pvs_rec update_pvs'
      | [] -> ""
    in
    "[" ^ string_update_pvs_rec update_pvs ^ "]"

  (* y0,y3,y6 -> (y0,y0);(y3,y1);(y6,y2) *)

  (** [_find_update_path_var ps] creates substitution pairs mapping path
      variables to new contiguous indices. The new indices start from the number
      of qubits in the path sum. For a path sum with 3 qubits and path variables
      [10; 12; 15], returns [(10, 3); (12, 4); (15, 5)] *)
  let _find_update_path_var (ps' : Path_sum.t) =
    let rec aux pvs indice acc =
      match pvs with
      | pv :: [] -> (pv, indice) :: acc
      | pv :: pvs' -> aux pvs' (indice + 1) ((pv, indice) :: acc)
      | [] -> acc
    in
    List.rev
      (aux (List.fast_sort compare ps'.path_var) (Array.length ps'.ket) [])

  (** [_path_var_substitute vars substitutions] applies multiple substitutions
      to a list of path variables. Each substitution (old, new) replaces all
      occurrences of old with new. Example:
      [_path_var_substitute [10; 12; 15] [(10, 3); (12, 4); (15, 5)]] returns
      [3; 4; 5] *)
  let _path_var_substitute pvs update_pvs =
    let rec substitute_rec pvs pv1 pv2 (acc : int list) : int list =
      match pvs with
      | [] -> acc
      | pv :: pvs' when Int.equal pv pv1 ->
          substitute_rec pvs' pv1 pv2 (pv2 :: acc)
      | pv :: pvs' -> substitute_rec pvs' pv1 pv2 (pv :: acc)
    in
    let rec path_sum_update update pvs_output =
      match update with
      | (pv1, pv2) :: [] -> List.rev (substitute_rec pvs_output pv1 pv2 [])
      | (pv1, pv2) :: update' ->
          path_sum_update update'
            (List.rev (substitute_rec pvs_output pv1 pv2 []))
      | [] -> pvs_output
    in
    path_sum_update update_pvs pvs

  let path_var_as_poly path_var = Poly.q path_var

  let _substitute_path_vars_in_ket_and_phase ket_before_substitution
      phase_before_substitution path_var_updates =
    (* Ket substitutions are independent, so all variable renamings can be
       applied in one traversal of the ket. *)
    let ket_substitutions =
      List.filter_map
        (fun (old_path_var, new_path_var) ->
          if Int.equal old_path_var new_path_var then None
          else Some (old_path_var, Qubit.Var new_path_var))
        path_var_updates
    in
    let ket_after_substitution =
      Path_sum.Ket.substitute_many ket_before_substitution ket_substitutions
    in
    (* The polynomial API substitutes one variable at a time, so the phase keeps
       a fold even though the ket can use grouped substitutions. *)
    let phase_after_substitution =
      List.fold_left
        (fun phase (old_path_var, new_path_var) ->
          if Int.equal old_path_var new_path_var then phase
          else
            Poly.substitute_poly phase old_path_var
              (path_var_as_poly new_path_var))
        phase_before_substitution path_var_updates
    in
    (ket_after_substitution, phase_after_substitution)

  (** [_substitute_path_var ?debug ps substitutions] applies substitutions to
      all components of a path sum. Updates path variables in phase polynomial,
      ket, and path_var list. Example: For path sum with:
      - 3 qubits
      - path variables [10; 12]
      - phase containing terms with y7 and y9 (10 - 3 and 12 - 3)
      - ket containing |y7 + y9> [_substitute_path_var ps [(10, 3); (12, 4)]]
        produces a path sum where:
      - path variables become [3; 4]
      - phase terms use y0 and y1 (3 - 3 and 4 - 3)
      - ket becomes |y0 + y1> *)
  let _substitute_path_var ?(debug = false) (ps : Path_sum.t) update_pvs =
    let k, p =
      _substitute_path_vars_in_ket_and_phase ps.ket ps.phase update_pvs
    in
    let ps_output : Path_sum.t =
      {
        phase = p;
        ket = k;
        path_var = _path_var_substitute ps.path_var update_pvs;
      }
    in
    if debug then
      printf "Reduction_rules.substitute_path, update_pvs = %s\n"
        (_string_update_pvs update_pvs);
    if debug then
      printf "Reduction_rules.substitute_path,\nps_output =\n%s\n"
        (PSS.pretty ps_output);
    ps_output

  let rename ?(debug = false) (ps : Path_sum.t) =
    _substitute_path_var ~debug ps (_find_update_path_var ps)
end

module Variable_replacement = struct
  (** [condition_to_substitute ?debug q except ps] returns [Some y] when [q]
      has the proved form [y xor Q], where [y] is a declared path variable that
      does not occur in [Q], the phase, or another output qubit. *)

  let condition_to_substitute ?(debug = false) (q : Qubit.t) except
      (ps : Path_sum.t) : (int option, reduction_error) result =
    let width = Array.length ps.ket in
    let rec direct_xor_path_variables (qubit : Qubit.t) =
      match qubit with
      | Var variable
        when width <= variable
             && ListBis.member variable ps.path_var Int.equal ->
          [ variable ]
      | SumMod2 (left_qubit, right_qubit) ->
          direct_xor_path_variables left_qubit
          @ direct_xor_path_variables right_qubit
      | Qubit.Zero | Qubit.One | Var _ | Prod _ -> []
    in
    let rec occurs_under_product variable (qubit : Qubit.t) =
      match qubit with
      | Prod _ as product -> Qubit.member variable product
      | SumMod2 (left_qubit, right_qubit) ->
          occurs_under_product variable left_qubit
          || occurs_under_product variable right_qubit
      | Qubit.Zero | Qubit.One | Var _ -> false
    in
    let direct_path_variables = direct_xor_path_variables q in
    let occurs_once variable =
      Int.equal 1
        (List.fold_left
           (fun count candidate ->
             if Int.equal variable candidate then count + 1 else count)
           0 direct_path_variables)
    in
    let is_available variable =
      occurs_once variable
      && not (occurs_under_product variable q)
      && not (Path_sum.Ket.member ~except variable ps.ket)
      && not (Poly.member variable ps.phase)
    in
    let available_path_variables =
      List.sort_uniq Int.compare
        (List.filter is_available direct_path_variables)
    in
    if debug then
      printf "Reduction_rules.condition_to_substitute, candidates = %s\n\n"
        (ListBis.string_int available_path_variables);
    match available_path_variables with
    | [ variable ] -> Ok (Some variable)
    | _ -> Ok None

  (* original_qubit[qubit_to_replace <- replacement_qubit] *)
  let substitute_qubit_in_qubit original_qubit replacement_qubit qubit_to_replace
      : Qubit.t =
    let rec substitute_in_qubit (qubit : Qubit.t) : Qubit.t =
      match qubit with
      | qubit when Qubit.equal qubit qubit_to_replace -> replacement_qubit
      | Qubit.SumMod2 (left_qubit, right_qubit) ->
          Qubit.SumMod2
            (substitute_in_qubit left_qubit, substitute_in_qubit right_qubit)
      | Qubit.Prod (left_qubit, right_qubit) ->
          Qubit.Prod
            (substitute_in_qubit left_qubit, substitute_in_qubit right_qubit)
      | _ -> qubit
    in
    Qubit.simplify (substitute_in_qubit original_qubit)

  (* Keep the input ket unchanged. Allocate a new ket only when a substitution
     really changes at least one qubit. *)
  let substitute_qubit_in_ket (ket_before_substitution : Path_sum.Ket.t)
      replacement_qubit qubit_to_replace =
    let width = Array.length ket_before_substitution in
    let rec substitute_in_ket changed_ket qubit_index =
      if qubit_index = width then
        match changed_ket with
        | None -> ket_before_substitution
        | Some changed_ket -> changed_ket
      else
        let original_qubit = ket_before_substitution.(qubit_index) in
        let substituted_qubit =
          substitute_qubit_in_qubit original_qubit replacement_qubit
            qubit_to_replace
        in
        if Qubit.equal original_qubit substituted_qubit then
          substitute_in_ket changed_ket (qubit_index + 1)
        else
          let changed_ket =
            match changed_ket with
            | Some changed_ket -> changed_ket
            | None -> Array.copy ket_before_substitution
          in
          changed_ket.(qubit_index) <- substituted_qubit;
          substitute_in_ket (Some changed_ket) (qubit_index + 1)
    in
    substitute_in_ket None 0

  let variable_replacement ?(debug = false) (input : Path_sum.t) :
      (Path_sum.t option, reduction_error) result =
    let width = Array.length input.ket in

    if debug then
      printf "Reduction_rules.variable_replacement, input.phase = %s\n\n"
        (PS.pretty input.phase width);

    let ps = Simplification.simplify input in

    if debug then
      printf "Reduction_rules.variable_replacement, ps.phase = %s\n\n"
        (PS.pretty ps.phase width);

    if List.exists (fun path_var -> path_var < width) ps.path_var then
      Error
        (MalformedPathSum
           "Rules.Variable_replacement.variable_replacement: path variable index below ket width")
    else if List.equal Int.equal ps.path_var [] then Ok None
    else
      let new_y = ListBis.max_int ps.path_var + 1 in

      let rec iterate_over_qubits indice =
        if Int.equal indice width then Ok None
        else
          let process_qubit (qubit_i : Qubit.t) =
            match qubit_i with
            | SumMod2 _ -> (
                match condition_to_substitute ~debug qubit_i indice ps with
                | Error reduction_error -> Error reduction_error
                | Ok (Some v) ->
                    Ok
                      (Some
                         ( substitute_qubit_in_ket ps.ket (Var new_y) qubit_i,
                           v ))
                | Ok None -> iterate_over_qubits (indice + 1))
            | _ -> iterate_over_qubits (indice + 1)
          in
          process_qubit ps.ket.(indice)
      in

      match iterate_over_qubits 0 with
      | Error reduction_error -> Error reduction_error
      | Ok (Some (k, v)) ->
          let output : Path_sum.t =
            {
              phase = ps.phase;
              ket = k;
              path_var =
                List.sort_uniq Int.compare
                  (new_y :: ListBis.remove v ps.path_var);
            }
          in
          Ok (Some (Rename.rename output))
      | Ok None -> Ok None

  (* Factorization by variable replacement.
   Example: phase = x0y0 + x0y1, ket = |y0 + y1>
   After replacement: phase[y0 <- y0 + y1] = x0y0, ket[y0 <- y0 + y1] = |y0> *)
  let variable_replacement_factorisation ?(debug = false) (state : Path_sum.t) =
    if debug then
      printf "Rules.variable_replacement_factorisation, state =\n%s\n%!"
        (PSS.pretty state);

    let width = Array.length state.ket in

    (* Try to factorize one step in the polynomial *)
    let rec factorize_step (p : Poly.t) (acc_state : Path_sum.t) :
        Path_sum.t option =
      if debug then (
        printf
          "Rules.variable_replacement_factorisation.factorize_step, p = %s\n\n\
           %!"
          (PS.pretty p width);
        printf
          "Rules.variable_replacement_factorisation.factorize_step, acc_state =\n\
           %s\n\n\
           %!"
          (PSS.pretty acc_state));

      let poly = acc_state.phase in

      if Poly.size p < 2 then
        (* if ok then Some acc_state else  *)
        None
      else
        let m1 = Poly.find p in
        let p1 = Poly.del p in
        let m2 = Poly.find p1 in

        if debug then (
          printf "Rules.variable_replacement_factorisation, m1 = %s\n\n%!"
            (Monome.String.pretty m1 width);
          printf "Rules.variable_replacement_factorisation, m2 = %s\n\n%!"
            (Monome.String.pretty m2 width));

        match (m1, m2) with
        | ( Prod (Scal q1, Prod (Qubit (Var v1), Qubit (Var v2))),
            Prod (Scal q2, Prod (Qubit (Var v3), Qubit (Var v4))) )
          when Q.equal q1 q2 && Q.equal q1 Rational.div2 && v1 = v3
               && width <= v2 && width <= v4 ->
            let new_qubit : Qubit.t = SumMod2 (Var v2, Var v4) in

            let new_poly : Poly.t =
              Poly.insert (Monome.Qubit (Var v2))
                (Poly.insert (Monome.Qubit (Var v4))
                   (Poly.insert
                      (Monome.Prod
                         ( Scal (Q.of_int (-2)),
                           Prod (Qubit (Var v2), Qubit (Var v4)) ))
                      Poly.empty))
            in

            if debug then (
              printf
                "Rules.variable_replacement_factorisation, new_qubit = %s\n\n%!"
                (QS.pretty new_qubit width);
              printf
                "Rules.variable_replacement_factorisation, new_poly = %s\n\n%!"
                (PS.pretty new_poly width));

            let poly_subst = Poly.substitute_poly poly v2 new_poly in
            let ket_subst =
              substitute_qubit_in_ket acc_state.ket new_qubit (Var v2)
            in

            if debug then (
              printf
                "Rules.variable_replacement_factorisation, ket_subst = %s\n\n%!"
                (KS.pretty ket_subst);
              printf "Rules.variable_replacement_factorisation, poly = %s\n\n%!"
                (PS.pretty poly width);
              printf
                "Rules.variable_replacement_factorisation, poly_subst = %s\n\n\
                 %!"
                (PS.pretty poly_subst width));

            let out_state : Path_sum.t =
              {
                phase = poly_subst;
                ket = ket_subst;
                path_var = acc_state.path_var;
              }
            in

            if debug then
              printf
                "Rules.variable_replacement_factorisation, out_state =\n\
                 %s\n\n\
                 %!"
                (PSS.pretty out_state);

            let out_state_simplified = Simplification.simplify out_state in

            if debug then
              printf
                "Rules.variable_replacement_factorisation, \
                 out_state_simplified =\n\
                 %s\n\n\
                 %!"
                (PSS.pretty out_state_simplified);

            let number_of_sum_input =
              Ket.number_of_sum acc_state.ket + Poly.size poly
            in
            let number_of_sum_out_state_simplified =
              Ket.number_of_sum out_state_simplified.ket
              + Poly.size out_state_simplified.phase
            in

            if debug then
              printf
                "Rules.variable_replacement_factorisation, number_of_sum_input \
                 = %d, number_of_sum_out_state_simplified = %d\n\n\
                 %!"
                number_of_sum_input number_of_sum_out_state_simplified;

            if number_of_sum_input <= number_of_sum_out_state_simplified then
              (* simplification not useful, continue with next monome *)
              factorize_step p1 acc_state
            (* else if number_of_sum_input = number_of_sum_out_state_simplified
            then Some acc_state *)
              else
              (* simplification done, restart from scratch with new state *)
              Some out_state_simplified
            (* factorize_step out_state_simplified.phase out_state_simplified true *)
        | _, _ ->
            (* no simplification, continue with next monome *)
            factorize_step p1 acc_state
    in

    match factorize_step state.phase state with
    | Some new_state -> new_state
    | None -> state

  let replace_not_path_var_by_var ?(debug = false) (input_state : Path_sum.t) =
    let width = Array.length input_state.ket in

    let is_declared_path_var variable =
      width <= variable
      && ListBis.member variable input_state.path_var Int.equal
    in

    (* Return the path variables, whether a non-path term is present, and the
       Boolean polynomial represented by an affine XOR expression. *)
    let rec affine_xor_expression = function
      | Qubit.Zero -> Some ([], false, Poly.empty)
      | Qubit.One -> Some ([], true, Poly.one)
      | Qubit.Var variable when is_declared_path_var variable ->
          Some ([ variable ], false, Poly.q variable)
      | Qubit.Var variable when 0 <= variable && variable < width ->
          Some ([], true, Poly.q variable)
      | Qubit.SumMod2 (left_qubit, right_qubit) -> (
          match
            ( affine_xor_expression left_qubit,
              affine_xor_expression right_qubit )
          with
          | ( Some (left_path_vars, left_has_shift, left_poly),
              Some (right_path_vars, right_has_shift, right_poly) ) ->
              Some
                ( left_path_vars @ right_path_vars,
                  left_has_shift || right_has_shift,
                  Poly.merge left_poly right_poly )
          | _ -> None)
      | Qubit.Prod _ | Qubit.Var _ -> None
    in

    let count_direct_path_var ket path_var =
      Array.fold_left
        (fun count qubit ->
          if
            Qubit.equal ~wq1:width ~wq2:width (Qubit.simplify qubit)
              (Qubit.Var path_var)
          then
            count + 1
          else count)
        0 ket
    in

    let rec try_qubits qubit_index =
      if qubit_index = width then input_state
      else
        let shifted_path_var =
          Qubit.simplify input_state.ket.(qubit_index)
        in
        match affine_xor_expression shifted_path_var with
        | Some ([ path_var ], true, shifted_path_var_poly) ->
            let direct_before =
              count_direct_path_var input_state.ket path_var
            in
            let candidate_ket =
              Path_sum.Ket.substitute ~debug input_state.ket path_var
                shifted_path_var
            in
            let direct_after = count_direct_path_var candidate_ket path_var in
            if debug then
              printf
                "Rules.replace_not_path_var_by_var, path_var = %d, \
                 direct_before = %d, direct_after = %d\n\n%!"
                path_var direct_before direct_after;
            if direct_before >= direct_after then try_qubits (qubit_index + 1)
            else
              let substituted_phase =
                match shifted_path_var with
                | Qubit.SumMod2 (Qubit.One, Qubit.Var variable)
                  when Int.equal variable path_var ->
                    (* Boolean negation lifts directly to 1 - y. Avoid the
                       general affine-XOR lifting for this frequent case. *)
                    let one_minus_path_var =
                      Poly.merge Poly.one
                        (Poly.to_poly
                           (Monome.Prod
                              ( Monome.Scal Q.minus_one,
                                Monome.Qubit (Qubit.Var path_var) )))
                    in
                    Poly.substitute_poly ~debug input_state.phase path_var
                      one_minus_path_var
                | _ ->
                    Poly.substitute_rules_hh ~debug input_state.phase path_var
                      shifted_path_var_poly
              in
              let output_state : Path_sum.t =
                {
                  phase = substituted_phase;
                  ket = candidate_ket;
                  path_var = input_state.path_var;
                }
              in
              Simplification.simplify ~debug output_state
        | _ -> try_qubits (qubit_index + 1)
    in
    try_qubits 0

  module Ket = Path_sum.Ket
  module ArrayBis = Common.ArrayBis

  (* Poly normalized :
    (1/2 * [x0] * [y1] + 1/2 * [y1] + 1/2 * [y2] * [y3])
    ->
    (1/2 * [x0] * [y0] + 1/2 * [y0] + 1/2 * [y1] * [y2]) *)
  let poly_normalized ?(debug = false) (ps : Path_sum.t) :
      (Path_sum.t, reduction_error) result =
    if debug then
      printf "Rules.poly_normalised, input ps =\n%s\n%!" (PSS.pretty ps);

    let ps = Rename.rename ps in

    if debug then
      printf "Rules.poly_normalised, rename ps =\n%s\n%!" (PSS.pretty ps);

    let poly = ref ps.phase in
    let ket = ref ps.ket in
    let pvs = ps.path_var in
    if debug then
      printf "Rules.poly_normalised, pvs = %s\n%!" (ListBis.string_int pvs);

    let wq = Array.length !ket in
    (* Path var memoisation *)
    let nb_pvs = List.length pvs in
    let tmp = Array.make nb_pvs (-1) in

    if debug then printf "Rules.poly_normalised, wq = %d\n%!" wq;

    let path_var_poly =
      ListBis.remove_duplicate (Poly.extract_path_var !poly wq)
    in

    if debug then
      printf "Rules.poly_normalised, path_var_poly = %s\n%!"
        (ListBis.string_int path_var_poly);

    let path_var_ket = Ket.extract_path_var !ket in

    if debug then
      printf "Rules.poly_normalised, path_var_ket = %s\n%!"
        (ListBis.string_int path_var_ket);
    if debug then
      printf "Rules.poly_normalised, path_var_poly = %s\n\n%!"
        (ListBis.string_int path_var_poly);

    let invalid_path_var =
      List.find_opt
        (fun path_var -> path_var < wq || wq + nb_pvs <= path_var)
        (path_var_poly @ path_var_ket)
    in
    match invalid_path_var with
    | Some path_var ->
        Error
          (MalformedPathSum
             (sprintf
                "Rules.Variable_replacement.poly_normalized: path \
                 variable %d is outside [%d,%d)"
                path_var wq (wq + nb_pvs)))
    | None ->

    let tmp_construct path_var =
      let rec aux i l =
        if debug then
          printf
            "Rules.poly_normalised.tmp_construct, i = %d, l = %s, tmp = %s\n%!"
            i (ListBis.string_int l) (ArrayBis.string_int tmp);
        if wq <= i && i < wq + nb_pvs then
          match l with
          | hd :: tl ->
              if tmp.(hd - wq) = -1 then (
                tmp.(hd - wq) <- i;
                aux (i + 1) tl)
              else aux i tl
          | [] -> ()
      in
      let starting_value = Int.max wq (ArrayBis.max_int tmp + 1) in
      if debug then
        printf "Rules.poly_normalised.tmp_construct, starting_value = %d\n%!"
          starting_value;
      aux starting_value path_var
    in

    if debug then printf "Rules.poly_normalised, wq = %d\n%!" wq;
    if debug then
      printf "Rules.poly_normalised, tmp = %s\n%!" (ArrayBis.string_int tmp);

    tmp_construct path_var_poly;

    if debug then
      printf "Rules.poly_normalised. path_var_poly, tmp = %s\n%!"
        (ArrayBis.string_int tmp);

    let tmp_list =
      List.rev
        (ListBis.remove (-1) (List.sort_uniq Int.compare (Array.to_list tmp)))
    in

    if debug then
      printf "Rules.poly_normalised. path_var_poly, tmp_list = %s\n%!"
        (ListBis.string_int tmp_list);

    let path_var_available =
      ListBis.missing_in_range ~lower:wq tmp_list (wq + nb_pvs)
    in

    if debug then
      printf "Rules.poly_normalised. path_var_poly, path_var_available = %s\n%!"
        (ListBis.string_int path_var_available);

    let complete_tmp_with_path_var_missing path_var_available =
      let rec aux i l =
        if nb_pvs <= i then ()
        else if tmp.(i) = -1 then
          match l with
          | hd :: tl ->
              tmp.(i) <- hd;
              aux (i + 1) tl
          | [] -> ()
        else aux (i + 1) l
      in
      aux 0 path_var_available
    in
    complete_tmp_with_path_var_missing path_var_available;

    if debug then
      printf "Rules.poly_normalised.aux path_var_available, tmp = %s\n\n%!"
        (ArrayBis.string_int tmp);

    for i = 0 to nb_pvs - 1 do
      if debug then printf "Rules.poly_normalised, -tmp(%d) = -%d\n%!" i tmp.(i);
      if tmp.(i) <> -1 then (
        poly := Poly.substitute (i + wq) !poly (Var (-tmp.(i)));
        ket := Ket.substitute !ket (i + wq) (Var (-tmp.(i))));

      if debug then
        printf "Rules.poly_normalised, [%d <- -%d]\n%!" (i + wq) tmp.(i);
      if debug then
        printf "Rules.poly_normalised, poly_acc = %s\n%!" (PS.pretty !poly wq);
      if debug then
        printf "Rules.poly_normalised, ket_acc = %s\n%!" (KS.pretty !ket)
    done;

    if debug then
      printf "Rules.poly_normalised, new_poly = %s\n%!" (PS.pretty !poly wq);
    if debug then
      printf "Rules.poly_normalised, new_ket = %s\n\n%!" (KS.pretty !ket);

    for i = 0 to nb_pvs - 1 do
      if debug then printf "Rules.poly_normalised, tmp(%d) = %d\n%!" i tmp.(i);
      if tmp.(i) <> -1 then (
        poly := Poly.substitute (-tmp.(i)) !poly (Var tmp.(i));
        ket := Ket.substitute !ket (-tmp.(i)) (Var tmp.(i)));

      if debug then
        printf "Rules.poly_normalised, [%d <- %d]\n%!" (i + wq) tmp.(i);
      if debug then
        printf "Rules.poly_normalised, poly_acc = %s\n%!" (PS.pretty !poly wq);
      if debug then
        printf "Rules.poly_normalised, ket_acc = %s\n%!" (KS.pretty !ket)
    done;

    if debug then
      printf "Rules.poly_normalised, new_poly = %s\n%!" (PS.pretty !poly wq);
    if debug then
      printf "Rules.poly_normalised, new_ket = %s\n\n%!" (KS.pretty !ket);

    let output : Path_sum.t =
      { phase = !poly; ket = !ket; path_var = ps.path_var }
    in
    Ok output

end
