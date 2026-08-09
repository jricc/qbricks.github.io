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

open Alcotest
open Printf
open SQbricks
module QS = Qubit.String
module PS = Poly.String
open Path_sum
module KS = Ket.String
module PSS = String
module Monome = Poly.Monome
open Common
include Rational
open Program
module ProgS = String
include Macros

type poly = Poly.t
type monome = Monome.t

let to_poly (m : monome) : poly = Poly.to_poly m
let reduce_valid_path_sum ?(debug = false) input =
  match Reduction_algorithm.reduction_algorithm ~debug input with
  | Ok output -> output
  | Error (Rules.MalformedPathSum message) ->
      Alcotest.fail ("unexpected malformed path sum: " ^ message)
let ( ++ ) (q1 : Qubit.t) (q2 : Qubit.t) : Qubit.t = Qubit.SumMod2 (q1, q2)
let zero : Qubit.t = Qubit.Zero
let one : Qubit.t = Qubit.One

(* Insert with duplication *)
let ( +++ ) (m : Monome.t) (p : Poly.t) : Poly.t = Poly.insert m p
let x0 = Qubit.Var 0
let x1 = Qubit.Var 1
let x2 = Qubit.Var 2
let x0x1 = Monome.Qubit (Prod (Var 0, Var 1))
let x0x2 = Monome.Qubit (Prod (Var 0, Var 2))
let x1x2 = Monome.Qubit (Prod (Var 1, Var 2))
let x0x1x2 = Monome.Prod (Qubit x0, x1x2)
let v i = Qubit.Var i

(* let test_normalise_path_var ?(debug = true) ?(outputs1 = []) ?(outputs2 = [])
    (input : Path_sum.t) (expect : Path_sum.t) () =
  if debug then
    printf "Primitives.test_normalise_path_var, expect =\n%s\n\n%!"
      (PSS.pretty expect);
  if debug then
    printf "Primitives.test_normalise_path_var, input =\n%s\n\n%!"
      (PSS.pretty input);
  let input_normalised = Rules.Rename.normalise_path_var ~debug input in
  if debug then
    printf "Primitives.test_normalise_path_var, input_normalised =\n%s\n\n%!"
      (PSS.pretty input_normalised);
  let greet =
    Path_sum.equal ~debug ~outputs1 ~outputs2 input_normalised expect
  in
  let expect = true in
  check bool (sprintf "Primitives.test_normalise_path_var") expect greet *)

let p0 = Poly.zero

let test_list_bis_remove_preserves_order () =
  (* All matching values are removed without reordering the retained values. *)
  check (list int) "removed repeated value" [ 1; 3; 4 ]
    (ListBis.remove 2 [ 1; 2; 3; 2; 4 ])

let test_list_bis_remove_absent_value_preserves_list () =
  (* Removing an absent value must leave the original order unchanged. *)
  check (list int) "absent value" [ 1; 2; 3 ]
    (ListBis.remove 9 [ 1; 2; 3 ])

let test_list_bis_check_bounds_matches_documented_interval () =
  (* The lower bound is inclusive, the upper bound is exclusive, and every
     element must satisfy the interval. The empty list is therefore valid. *)
  let actual =
    [
      ListBis.check_bounds 1 4 [ 1; 2; 3 ];
      ListBis.check_bounds 1 4 [ 1; 2; 4 ];
      ListBis.check_bounds 1 4 [ 0; 1 ];
      ListBis.check_bounds 1 4 [];
    ]
  in
  check (list bool) "bounds results" [ true; false; false; true ] actual

let test_list_bis_extract_upper_bound_preserves_order () =
  (* Filtering values greater than or equal to the bound keeps input order. *)
  check (list int) "upper-bound extraction" [ 3; 5; 4 ]
    (ListBis.extract_upper_bound_list 3 [ 1; 3; 5; 2; 4 ])

let test_list_bis_extract_lower_bound_preserves_order () =
  (* Filtering values below the bound keeps input order. *)
  check (list int) "lower-bound extraction" [ 1; 0; 2 ]
    (ListBis.extract_lower_bound_list 3 [ 1; 3; 0; 5; 2 ])

let list_bis =
  [
    ( "remove preserves order",
      `Quick,
      test_list_bis_remove_preserves_order );
    ( "removing an absent value preserves the list",
      `Quick,
      test_list_bis_remove_absent_value_preserves_list );
    ( "check_bounds follows its documented interval",
      `Quick,
      test_list_bis_check_bounds_matches_documented_interval );
    ( "extract_upper_bound_list preserves order",
      `Quick,
      test_list_bis_extract_upper_bound_preserves_order );
    ( "extract_lower_bound_list preserves order",
      `Quick,
      test_list_bis_extract_lower_bound_preserves_order );
  ]

let malformed_zero_width_path_sum : Path_sum.t =
  { phase = p0; ket = [||]; path_var = [ 0 ] }

let test_hh_reports_malformed_path_sum () =
  let malformed =
    match Rules.HH.hh ~y0_to_remove:0 malformed_zero_width_path_sum with
    | Error (Rules.MalformedPathSum _) -> true
    | Ok _ -> false
  in
  check bool "malformed path sum" true malformed

let test_hh_keeps_unmatched_path_variable () =
  (* An unused path variable alone cannot be removed from a canonical path sum:
     its presence determines the implicit normalization factor. *)
  let input : Path_sum.t =
    { phase = p0; ket = [| x0 |]; path_var = [ 1 ] }
  in
  let output =
    match Rules.HH.hh input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "unmatched path variable is preserved" (PSS.exact input)
    (PSS.exact output)

let test_reduction_keeps_unmatched_path_variable () =
  let input : Path_sum.t =
    { phase = p0; ket = [| x0 |]; path_var = [ 1 ] }
  in
  let output = reduce_valid_path_sum input in
  check string "reduction preserves unmatched path variable" (PSS.exact input)
    (PSS.exact output)

let test_hh_explicitly_removes_matched_pair () =
  (* phase = 1/2 y0(x0 + y1), ket = |y1>.
     Summing over y0 imposes y1 = x0, so HH must remove y0 and y1 together. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit x0))
    +++ (Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2))) +++ Poly.empty)
  in
  let input : Path_sum.t =
    { phase; ket = [| v 2 |]; path_var = [ 1; 2 ] }
  in
  let expected : Path_sum.t =
    { phase = p0; ket = [| x0 |]; path_var = [] }
  in
  let output =
    match Rules.HH.hh ~y0_to_remove:1 input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "matched path variables are removed together"
    (PSS.exact expected) (PSS.exact output)

let test_hh_skips_unmatched_candidate_before_match () =
  (* phase = 1/2 y1y2, ket = |y2>, path variables = [y0; y1; y2].
     The absent y0 is ignored, then HH applies to y1 and y2. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 2), Qubit (v 3))) +++ Poly.empty
  in
  let input : Path_sum.t =
    { phase; ket = [| v 3 |]; path_var = [ 1; 2; 3 ] }
  in
  let expected : Path_sum.t =
    { phase = p0; ket = [| zero |]; path_var = [ 1 ] }
  in
  let output =
    match Rules.HH.hh input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "unmatched candidate before HH match" (PSS.exact expected)
    (PSS.exact output)

let test_hh_applies_two_successive_matches () =
  (* phase = 1/2 y0y1 + 1/2 y2y3, ket = |y1 + y3>.
     The first HH match removes y0,y1; the second removes y2,y3. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2)))
    +++ (Prod (Scal div2, Prod (Qubit (v 3), Qubit (v 4))) +++ Poly.empty)
  in
  let input : Path_sum.t =
    { phase; ket = [| v 2 ++ v 4 |]; path_var = [ 1; 2; 3; 4 ] }
  in
  let expected : Path_sum.t =
    { phase = p0; ket = [| zero |]; path_var = [] }
  in
  let output =
    match Rules.HH.hh input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "two successive HH matches" (PSS.exact expected)
    (PSS.exact output)

let test_hh_simplifies_remainder_after_substitution () =
  (* phase = 1/2 y0(yi + Q) + R and ket = |yi>, with yi = y1, Q = x0,
     and R = 1/4 y1 + 1/4 y1. Substitution gives R[yi <- Q] = 1/2 x0. *)
  let remainder_term : Monome.t = Prod (Scal div4, Qubit (v 2)) in
  let phase =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2)))
    +++ (Prod (Scal div2, Prod (Qubit (v 1), Qubit x0))
         +++ (remainder_term +++ (remainder_term +++ Poly.empty)))
  in
  let input : Path_sum.t =
    { phase; ket = [| v 2 |]; path_var = [ 1; 2 ] }
  in
  let expected : Path_sum.t =
    {
      phase = Prod (Scal div2, Qubit x0) +++ Poly.empty;
      ket = [| x0 |];
      path_var = [];
    }
  in
  let output =
    match Rules.HH.hh input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "remainder simplified after substitution" (PSS.exact expected)
    (PSS.exact output)

let test_hh_preserves_yi_candidate_order () =
  (* phase = 1/2 y0y1 + 1/2 y0y2, ket = |y1>.
     The canonical phase order selects y1, so Q = y2 and the ket becomes |y2>. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2)))
    +++ (Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 3))) +++ Poly.empty)
  in
  let input : Path_sum.t =
    { phase; ket = [| v 2 |]; path_var = [ 1; 2; 3 ] }
  in
  let expected : Path_sum.t =
    { phase = p0; ket = [| v 3 |]; path_var = [ 3 ] }
  in
  let output =
    match Rules.HH.hh ~y0_to_remove:1 input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "first yi follows canonical phase order" (PSS.exact expected)
    (PSS.exact output)

let test_hh_rejects_repeated_y0_yi_pair () =
  (* Two occurrences of 1/2 y0y1 do not satisfy the unique-pair condition. *)
  let pair : Monome.t =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2)))
  in
  let input : Path_sum.t =
    {
      phase = pair +++ (pair +++ Poly.empty);
      ket = [| v 2 |];
      path_var = [ 1; 2 ];
    }
  in
  let output =
    match Rules.HH.hh ~y0_to_remove:1 input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "repeated y0-yi pair is rejected" (PSS.exact input)
    (PSS.exact output)

let test_hh_rejects_y0_with_unauthorized_coefficient () =
  (* The 1/4 y0x0 term prevents y0 from matching the HH rule. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 1), Qubit (v 2)))
    +++ (Prod (Scal div4, Prod (Qubit (v 1), Qubit x0)) +++ Poly.empty)
  in
  let input : Path_sum.t =
    { phase; ket = [| v 2 |]; path_var = [ 1; 2 ] }
  in
  let output =
    match Rules.HH.hh ~y0_to_remove:1 input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "unauthorized y0 coefficient is rejected" (PSS.exact input)
    (PSS.exact output)

let test_hh_preserves_remainder_without_yi () =
  (* phase = 1/2 y0(yi + x0) + 1/4 yi + 1/8 x1.
     Only 1/4 yi is substituted; the independent 1/8 x1 term is preserved. *)
  let phase =
    Prod (Scal div2, Prod (Qubit (v 2), Qubit (v 3)))
    +++ (Prod (Scal div2, Prod (Qubit (v 2), Qubit x0))
         +++ (Prod (Scal div4, Qubit (v 3))
              +++ (Prod (Scal div8, Qubit x1) +++ Poly.empty)))
  in
  let input : Path_sum.t =
    { phase; ket = [| v 3; x1 |]; path_var = [ 2; 3 ] }
  in
  let expected : Path_sum.t =
    {
      phase =
        Prod (Scal div4, Qubit x0)
        +++ (Prod (Scal div8, Qubit x1) +++ Poly.empty);
      ket = [| x0; x1 |];
      path_var = [];
    }
  in
  let output =
    match Rules.HH.hh ~y0_to_remove:2 input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  check string "remainder without yi is preserved" (PSS.exact expected)
    (PSS.exact output)

let test_reduction_algorithm_reports_malformed_path_sum () =
  let malformed =
    match
      Reduction_algorithm.reduction_algorithm malformed_zero_width_path_sum
    with
    | Error (Rules.MalformedPathSum _) -> true
    | Ok _ -> false
  in
  check bool "malformed path sum" true malformed

let hh =
  [
    ( "hh reports malformed path sum",
      `Quick,
      test_hh_reports_malformed_path_sum );
    ( "hh keeps an unmatched path variable",
      `Quick,
      test_hh_keeps_unmatched_path_variable );
    ( "reduction keeps an unmatched path variable",
      `Quick,
      test_reduction_keeps_unmatched_path_variable );
    ( "hh explicitly removes its matched pair",
      `Quick,
      test_hh_explicitly_removes_matched_pair );
    ( "hh skips an unmatched candidate before a match",
      `Quick,
      test_hh_skips_unmatched_candidate_before_match );
    ( "hh applies two successive matches",
      `Quick,
      test_hh_applies_two_successive_matches );
    ( "hh simplifies its remainder after substitution",
      `Quick,
      test_hh_simplifies_remainder_after_substitution );
    ( "hh preserves yi candidate order",
      `Quick,
      test_hh_preserves_yi_candidate_order );
    ( "hh rejects a repeated y0-yi pair",
      `Quick,
      test_hh_rejects_repeated_y0_yi_pair );
    ( "hh rejects y0 with an unauthorized coefficient",
      `Quick,
      test_hh_rejects_y0_with_unauthorized_coefficient );
    ( "hh preserves the remainder without yi",
      `Quick,
      test_hh_preserves_remainder_without_yi );
    ( "reduction_algorithm reports malformed path sum",
      `Quick,
      test_reduction_algorithm_reports_malformed_path_sum );
  ]
(* let out_1_qubit = [ 0 ]
let out_2_qubits = [ 0; 1 ] *)
(* 
let normalise_path_var =
  [
    ( "0, x0 -> 0, x0",
      `Quick,
      test_normalise_path_var ~outputs1:out_1_qubit ~outputs2:out_1_qubit
        { phase = p0; ket = [| v 0 |]; path_var = [] }
        { phase = p0; ket = [| v 0 |]; path_var = [] } );
    ( "x0, x0 -> x0, x0",
      `Quick,
      let x0 = v 0 in
      let p_x0 = to_poly (Qubit x0) in
      test_normalise_path_var ~outputs1:out_1_qubit ~outputs2:out_1_qubit
        { phase = p_x0; ket = [| x0 |]; path_var = [] }
        { phase = p_x0; ket = [| x0 |]; path_var = [] } );
    ( "1/4, 1 -> 1/4, 1",
      `Quick,
      let p = to_poly (Scal div4) in
      test_normalise_path_var ~outputs1:out_1_qubit ~outputs2:out_1_qubit
        { phase = p; ket = [| Qubit.One |]; path_var = [] }
        { phase = p; ket = [| Qubit.One |]; path_var = [] } );
    ( "1/4 x0, x0 -> 1/4 x0, x0",
      `Quick,
      let x0 = v 0 in
      let p = to_poly (Prod (Scal div4, Qubit x0)) in
      test_normalise_path_var ~outputs1:out_1_qubit ~outputs2:out_1_qubit
        { phase = p; ket = [| x0 |]; path_var = [] }
        { phase = p; ket = [| x0 |]; path_var = [] } );
    ( "1/2 y0, y0 -> 1/2 y0, y0",
      `Quick,
      let y0 = v 1 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      test_normalise_path_var
        { phase = p; ket = [| y0 |]; path_var = [ 1 ] }
        { phase = p; ket = [| y0 |]; path_var = [ 1 ] } );
    ( "0, |x0,x1> -> 0, |x0,x1>",
      `Quick,
      let x0 = v 1 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| x0; x1 |]; path_var = [] }
        { phase = p0; ket = [| x0; x1 |]; path_var = [] } );
    ( "0, |x0,y0> -> 0, |x0,y0>",
      `Quick,
      let x0 = v 0 in
      let y0 = v 2 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| x0; y0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| x0; y0 |]; path_var = [ 2 ] } );
    ( "0, |y0,x0> -> 0, |y0,x0>",
      `Quick,
      let x0 = v 0 in
      let y0 = v 2 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| y0; x0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| y0; x0 |]; path_var = [ 2 ] } );
    ( "0, |y0,y1> -> 0, |y0,y1>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| y0; y1 |]; path_var = [ 2; 3 ] }
        { phase = p0; ket = [| y0; y1 |]; path_var = [ 2; 3 ] } );
    ( "0, |y0,y0> -> 0, |y0,y0>",
      `Quick,
      let y0 = v 2 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| y0; y0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| y0; y0 |]; path_var = [ 2 ] } );
    ( "0, |y1,y0> -> 0, |y0,y1>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p0; ket = [| y1; y0 |]; path_var = [ 2; 3 ] }
        { phase = p0; ket = [| y0; y1 |]; path_var = [ 2; 3 ] } );
    ( "1/2 y0, |y1,y0> -> 1/2 y0, |y0,y1>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      let p' = to_poly (Monome.Prod (Scal div2, Qubit y1)) in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p; ket = [| y1; y0 |]; path_var = [ 2; 3 ] }
        { phase = p'; ket = [| y0; y1 |]; path_var = [ 2; 3 ] } );
    ( "1/2 y1, |y1,y1> -> 1/2 y0, |y0,y0>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y1)) in
      let p' = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      test_normalise_path_var ~outputs1:out_2_qubits ~outputs2:out_2_qubits
        { phase = p; ket = [| y1; y1 |]; path_var = [ 3 ] }
        { phase = p'; ket = [| y0; y0 |]; path_var = [ 2 ] } );
  ] *)

let test_poly_normalize ?(debug = true) (input : Path_sum.t)
    (expect : Path_sum.t) () =
  if debug then
    printf "Primitives.test_normalise_path_var, expect =\n%s\n\n%!"
      (PSS.pretty expect);
  if debug then
    printf "Primitives.test_normalise_path_var, input =\n%s\n\n%!"
      (PSS.pretty input);
  (* This helper is used only for valid examples; malformed path sums have
     dedicated tests for the typed error below. *)
  let input_normalised =
    match Rules.Variable_replacement.poly_normalized ~debug input with
    | Ok output -> output
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  if debug then
    printf "Primitives.test_normalise_path_var, input_normalised =\n%s\n\n%!"
      (PSS.pretty input_normalised);
  let greet = Path_sum.equal ~debug input_normalised expect in
  let expect = true in
  check bool (sprintf "Primitives.test_normalise_path_var") expect greet

let test_poly_normalized_reports_malformed_path_sum () =
  let malformed_path_sum : Path_sum.t =
    {
      phase = to_poly (Qubit (Qubit.Var 2));
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match
    Rules.Variable_replacement.poly_normalized malformed_path_sum
  with
  | Error (Rules.MalformedPathSum _) -> check bool "malformed path sum" true true
  | Ok _ -> check bool "malformed path sum expected" true false

let poly_normalize =
  [
    ( "poly_normalized reports malformed path sum",
      `Quick,
      test_poly_normalized_reports_malformed_path_sum );
    ( "0, x0 -> 0, x0",
      `Quick,
      test_poly_normalize
        { phase = p0; ket = [| v 0 |]; path_var = [] }
        { phase = p0; ket = [| v 0 |]; path_var = [] } );
    ( "x0, x0 -> x0, x0",
      `Quick,
      let x0 = v 0 in
      let p_x0 = to_poly (Qubit x0) in
      test_poly_normalize
        { phase = p_x0; ket = [| x0 |]; path_var = [] }
        { phase = p_x0; ket = [| x0 |]; path_var = [] } );
    ( "1/4, 1 -> 1/4, 1",
      `Quick,
      let p = to_poly (Scal div4) in
      test_poly_normalize
        { phase = p; ket = [| Qubit.One |]; path_var = [] }
        { phase = p; ket = [| Qubit.One |]; path_var = [] } );
    ( "1/4 x0, x0 -> 1/4 x0, x0",
      `Quick,
      let x0 = v 0 in
      let p = to_poly (Prod (Scal div4, Qubit x0)) in
      test_poly_normalize
        { phase = p; ket = [| x0 |]; path_var = [] }
        { phase = p; ket = [| x0 |]; path_var = [] } );
    ( "1/2 y0, y0 -> 1/2 y0, y0",
      `Quick,
      let y0 = v 1 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      test_poly_normalize
        { phase = p; ket = [| y0 |]; path_var = [ 1 ] }
        { phase = p; ket = [| y0 |]; path_var = [ 1 ] } );
    ( "0, |x0,x1> -> 0, |x0,x1>",
      `Quick,
      let x0 = v 1 in
      test_poly_normalize
        { phase = p0; ket = [| x0; x1 |]; path_var = [] }
        { phase = p0; ket = [| x0; x1 |]; path_var = [] } );
    ( "0, |x0,y0> -> 0, |x0,y0>",
      `Quick,
      let x0 = v 0 in
      let y0 = v 2 in
      test_poly_normalize
        { phase = p0; ket = [| x0; y0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| x0; y0 |]; path_var = [ 2 ] } );
    ( "0, |y0,x0> -> 0, |y0,x0>",
      `Quick,
      let x0 = v 0 in
      let y0 = v 2 in
      test_poly_normalize
        { phase = p0; ket = [| y0; x0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| y0; x0 |]; path_var = [ 2 ] } );
    ( "0, |y0,y1> -> 0, |y0,y1>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      test_poly_normalize
        { phase = p0; ket = [| y0; y1 |]; path_var = [ 2; 3 ] }
        { phase = p0; ket = [| y0; y1 |]; path_var = [ 2; 3 ] } );
    ( "0, |y0,y0> -> 0, |y0,y0>",
      `Quick,
      let y0 = v 2 in
      test_poly_normalize
        { phase = p0; ket = [| y0; y0 |]; path_var = [ 2 ] }
        { phase = p0; ket = [| y0; y0 |]; path_var = [ 2 ] } );
    ( "0, |y1,y0> -> 0, |y1,y0>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      test_poly_normalize
        { phase = p0; ket = [| y1; y0 |]; path_var = [ 2; 3 ] }
        { phase = p0; ket = [| y1; y0 |]; path_var = [ 2; 3 ] } );
    ( "1/2 y0, |y1,y0> -> 1/2 y0, |y1,y0>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      test_poly_normalize
        { phase = p; ket = [| y1; y0 |]; path_var = [ 2; 3 ] }
        { phase = p; ket = [| y1; y0 |]; path_var = [ 2; 3 ] } );
    ( "1/2 x0y1, |y0,y1> -> 1/2 x0y0, |y1,y0>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      let p = to_poly (Monome.Prod (Scal div2, Prod (Qubit x0, Qubit y1))) in
      let p' = to_poly (Monome.Prod (Scal div2, Prod (Qubit x0, Qubit y0))) in
      test_poly_normalize
        { phase = p; ket = [| y0; y1 |]; path_var = [ 2; 3 ] }
        { phase = p'; ket = [| y1; y0 |]; path_var = [ 2; 3 ] } );
    ( "1/2 y1, |y1,y1> -> 1/2 y0, |y0,y0>",
      `Quick,
      let y0 = v 2 in
      let y1 = v 3 in
      let p = to_poly (Monome.Prod (Scal div2, Qubit y1)) in
      let p' = to_poly (Monome.Prod (Scal div2, Qubit y0)) in
      test_poly_normalize
        { phase = p; ket = [| y1; y1 |]; path_var = [ 3 ] }
        { phase = p'; ket = [| y0; y0 |]; path_var = [ 2 ] } );
  ]

let test_monome_to_scalar_monome ?(debug = true) (input : Monome.t)
    (expect : Q.t * Monome.t) () =
  let greet, expect =
    match Monome.monome_to_scalar_monome input with
    | Some (s, m) ->
        if debug then
          printf "Primitives.test_monome_to_scalar_monome, s = %s, m = %s\n"
            (Q.to_string s) (Monome.String.exact m);
        let s', m' = expect in
        if debug then
          printf "Primitives.test_monome_to_scalar_monome, s' = %s, m' = %s\n"
            (Q.to_string s') (Monome.String.exact m');
        let s_eq = Q.equal s s' in
        let m_eq = Monome.equal m m' in
        if debug then
          printf
            "Primitives.test_monome_to_scalar_monome, s_eq = %b, m_eq = %b\n"
            s_eq m_eq;
        (s_eq && m_eq, true)
    | None -> (false, false)
  in
  check bool (sprintf "Primitives.test_monome_to_scalar_monome") expect greet

let test_monome_equal_result_returns_true () =
  match
    Monome.equal_result (Monome.Qubit (Qubit.Var 0))
      (Monome.Qubit (Qubit.Var 0))
  with
  | Ok true -> check bool "equal monomes" true true
  | Ok false -> check bool "equal monomes expected" true false
  | Error _ -> check bool "well-formed comparison expected" true false

let test_monome_equal_result_returns_false () =
  match Monome.equal_result (Monome.Scal Q.zero) (Monome.Scal Q.one) with
  | Ok false -> check bool "different monomes" false false
  | Ok true -> check bool "different monomes expected" false true
  | Error _ -> check bool "well-formed comparison expected" true false

let test_monome_equal_result_reports_incompatible_widths () =
  match
    Monome.equal_result ~wq1:0 ~wq2:1 (Monome.Qubit Qubit.Zero)
      (Monome.Qubit Qubit.Zero)
  with
  | Error Monome.IncompatibleWidths -> check bool "incompatible widths" true true
  | Error Monome.IncompletePathVariableMap ->
      check bool "incompatible widths expected" true false
  | Ok _ -> check bool "incompatible widths expected" true false

let test_monome_equal_result_reports_incomplete_path_var_map () =
  let map_path_var1 = IntMap.singleton 1 0 in
  let map_path_var2 = IntMap.empty in
  match
    Monome.equal_result ~wq1:1 ~wq2:1 ~map_path_var1 ~map_path_var2
      (Monome.Qubit (Qubit.Var 1)) (Monome.Qubit (Qubit.Var 1))
  with
  | Error Monome.IncompletePathVariableMap ->
      check bool "incomplete path variable map" true true
  | Error Monome.IncompatibleWidths ->
      check bool "incomplete path variable map expected" true false
  | Ok _ -> check bool "incomplete path variable map expected" true false

let test_monome_of_qubit_to_result_returns_monome () =
  let check_ok name qubit expected_monome =
    match Monome.of_qubit_to_result qubit with
    | Ok monome ->
        check string name (Monome.String.exact expected_monome)
          (Monome.String.exact monome)
    | Error Monome.CannotConvertSumMod2 ->
        check bool "direct monome conversion expected" true false
  in
  check_ok "constant zero" Qubit.Zero (Monome.Scal Q.zero);
  check_ok "variable" (Qubit.Var 1) (Monome.Qubit (Qubit.Var 1));
  check_ok "product" (Qubit.Prod (Qubit.Var 1, Qubit.Var 2))
    (Monome.Prod
       (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2)))

let test_monome_of_qubit_to_result_reports_sum_mod2 () =
  match Monome.of_qubit_to_result (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2)) with
  | Error Monome.CannotConvertSumMod2 ->
      check bool "sum modulo 2 rejected" true true
  | Ok _ -> check bool "sum modulo 2 rejection expected" true false

let test_monome_to_qubit_result_returns_qubit () =
  let check_ok name monome expected_qubit =
    match Monome.to_qubit_result monome with
    | Ok qubit -> check string name (QS.exact expected_qubit) (QS.exact qubit)
    | Error (Monome.CannotConvertScalarToQubit _) ->
        check bool "qubit conversion expected" true false
  in
  check_ok "zero scalar" (Monome.Scal Q.zero) Qubit.Zero;
  check_ok "qubit monome" (Monome.Qubit (Qubit.Var 1)) (Qubit.Var 1);
  check_ok "product monome"
    (Monome.Prod
       (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2)))
    (Qubit.Prod (Qubit.Var 1, Qubit.Var 2))

let test_monome_to_qubit_result_reports_scalar () =
  match Monome.to_qubit_result (Monome.Scal (Q.of_int 2)) with
  | Error (Monome.CannotConvertScalarToQubit scalar) ->
      check string "invalid scalar" "2" (Q.to_string scalar)
  | Ok _ -> check bool "invalid scalar expected" true false

let test_monome_remove_result_returns_some () =
  match
    Monome.remove_result 1
      (Monome.Prod
         (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2)))
  with
  | Ok (Some output) ->
      check string "removed variable"
        (Monome.String.exact (Monome.Qubit (Qubit.Var 2)))
        (Monome.String.exact output)
  | Ok None -> check bool "removed variable expected" true false
  | Error Monome.CannotRemoveQubitSum ->
      check bool "product monome expected" true false

let test_monome_remove_result_returns_none () =
  match Monome.remove_result 3 (Monome.Qubit (Qubit.Var 1)) with
  | Ok None -> check bool "absent variable" true true
  | Ok (Some _) -> check bool "absent variable expected" true false
  | Error Monome.CannotRemoveQubitSum ->
      check bool "non-sum qubit expected" true false

let test_monome_remove_result_reports_qubit_sum () =
  match
    Monome.remove_result 1
      (Monome.Qubit (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2)))
  with
  | Error Monome.CannotRemoveQubitSum ->
      check bool "qubit sum rejected" true true
  | Ok _ -> check bool "qubit sum rejection expected" true false

let monome_equality =
  [
    ( "equal_result returns true",
      `Quick,
      test_monome_equal_result_returns_true );
    ( "equal_result returns false",
      `Quick,
      test_monome_equal_result_returns_false );
    ( "equal_result reports incompatible widths",
      `Quick,
      test_monome_equal_result_reports_incompatible_widths );
    ( "equal_result reports incomplete path variable map",
      `Quick,
      test_monome_equal_result_reports_incomplete_path_var_map );
    ( "of_qubit_to_result returns monome",
      `Quick,
      test_monome_of_qubit_to_result_returns_monome );
    ( "of_qubit_to_result reports sum modulo 2",
      `Quick,
      test_monome_of_qubit_to_result_reports_sum_mod2 );
    ( "to_qubit_result returns qubit",
      `Quick,
      test_monome_to_qubit_result_returns_qubit );
    ( "to_qubit_result reports scalar",
      `Quick,
      test_monome_to_qubit_result_reports_scalar );
    ( "remove_result returns some",
      `Quick,
      test_monome_remove_result_returns_some );
    ( "remove_result returns none",
      `Quick,
      test_monome_remove_result_returns_none );
    ( "remove_result reports qubit sum",
      `Quick,
      test_monome_remove_result_reports_qubit_sum );
  ]

let check_simplified_monome name input expected =
  check string name (Monome.String.exact expected)
    (Monome.String.exact (Monome.simplify input))

let test_monome_simplify_normalizes_negative_scalar () =
  (* -5/4 = 3/4 modulo 1. *)
  check_simplified_monome "negative scalar"
    (Monome.Scal (Q.make (Z.of_int (-5)) (Z.of_int 4)))
    (Monome.Scal (Q.make (Z.of_int 3) (Z.of_int 4)))

let test_monome_simplify_multiplies_scalars_before_normalizing () =
  (* (-5/4)*(1/2) = -5/8 = 3/8 modulo 1. Normalizing -5/4 first
     would incorrectly produce 7/8 with the current implementation. *)
  check_simplified_monome "scalar product before normalization"
    (Monome.Prod
       ( Monome.Scal (Q.make (Z.of_int (-5)) (Z.of_int 4)),
         Monome.Scal (Q.make (Z.of_int 1) (Z.of_int 2)) ))
    (Monome.Scal (Q.make (Z.of_int 3) (Z.of_int 8)))

let test_monome_simplify_normalizes_complete_qubit_coefficient () =
  (* (-9/4)*(1/2)*x0 = (-9/8)*x0 = (7/8)*x0 modulo 1. This
     prevents normalizing the first scalar before all factors are combined. *)
  check_simplified_monome "complete qubit coefficient"
    (Monome.Prod
       ( Monome.Scal (Q.make (Z.of_int (-9)) (Z.of_int 4)),
         Monome.Prod
           ( Monome.Scal (Q.make (Z.of_int 1) (Z.of_int 2)),
             Monome.Qubit (Qubit.Var 0) ) ))
    (Monome.Prod
       ( Monome.Scal (Q.make (Z.of_int 7) (Z.of_int 8)),
         Monome.Qubit (Qubit.Var 0) ))

let test_monome_simplify_combines_scalars_exposed_by_identity () =
  (* Input: (-9/4) * (1 * 1/2). Simplifying the identity exposes two scalar
     factors, which must be multiplied before normalization: -9/8 = 7/8. *)
  check_simplified_monome "scalars exposed by identity"
    (Monome.Prod
       ( Monome.Scal (Q.make (Z.of_int (-9)) (Z.of_int 4)),
         Monome.Prod
           ( Monome.Qubit Qubit.One,
             Monome.Scal (Q.make (Z.of_int 1) (Z.of_int 2)) ) ))
    (Monome.Scal (Q.make (Z.of_int 7) (Z.of_int 8)))

let monome_simplification =
  [
    ( "normalizes a negative scalar",
      `Quick,
      test_monome_simplify_normalizes_negative_scalar );
    ( "multiplies scalars before normalizing",
      `Quick,
      test_monome_simplify_multiplies_scalars_before_normalizing );
    ( "normalizes a complete qubit coefficient",
      `Quick,
      test_monome_simplify_normalizes_complete_qubit_coefficient );
    ( "combines scalars exposed by identity",
      `Quick,
      test_monome_simplify_combines_scalars_exposed_by_identity );
  ]

let monome_to_scalar_monome =
  [
    ( "1/2 x0 -> 1/2, x0",
      `Quick,
      test_monome_to_scalar_monome
        (Monome.Prod (Scal div2, Qubit (Var 0)))
        (div2, Qubit (Var 0)) );
    ( "-1/8 x5 -> -1/8, x0",
      `Quick,
      test_monome_to_scalar_monome
        (Monome.Prod (Scal divm8, Qubit (Var 6)))
        (divm8, Qubit (Var 6)) );
    ( "-1/8 x0x2x3 -> -1/8, x0x2x3",
      `Quick,
      test_monome_to_scalar_monome
        (Monome.Prod
           ( Scal divm8,
             Prod (Qubit (Var 0), Prod (Qubit (Var 2), Qubit (Var 3))) ))
        (divm8, Prod (Qubit (Var 0), Prod (Qubit (Var 2), Qubit (Var 3)))) );
    ( "x0 -> None",
      `Quick,
      test_monome_to_scalar_monome (Qubit (Var 0))
        (Q.of_int 0, Monome.Scal (Q.of_int 0)) );
  ]

let test_poly_equal_result_returns_true () =
  match Poly.equal_result Poly.zero Poly.zero with
  | Ok true -> check bool "equal polynomials" true true
  | Ok false -> check bool "equal polynomials expected" true false
  | Error _ -> check bool "well-formed comparison expected" true false

let test_poly_equal_result_returns_false () =
  match Poly.equal_result Poly.zero Poly.one with
  | Ok false -> check bool "different polynomials" false false
  | Ok true -> check bool "different polynomials expected" false true
  | Error _ -> check bool "well-formed comparison expected" true false

let test_poly_equal_result_reports_incompatible_widths () =
  match
    Poly.equal_result ~wq1:0 ~wq2:1
      (to_poly (Monome.Qubit Qubit.Zero))
      (to_poly (Monome.Qubit Qubit.Zero))
  with
  | Error Poly.IncompatibleWidths -> check bool "incompatible widths" true true
  | Error Poly.IncompletePathVariableMap ->
      check bool "incompatible widths expected" true false
  | Ok _ -> check bool "incompatible widths expected" true false

let test_poly_equal_result_reports_incomplete_path_var_map () =
  let map_path_var1 = IntMap.singleton 1 0 in
  let map_path_var2 = IntMap.empty in
  match
    Poly.equal_result ~wq1:1 ~wq2:1 ~map_path_var1 ~map_path_var2
      (to_poly (Monome.Qubit (Qubit.Var 1)))
      (to_poly (Monome.Qubit (Qubit.Var 1)))
  with
  | Error Poly.IncompletePathVariableMap ->
      check bool "incomplete path variable map" true true
  | Error Poly.IncompatibleWidths ->
      check bool "incomplete path variable map expected" true false
  | Ok _ -> check bool "incomplete path variable map expected" true false

let poly_equality =
  [
    ( "equal_result returns true",
      `Quick,
      test_poly_equal_result_returns_true );
    ( "equal_result returns false",
      `Quick,
      test_poly_equal_result_returns_false );
    ( "equal_result reports incompatible widths",
      `Quick,
      test_poly_equal_result_reports_incompatible_widths );
    ( "equal_result reports incomplete path variable map",
      `Quick,
      test_poly_equal_result_reports_incomplete_path_var_map );
  ]

let test_poly_to_qubit_result_returns_qubit () =
  let check_ok name poly expected_qubit =
    match Poly.to_qubit_result poly with
    | Ok qubit -> check string name (QS.exact expected_qubit) (QS.exact qubit)
    | Error (Poly.CannotConvertScalarMonomeToQubit _) ->
        check bool "qubit conversion expected" true false
  in
  (* An empty polynomial represents no parity term, so it converts to Zero. *)
  check_ok "empty polynomial" Poly.empty Qubit.Zero;
  (* Poly.to_qubit folds monomes into a SumMod2 accumulator initialized to Zero. *)
  check_ok "single qubit monome"
    (Monome.Qubit (Qubit.Var 1) +++ Poly.empty)
    (Qubit.SumMod2 (Qubit.Var 1, Qubit.Zero))

let test_poly_to_qubit_result_reports_scalar_monome () =
  (* A scalar monome is a phase term, not a qubit expression. *)
  match Poly.to_qubit_result (Monome.Scal (Q.of_int 2) +++ Poly.empty) with
  | Error (Poly.CannotConvertScalarMonomeToQubit scalar) ->
      check string "invalid scalar" "2" (Q.to_string scalar)
  | Ok _ -> check bool "invalid scalar expected" true false

let test_poly_of_qubit_result_returns_poly () =
  let check_ok name qubit expected_poly =
    match Poly.of_qubit_result qubit Q.one with
    | Ok poly -> check bool name true (Poly.equal expected_poly poly)
    | Error Poly.UnformattedQubitSum ->
        check bool "formatted qubit expected" true false
  in
  (* For scalar 1, lifting x1 ++ x2 gives x1 + x2 - x1.x2:
     coef_lift(1) = -1, hence the product term below has coefficient -1. *)
  let expected_sum =
    Monome.Qubit (Qubit.Var 1)
    +++ (Monome.Qubit (Qubit.Var 2)
        +++ (Monome.Prod
               ( Monome.Scal (Q.of_int (-1)),
                 Monome.Prod
                   (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2)) )
            +++ Poly.empty))
  in
  (* A single variable is already directly convertible to one monome. *)
  check_ok "single variable" (Qubit.Var 1)
    (Monome.Qubit (Qubit.Var 1) +++ Poly.empty);
  (* This is the accepted binary SumMod2 shape: SumMod2 (x1, x2). *)
  check_ok "formatted sum" (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2))
    expected_sum

let test_poly_of_qubit_result_reports_unformatted_sum () =
  (* The current implementation rejects a nested sum on the left. Such qubits
     must be normalized before calling Poly.of_qubit_result. *)
  let unformatted_sum =
    Qubit.SumMod2 (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2), Qubit.Var 3)
  in
  match Poly.of_qubit_result unformatted_sum Q.one with
  | Error Poly.UnformattedQubitSum ->
      check bool "unformatted qubit sum rejected" true true
  | Ok _ -> check bool "unformatted qubit sum rejection expected" true false

let test_poly_of_qubit_2_pi_result_returns_poly () =
  (* The 2*pi shortcut drops the product correction term. For x1 ++ x2, the
     expected polynomial is only x1 + x2. *)
  let expected_sum =
    Monome.Qubit (Qubit.Var 1)
    +++ (Monome.Qubit (Qubit.Var 2) +++ Poly.empty)
  in
  match Poly.of_qubit_2_pi_result (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2)) with
  | Ok poly -> check bool "2*pi formatted sum" true (Poly.equal expected_sum poly)
  | Error Poly.UnformattedQubitSum ->
      check bool "formatted qubit expected" true false

let test_poly_of_qubit_2_pi_result_reports_unformatted_sum () =
  (* Same format restriction as Poly.of_qubit_result: a nested left sum must be
     normalized before this conversion. *)
  let unformatted_sum =
    Qubit.SumMod2 (Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2), Qubit.Var 3)
  in
  match Poly.of_qubit_2_pi_result unformatted_sum with
  | Error Poly.UnformattedQubitSum ->
      check bool "unformatted qubit sum rejected" true true
  | Ok _ -> check bool "unformatted qubit sum rejection expected" true false

let poly_conversion =
  [
    ( "of_qubit_result returns poly",
      `Quick,
      test_poly_of_qubit_result_returns_poly );
    ( "of_qubit_result reports unformatted sum",
      `Quick,
      test_poly_of_qubit_result_reports_unformatted_sum );
    ( "of_qubit_2_pi_result returns poly",
      `Quick,
      test_poly_of_qubit_2_pi_result_returns_poly );
    ( "of_qubit_2_pi_result reports unformatted sum",
      `Quick,
      test_poly_of_qubit_2_pi_result_reports_unformatted_sum );
    ( "to_qubit_result returns qubit",
      `Quick,
      test_poly_to_qubit_result_returns_qubit );
    ( "to_qubit_result reports scalar monome",
      `Quick,
      test_poly_to_qubit_result_reports_scalar_monome );
  ]

let test_poly_distribution_result_returns_poly () =
  (* Distribution multiplies one monome by each monome of the right polynomial.
     Here both polynomials contain a single qubit monome, so the result is
     exactly x1.x2. *)
  let left = Monome.Qubit (Qubit.Var 1) in
  let right = Monome.Qubit (Qubit.Var 2) +++ Poly.empty in
  let expected =
    Monome.Prod (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2))
    +++ Poly.empty
  in
  match Poly.distribution_result left right with
  | Ok poly -> check bool "distributed product" true (Poly.equal expected poly)
  | Error Poly.UnformattedDistributionMonome ->
      check bool "formatted distribution monome expected" true false

let test_poly_distribution_result_reports_unformatted_monome () =
  (* Scalars are expected on the left of Prod. The old distribution function
     raised Failure on this shape; the typed version reports it explicitly. *)
  let unformatted_right =
    Monome.Prod (Monome.Qubit (Qubit.Var 1), Monome.Scal (Q.of_int 2))
    +++ Poly.empty
  in
  match
    Poly.distribution_result (Monome.Qubit (Qubit.Var 0)) unformatted_right
  with
  | Error Poly.UnformattedDistributionMonome ->
      check bool "unformatted distribution monome rejected" true true
  | Ok _ -> check bool "unformatted distribution monome expected" true false

let test_poly_lift_half_coefficient_returns_input () =
  (* With a 1/2 phase coefficient, the correction for lifting x0 xor x1 is an
     integer phase. It vanishes modulo one, so no x0.x1 term is needed. *)
  let input =
    Monome.Qubit (Qubit.Var 0)
    +++ (Monome.Qubit (Qubit.Var 1) +++ Poly.empty)
  in
  let output = Poly.lift input div2 in
  check bool "half-coefficient lift" true (Poly.equal input output)

let poly_algebra =
  [
    ( "distribution_result returns poly",
      `Quick,
      test_poly_distribution_result_returns_poly );
    ( "distribution_result reports unformatted monome",
      `Quick,
      test_poly_distribution_result_reports_unformatted_monome );
    ( "lift with half coefficient adds no cross term",
      `Quick,
      test_poly_lift_half_coefficient_returns_input );
  ]

let test_path_sum_equal_result_returns_true () =
  let path_sum : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  match Path_sum.equal_result path_sum path_sum with
  | Ok true -> check bool "equal path sums" true true
  | Ok false -> check bool "equal path sums expected" true false
  | Error Path_sum.DifferentOutputLengths ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.InvalidOutputIndex ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.IncompatiblePhaseWidths ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.IncompletePhasePathVariableMap ->
      check bool "well-formed comparison expected" true false

let test_path_sum_equal_result_returns_false () =
  let path_sum1 : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Zero |]; path_var = [] }
  in
  let path_sum2 : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.One |]; path_var = [] }
  in
  match Path_sum.equal_result path_sum1 path_sum2 with
  | Ok false -> check bool "different path sums" false false
  | Ok true -> check bool "different path sums expected" false true
  | Error Path_sum.DifferentOutputLengths ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.InvalidOutputIndex ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.IncompatiblePhaseWidths ->
      check bool "well-formed comparison expected" true false
  | Error Path_sum.IncompletePhasePathVariableMap ->
      check bool "well-formed comparison expected" true false

let test_path_sum_equal_result_reports_different_output_lengths () =
  let path_sum : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  match Path_sum.equal_result ~outputs1:[ 0 ] ~outputs2:[] path_sum path_sum with
  | Error Path_sum.DifferentOutputLengths ->
      check bool "different output lengths" true true
  | Error Path_sum.InvalidOutputIndex ->
      check bool "different output lengths expected" true false
  | Error Path_sum.IncompatiblePhaseWidths ->
      check bool "different output lengths expected" true false
  | Error Path_sum.IncompletePhasePathVariableMap ->
      check bool "different output lengths expected" true false
  | Ok _ -> check bool "different output lengths expected" true false

let test_path_sum_equal_result_reports_invalid_output_index () =
  let path_sum : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  match
    Path_sum.equal_result ~outputs1:[ 1 ] ~outputs2:[ 0 ] path_sum path_sum
  with
  | Error Path_sum.InvalidOutputIndex ->
      check bool "invalid output index" true true
  | Error Path_sum.DifferentOutputLengths ->
      check bool "invalid output index expected" true false
  | Error Path_sum.IncompatiblePhaseWidths ->
      check bool "invalid output index expected" true false
  | Error Path_sum.IncompletePhasePathVariableMap ->
      check bool "invalid output index expected" true false
  | Ok _ -> check bool "invalid output index expected" true false

let test_path_sum_equal_result_rejects_narrower_first_without_outputs () =
  (* Without explicit outputs, equality compares the complete kets. Their
     widths must therefore be equal, independently of argument order. *)
  let narrow : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  let wide : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| Qubit.Var 0; Qubit.Var 1 |];
      path_var = [];
    }
  in
  match Path_sum.equal_result narrow wide with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "different complete ket widths accepted"
  | Error _ -> Alcotest.fail "different complete ket widths should return false"

let test_path_sum_equal_result_rejects_wider_first_without_outputs () =
  (* Check the reverse order separately to prevent an asymmetric result. *)
  let wide : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| Qubit.Var 0; Qubit.Var 1 |];
      path_var = [];
    }
  in
  let narrow : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  match Path_sum.equal_result wide narrow with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "different complete ket widths accepted"
  | Error _ -> Alcotest.fail "different complete ket widths should return false"

let test_path_sum_equal_result_accepts_different_widths_with_outputs () =
  (* Explicit output lists compare only the selected components, so the two
     complete kets may have different widths. *)
  let narrow : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  let wide : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| Qubit.Var 0; Qubit.Var 1 |];
      path_var = [];
    }
  in
  match
    Path_sum.equal_result ~outputs1:[ 0 ] ~outputs2:[ 0 ] narrow wide
  with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "equal selected outputs rejected"
  | Error _ -> Alcotest.fail "different ket widths with explicit outputs rejected"

let test_path_sum_equal_result_uses_ket_path_var_mapping_in_phase () =
  (* ps1: phase = 1/2 y0, ket = |y0,y1>
     ps2: phase = 1/2 y1, ket = |y1,y0>
     The ket establishes y0 <-> y1, and the phase must use the same renaming. *)
  let path_sum1 : Path_sum.t =
    {
      phase =
        to_poly
          (Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 2)));
      ket = [| Qubit.Var 2; Qubit.Var 3 |];
      path_var = [ 2; 3 ];
    }
  in
  let path_sum2 : Path_sum.t =
    {
      phase =
        to_poly
          (Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 3)));
      ket = [| Qubit.Var 3; Qubit.Var 2 |];
      path_var = [ 2; 3 ];
    }
  in
  match Path_sum.equal_result path_sum1 path_sum2 with
  | Ok true -> ()
  | Ok false -> Alcotest.fail "consistent path-variable renaming rejected"
  | Error _ -> Alcotest.fail "well-formed path-sum comparison expected"

let test_path_sum_equal_result_rejects_phase_incompatible_with_ket_mapping () =
  (* The kets map the first path variable y0 to y1. The second phase still
     depends on its own y0, so the phases differ without either map being
     malformed. *)
  let phase =
    Monome.Prod
      ( Monome.Scal div2,
        Monome.Prod
          (Monome.Qubit (Qubit.Var 0), Monome.Qubit (Qubit.Var 1)) )
    +++ Poly.empty
  in
  let path_sum1 : Path_sum.t =
    { phase; ket = [| Qubit.Var 1 |]; path_var = [ 1 ] }
  in
  let path_sum2 : Path_sum.t =
    { phase; ket = [| Qubit.Var 2 |]; path_var = [ 1; 2 ] }
  in
  match Path_sum.equal_result path_sum1 path_sum2 with
  | Ok false -> ()
  | Ok true -> Alcotest.fail "phases incompatible with ket mapping accepted"
  | Error _ -> Alcotest.fail "well-formed unequal path sums reported malformed"

let path_sum_equality =
  [
    ( "equal_result returns true",
      `Quick,
      test_path_sum_equal_result_returns_true );
    ( "equal_result returns false",
      `Quick,
      test_path_sum_equal_result_returns_false );
    ( "equal_result reports different output lengths",
      `Quick,
      test_path_sum_equal_result_reports_different_output_lengths );
    ( "equal_result reports invalid output index",
      `Quick,
      test_path_sum_equal_result_reports_invalid_output_index );
    ( "equal_result rejects narrower ket first without outputs",
      `Quick,
      test_path_sum_equal_result_rejects_narrower_first_without_outputs );
    ( "equal_result rejects wider ket first without outputs",
      `Quick,
      test_path_sum_equal_result_rejects_wider_first_without_outputs );
    ( "equal_result accepts different widths with outputs",
      `Quick,
      test_path_sum_equal_result_accepts_different_widths_with_outputs );
    ( "equal_result applies ket path-variable mapping to phase",
      `Quick,
      test_path_sum_equal_result_uses_ket_path_var_mapping_in_phase );
    ( "equal_result rejects phase incompatible with ket mapping",
      `Quick,
      test_path_sum_equal_result_rejects_phase_incompatible_with_ket_mapping );
  ]

let test_path_sum_ofSize_init_result_returns_path_sum () =
  let check_ok name width inits_0 expected_output =
    match Path_sum.ofSize_init_result width inits_0 with
    | Ok output ->
        check string name (PSS.exact expected_output) (PSS.exact output)
    | Error Path_sum.InvalidWidth ->
        check bool "valid width expected" true false
    | Error Path_sum.InvalidInitIndex ->
        check bool "valid initialization indices expected" true false
  in
  check_ok "no initialized qubit" 2 []
    { phase = Poly.zero; ket = [| Qubit.Var 0; Qubit.Var 1 |]; path_var = [] };
  check_ok "one initialized qubit" 2 [ 0 ]
    { phase = Poly.zero; ket = [| Qubit.Zero; Qubit.Var 0 |]; path_var = [] };
  check_ok "several initialized qubits" 3 [ 0; 2 ]
    {
      phase = Poly.zero;
      ket = [| Qubit.Zero; Qubit.Var 0; Qubit.Zero |];
      path_var = [];
    };
  check_ok "zero width" 0 [] { phase = Poly.zero; ket = [||]; path_var = [] }

let test_path_sum_ofSize_init_result_reports_invalid_width () =
  match Path_sum.ofSize_init_result (-1) [] with
  | Error Path_sum.InvalidWidth -> check bool "invalid width" true true
  | Error Path_sum.InvalidInitIndex ->
      check bool "invalid width expected" true false
  | Ok _ -> check bool "invalid width expected" true false

let test_path_sum_ofSize_init_result_reports_invalid_init_index () =
  match Path_sum.ofSize_init_result 1 [ 1 ] with
  | Error Path_sum.InvalidInitIndex ->
      check bool "invalid initialization index" true true
  | Error Path_sum.InvalidWidth ->
      check bool "invalid initialization index expected" true false
  | Ok _ -> check bool "invalid initialization index expected" true false

let path_sum_initialization =
  [
    ( "ofSize_init_result returns path sum",
      `Quick,
      test_path_sum_ofSize_init_result_returns_path_sum );
    ( "ofSize_init_result reports invalid width",
      `Quick,
      test_path_sum_ofSize_init_result_reports_invalid_width );
    ( "ofSize_init_result reports invalid init index",
      `Quick,
      test_path_sum_ofSize_init_result_reports_invalid_init_index );
  ]

let test_path_sum_substitute_result_returns_path_sum () =
  let check_ok name ?(except_path_var = false) input variable replacement
      expected_output =
    match
      Path_sum.substitute_result ~except_path_var input variable replacement
    with
    | Ok output ->
        check string name (PSS.exact expected_output) (PSS.exact output)
    | Error Path_sum.CannotSubstitutePathVariable ->
        check bool "substitutable variable expected" true false
  in
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 1; Qubit.Var 2 |]; path_var = [ 2 ] }
  in
  (* Var 1 is not declared as a path variable, so it may be replaced. *)
  check_ok "substituted free variable" input 1 Qubit.One
    { phase = Poly.zero; ket = [| Qubit.One; Qubit.Var 2 |]; path_var = [ 2 ] };
  (* With except_path_var=true, declared path variables are left untouched. *)
  check_ok "protected path variable" ~except_path_var:true input 2 Qubit.One
    input

let test_path_sum_substitute_result_reports_path_var_substitution () =
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 1 |]; path_var = [ 1 ] }
  in
  (* Without except_path_var=true, substituting a path variable is rejected. *)
  match Path_sum.substitute_result input 1 Qubit.One with
  | Error Path_sum.CannotSubstitutePathVariable ->
      check bool "path variable substitution rejected" true true
  | Ok _ -> check bool "path variable substitution rejection expected" true false

let path_sum_substitution =
  [
    ( "substitute_result returns path sum",
      `Quick,
      test_path_sum_substitute_result_returns_path_sum );
    ( "substitute_result reports path variable substitution",
      `Quick,
      test_path_sum_substitute_result_reports_path_var_substitution );
  ]

let test_lift_poly ?(debug = true) (p : Poly.t) (expect : Poly.t) (wq : int) ()
    =
  if debug then printf "Primitives.test_lift_poly, p = %s\n%!" (PS.pretty p wq);
  if debug then
    printf "Primitives.test_lift_poly, expect = %s\n%!" (PS.pretty expect wq);
  let expect = Poly.simplify expect in
  let greet = Poly.simplify (Poly.lift_poly ~debug (Poly.simplify p)) in
  if debug then
    printf "Primitives.test_lift_poly, greet = %s\n\n%!" (PS.pretty greet wq);
  let greet = Poly.equal greet expect in
  let expect = true in
  check bool (sprintf "Primitives.test_lift_poly") expect greet

let mx0x1 = Monome.Prod (Scal div4, x0x1)

let poly0 s =
  Prod (Scal s, Qubit x0)
  +++ (Prod (Scal s, Qubit x1)
      +++ to_poly (Prod (Scal (Q.mul minus_two s), x0x1)))

let poly0' s = to_poly (Monome.Prod (Scal s, Qubit (SumMod2 (Var 0, Var 1))))

let poly1 s =
  Prod (Scal s, Qubit x0)
  +++ (Prod (Scal s, Qubit x1)
      +++ (Prod (Scal s, Qubit x2)
          +++ (Prod (Scal (Q.mul s minus_two), x0x1)
              +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                  +++ (Prod (Scal (Q.mul s minus_two), x0x2) +++ Poly.empty))))
      )

let poly1' s = to_poly (Monome.Prod (Scal s, Qubit (x0 ++ (x1 ++ x2))))

let poly2 s =
  Poly.insert
    (Prod (Scal s, Qubit x0))
    (Poly.insert (Prod (Scal s, Qubit x1)) (to_poly mx0x1))

let poly2' s = to_poly (Monome.Prod (Scal s, Qubit (SumMod2 (Var 0, Var 1))))

let lift_poly =
  [
    ( "1/2 -> 1/2",
      `Quick,
      let s = div2 in
      test_lift_poly (to_poly (Monome.Scal s)) (to_poly (Scal s)) 1 );
    ( "1/2 0 -> 0",
      `Quick,
      test_lift_poly (to_poly (Monome.Scal Q.zero)) Poly.zero 1 );
    ( "1/2 [0] -> 0",
      `Quick,
      let s = div2 in
      test_lift_poly (to_poly (Prod (Scal s, Qubit Qubit.Zero))) Poly.zero 1 );
    ( "1/2 x0 -> 1/2 x0",
      `Quick,
      let s = div2 in
      test_lift_poly
        (to_poly (Prod (Scal s, Qubit x0)))
        (to_poly (Prod (Scal s, Qubit x0)))
        1 );
    ( "1/2 x0 ++ x1 -> 1/2 x0 + 1/2 x1",
      `Quick,
      let s = div2 in
      test_lift_poly (poly0' s) (poly0 s) 2 );
    ( "-1/8, x0 ++ x1 -> 7/8 x0 + 7/8 x1 + 1/4 x0x1",
      `Quick,
      let s = divm8 in
      test_lift_poly (poly2' s) (poly2 s) 2 );
    ( "1/8, x0 ++ x1x2 -> 1/8 x0 + 1/8 x1x2 - 1/4 x0x1x2",
      `Quick,
      let s = div8 in
      let s' = divm4 in
      let x =
        to_poly
          (Monome.Prod (Scal s, Qubit (SumMod2 (x0, Monome.to_qubit x1x2))))
      in
      test_lift_poly x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (Poly.insert
              (Prod (Scal s, x1x2))
              (to_poly (Prod (Scal s', x0x1x2)))))
        3 );
    ( "1/4, x0++x1++x2 -> 1/4x0+1/4x1+1/4x2 -1/2x0x1-1/2x0x2-1/2x1x2",
      `Quick,
      let s = div4 in
      test_lift_poly (poly1' s) (poly1 s) 3 );
    ( "1/8, x0++x1++x2 -> 1/8x0+1/8x1+1/8x2 -1/4x0x1-1/4x0x2-1/4x1x2 +1/2x0x1x2",
      `Quick,
      let s = div8 in
      let x =
        to_poly (Monome.Prod (Scal s, Qubit (SumMod2 (x0, SumMod2 (x1, x2)))))
      in
      test_lift_poly x
        (Prod (Scal s, Qubit x0)
        +++ (Prod (Scal s, Qubit x1)
            +++ (Prod (Scal s, Qubit x2)
                +++ (Prod (Scal (Q.mul s minus_two), x0x1)
                    +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                        +++ (Prod (Scal (Q.mul s minus_two), x0x2)
                            +++ (Prod (Scal (Q.mul s four), x0x1x2)
                                +++ Poly.empty)))))))
        3 );
    ( "1/4 lift (p1 + p1) = 1/4 lift p1 + 1/4 lift p1",
      `Quick,
      let s = div4 in
      test_lift_poly
        (Poly.merge (poly1' s) (poly1' s))
        (Poly.merge (poly1 s) (poly1 s))
        3 );
    ( "1/4 lift (p0 + p0) = 1/4 lift p0 + 1/4 lift p0",
      `Quick,
      let s = div4 in
      test_lift_poly
        (Poly.merge (poly0' s) (poly0' s))
        (Poly.merge (poly0 s) (poly0 s))
        3 );
    ( "1/8 lift (p0 + p0) = 1/8 lift p0 + 1/8 lift p0",
      `Quick,
      let s = div8 in
      let p = Poly.merge (poly0' s) (poly0' s) in
      let expect = Poly.merge (poly0 s) (poly0 s) in
      test_lift_poly p expect 3 );
    ( "lift (1/2p0 + 1/4p0 + 1/8p0) = 1/2 lift p0 + 1/4 lift p0 + 18 lift p0",
      `Quick,
      let s0 = div2 in
      let s1 = div4 in
      let s2 = divm8 in
      let p = Poly.merge (poly0' s0) (Poly.merge (poly0' s1) (poly0' s2)) in
      let expect = Poly.merge (poly0 s0) (Poly.merge (poly0 s1) (poly0 s2)) in
      test_lift_poly p expect 3 );
  ]

let test_lift_monome ?(debug = true) (m : Monome.t) (expect : Poly.t) (wq : int)
    () =
  if debug then
    printf "Primitives.test_lift_monome, m = %s\n%!" (Monome.String.pretty m wq);
  let expect = Poly.simplify expect in
  if debug then
    printf "Primitives.test_lift_monome, expect = %s\n%!"
      (Poly.String.pretty expect wq);
  let greet = Poly.lift_monome ~debug m in
  if debug then
    printf "Primitives.test_lift_monome, greet = %s\n\n%!"
      (Poly.String.pretty greet wq);
  let greet = Poly.equal greet expect in
  let expect = true in
  check bool (sprintf "Primitives.test_lift_monome") expect greet

let lift_monome =
  [
    ( "1/2 -> 1/2",
      `Quick,
      let s = div2 in
      test_lift_monome (Monome.Scal s) (to_poly (Scal s)) 1 );
    ("1/2 0 -> 0", `Quick, test_lift_monome (Monome.Scal Q.zero) Poly.zero 1);
    ( "1/2 [0] -> 0",
      `Quick,
      let s = div2 in
      test_lift_monome (Prod (Scal s, Qubit Qubit.Zero)) Poly.zero 1 );
    ( "1/2 x0 -> 1/2 x0",
      `Quick,
      let s = div2 in
      test_lift_monome
        (Prod (Scal s, Qubit x0))
        (to_poly (Prod (Scal s, Qubit x0)))
        1 );
    ( "1/2 x0 ++ x1 -> 1/2 x0 + 1/2 x1",
      `Quick,
      let s = div2 in
      let x = Monome.Prod (Scal s, Qubit (SumMod2 (Var 0, Var 1))) in
      test_lift_monome x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (to_poly (Prod (Scal s, Qubit x1))))
        2 );
    ( "-1/8, x0 ++ x1 -> 7/8 x0 + 7/8 x1 + 1/4 x0x1",
      `Quick,
      let s = divm8 in
      let x = Monome.Prod (Scal s, Qubit (SumMod2 (Var 0, Var 1))) in
      let mx0x1 = Monome.Prod (Scal div4, x0x1) in
      test_lift_monome x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (Poly.insert (Prod (Scal s, Qubit x1)) (to_poly mx0x1)))
        2 );
    ( "1/8, x0 ++ x1x2 -> 1/8 x0 + 1/8 x1x2 - 1/4 x0x1x2",
      `Quick,
      let s = div8 in
      let s' = divm4 in
      let x =
        Monome.Prod (Scal s, Qubit (SumMod2 (x0, Monome.to_qubit x1x2)))
      in
      test_lift_monome x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (Poly.insert
              (Prod (Scal s, x1x2))
              (to_poly (Prod (Scal s', x0x1x2)))))
        3 );
    ( "1/4, x0++x1++x2 -> 1/4x0+1/4x1+1/4x2 -1/2x0x1-1/2x0x2-1/2x1x2",
      `Quick,
      let s = div4 in
      let x = Monome.Prod (Scal s, Qubit (SumMod2 (x0, SumMod2 (x1, x2)))) in
      test_lift_monome x
        (Prod (Scal s, Qubit x0)
        +++ (Prod (Scal s, Qubit x1)
            +++ (Prod (Scal s, Qubit x2)
                +++ (Prod (Scal (Q.mul s minus_two), x0x1)
                    +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                        +++ (Prod (Scal (Q.mul s minus_two), x0x2)
                            +++ Poly.empty))))))
        3 );
    ( "1/8, x0++x1++x2 -> 1/8x0+1/8x1+1/8x2 -1/4x0x1-1/4x0x2-1/4x1x2 +1/2x0x1x2",
      `Quick,
      let s = div8 in
      let x = Monome.Prod (Scal s, Qubit (SumMod2 (x0, SumMod2 (x1, x2)))) in
      test_lift_monome x
        (Prod (Scal s, Qubit x0)
        +++ (Prod (Scal s, Qubit x1)
            +++ (Prod (Scal s, Qubit x2)
                +++ (Prod (Scal (Q.mul s minus_two), x0x1)
                    +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                        +++ (Prod (Scal (Q.mul s minus_two), x0x2)
                            +++ (Prod (Scal (Q.mul s four), x0x1x2)
                                +++ Poly.empty)))))))
        3 );
  ]

let test_lift_qubit ?(debug = true) (s : Q.t) (m : Monome.t) (expect : Poly.t)
    (wq : int) () =
  if debug then
    printf "Primitives.test_lift_qubit, m = %s\n%!" (Monome.String.pretty m wq);
  let expect = Poly.simplify expect in
  if debug then
    printf "Primitives.test_lift_qubit, expect = %s\n%!"
      (Poly.String.pretty expect wq);
  let greet = Poly.lift_qubit ~debug s m in
  if debug then
    printf "Primitives.test_lift_qubit, greet = %s\n\n%!"
      (Poly.String.pretty greet wq);
  let greet = Poly.equal greet expect in
  let expect = true in
  check bool (sprintf "Primitives.test_lift_qubit") expect greet

let lift_qubit =
  [
    ( "1/2, 1 -> 1/2",
      `Quick,
      let s = div2 in
      test_lift_qubit s (Qubit Qubit.One) (to_poly (Scal s)) 1 );
    ( "1/2, 0 -> 0",
      `Quick,
      let s = div2 in
      test_lift_qubit s (Qubit Qubit.Zero) Poly.zero 1 );
    ( "1/2, x0 -> 1/2 x0",
      `Quick,
      let s = div2 in
      test_lift_qubit s (Qubit x0) (to_poly (Prod (Scal s, Qubit x0))) 1 );
    ( "1/2, x0 ++ x1 -> 1/2 x0 + 1/2 x1",
      `Quick,
      let s = div2 in
      let x = Monome.Qubit (SumMod2 (Var 0, Var 1)) in
      test_lift_qubit s x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (to_poly (Prod (Scal s, Qubit x1))))
        2 );
    ( "-1/8, x0 ++ x1 -> 7/8 x0 + 7/8 x1 + 1/4 x0x1",
      `Quick,
      let s = divm8 in
      let x = Monome.Qubit (SumMod2 (Var 0, Var 1)) in
      let mx0x1 = Monome.Prod (Scal div4, x0x1) in
      test_lift_qubit s x
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (Poly.insert (Prod (Scal s, Qubit x1)) (to_poly mx0x1)))
        2 );
    ( "1/8, x0 ++ x1x2 -> 1/8 x0 + 1/8 x1x2 - 1/4 x0x1x2",
      `Quick,
      let s = div8 in
      let s' = divm4 in
      let x = Qubit.SumMod2 (x0, Monome.to_qubit x1x2) in
      test_lift_qubit s (Qubit x)
        (Poly.insert
           (Prod (Scal s, Qubit x0))
           (Poly.insert
              (Prod (Scal s, x1x2))
              (to_poly (Prod (Scal s', x0x1x2)))))
        3 );
    ( "1/4, x0++x1++x2 -> 1/4x0+1/4x1+1/4x2 -1/2x0x1-1/2x0x2-1/2x1x2",
      `Quick,
      let s = div4 in
      let x = Qubit.SumMod2 (x0, SumMod2 (x1, x2)) in
      test_lift_qubit s (Qubit x)
        (Prod (Scal s, Qubit x0)
        +++ (Prod (Scal s, Qubit x1)
            +++ (Prod (Scal s, Qubit x2)
                +++ (Prod (Scal (Q.mul s minus_two), x0x1)
                    +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                        +++ (Prod (Scal (Q.mul s minus_two), x0x2)
                            +++ Poly.empty))))))
        3 );
    ( "1/8, x0++x1++x2 -> 1/8x0+1/8x1+1/8x2 -1/4x0x1-1/4x0x2-1/4x1x2 +1/2x0x1x2",
      `Quick,
      let s = div8 in
      let x = Qubit.SumMod2 (x0, SumMod2 (x1, x2)) in
      test_lift_qubit s (Qubit x)
        (Prod (Scal s, Qubit x0)
        +++ (Prod (Scal s, Qubit x1)
            +++ (Prod (Scal s, Qubit x2)
                +++ (Prod (Scal (Q.mul s minus_two), x0x1)
                    +++ (Prod (Scal (Q.mul s minus_two), x1x2)
                        +++ (Prod (Scal (Q.mul s minus_two), x0x2)
                            +++ (Prod (Scal (Q.mul s four), x0x1x2)
                                +++ Poly.empty)))))))
        3 );
  ]

(* phase = x0y0 + x0y1, ket = |y0 + y1> *)
(* phase[y0 <- y0 + y1] = x0y0, ket[y0 <- y0 + y1] = |y0> *)
let test_variable_replacement_factorisation ?(debug = true) (input : Path_sum.t)
    (expect : Path_sum.t) () =
  if debug then
    printf "Test.test_variable_replacement_factorisation, input =\n%s\n\n"
      (PSS.pretty input);
  let greet_repl =
    Rules.Variable_replacement.variable_replacement_factorisation input
  in
  if debug then
    printf "Test.test_variable_replacement_factorisation, greet_repl =\n%s\n\n"
      (PSS.pretty greet_repl);
  let greet = Rules.Simplification.simplify greet_repl in
  if debug then
    printf "Test.test_variable_replacement_factorisation, greet =\n%s\n\n"
      (PSS.pretty greet);
  let expect = Rules.Simplification.simplify expect in
  if debug then
    printf "Test.test_variable_replacement_factorisation, expect =\n%s\n\n"
      (PSS.pretty expect);
  let greeting = greet = expect in
  let expected = true in
  check bool
    (sprintf "test_variable_replacement_factorisation")
    expected greeting

let test_variable_replacement_factorisation_does_not_mutate_input () =
  let phase =
    Prod (Scal div2, Prod (Qubit x0, Qubit (v 1)))
    +++ to_poly (Prod (Scal div2, Prod (Qubit x0, Qubit (v 2))))
  in
  let input : Path_sum.t =
    {
      phase;
      ket = [| x0 ++ Qubit.Var 1 ++ Qubit.Var 2 |];
      path_var = [ 1; 2 ];
    }
  in
  let input_before = PSS.exact input in
  let _ = Rules.Variable_replacement.variable_replacement_factorisation input in
  check string "input unchanged" input_before (PSS.exact input)

let variable_replacement_factorisation =
  [
    ( "|x0> -> |x0>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| Var 0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Var 0 |]; path_var = [] } );
    ( "|x0+x1> -> |x0+x1>",
      `Quick,
      test_variable_replacement_factorisation
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 1 |];
          path_var = [];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 1 |];
          path_var = [];
        } );
    ( "|y0> -> |y0>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| Var 1 |]; path_var = [ 1 ] }
        { phase = to_poly (Scal Q.zero); ket = [| Var 1 |]; path_var = [ 1 ] }
    );
    ( "|0> -> |0>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.Zero |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.Zero |]; path_var = [] }
    );
    ( "|1> -> |1>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.One |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.One |]; path_var = [] }
    );
    ( "|1+x0> -> |1+x0>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| one ++ x0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| one ++ x0 |]; path_var = [] }
    );
    ( "|0+x0> -> |x0>",
      `Quick,
      test_variable_replacement_factorisation
        { phase = to_poly (Scal Q.zero); ket = [| zero ++ x0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| x0 |]; path_var = [] } );
    ( "|0+y0> -> |y1>",
      `Quick,
      test_variable_replacement_factorisation
        {
          phase = to_poly (Scal Q.zero);
          ket = [| zero ++ Var 1 |];
          path_var = [ 1 ];
        }
        { phase = to_poly (Scal Q.zero); ket = [| Var 1 |]; path_var = [ 1 ] }
    );
    ( "|1+y0,y0> -> |1+y0,y0>",
      `Quick,
      test_variable_replacement_factorisation
        {
          phase = to_poly (Scal Q.zero);
          ket = [| one ++ Var 2; Var 2 |];
          path_var = [ 2 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| one ++ Var 2; Var 2 |];
          path_var = [ 2 ];
        } );
    ( "|x0+y0+y1,y0,y1> -> |x0+y0+y1,y0,y1>",
      `Quick,
      test_variable_replacement_factorisation
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 3 ++ Var 4; Var 3; Var 4 |];
          path_var = [ 3; 4 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 3 ++ Var 4; Var 3; Var 4 |];
          path_var = [ 3; 4 ];
        } );
    ( "|x0+y0+y1> -> |x0+y0+y1>",
      `Quick,
      test_variable_replacement_factorisation
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 1 ++ Var 2 |];
          path_var = [ 1; 2 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 1 ++ Var 2 |];
          path_var = [ 1; 2 ];
        } );
    (* Here + is modulo 2. The bijection (y0, y1) -> (u = y0 + y1, y1)
       preserves all path assignments and the two-variable normalization. The
       now-unused y1 must therefore remain declared after u is renamed y0. *)
    ( "1/2 x0y0 + 1/2 x0y1 |x0*(y0+y1)> -> 1/2 x0y0 |x0*y0>",
      `Quick,
      let p =
        Prod (Scal div2, Prod (Qubit x0, Qubit (v 1)))
        +++ to_poly (Prod (Scal div2, Prod (Qubit x0, Qubit (v 2))))
      in
      let p' = to_poly (Prod (Scal div2, Prod (Qubit x0, Qubit (v 1)))) in
      let ps : Path_sum.t =
        {
          phase = p;
          ket = [| Qubit.Prod (x0, (Var 1 ++ Var 2)) |];
          path_var = [ 1; 2 ];
        }
      in
      let ps' : Path_sum.t =
        {
          phase = p';
          ket = [| Qubit.Prod (x0, Var 1) |];
          path_var = [ 1; 2 ];
        }
      in
      test_variable_replacement_factorisation ps ps' );
    ( "1/2 x0y0 + 1/2 x0y1 |x0+y0+y1> -> 1/2 x0y0 |x0+y0>",
      `Quick,
      let p =
        Prod (Scal div2, Prod (Qubit x0, Qubit (v 1)))
        +++ to_poly (Prod (Scal div2, Prod (Qubit x0, Qubit (v 2))))
      in
      let p' = to_poly (Prod (Scal div2, Prod (Qubit x0, Qubit (v 1)))) in
      let ps : Path_sum.t =
        { phase = p; ket = [| x0 ++ Var 1 ++ Var 2 |]; path_var = [ 1; 2 ] }
      in
      let ps' : Path_sum.t =
        { phase = p'; ket = [| x0 ++ Var 1 |]; path_var = [ 1; 2 ] }
      in
      test_variable_replacement_factorisation ps ps' );
    ( "factorisation does not mutate input",
      `Quick,
      test_variable_replacement_factorisation_does_not_mutate_input );
    ( "1/4 x0y0 + 1/4 x0y1 |x0+y0+y1> -> 1/4 x0y0 + 1/4 x0y1 |x0+y0+y1>",
      `Quick,
      let p =
        Prod (Scal div4, Prod (Qubit x0, Qubit (v 1)))
        +++ to_poly (Prod (Scal div4, Prod (Qubit x0, Qubit (v 2))))
      in
      let ps : Path_sum.t =
        { phase = p; ket = [| x0 ++ Var 1 ++ Var 2 |]; path_var = [ 1; 2 ] }
      in
      test_variable_replacement_factorisation ps ps );
  ]

let test_variable_replacement ?(debug = true) (input : Path_sum.t)
    (expect : Path_sum.t) () =
  if debug then
    printf "Test.test_variable_replacement, input =\n%s\n\n" (PSS.pretty input);
  (* This helper keeps the historical expectation: no replacement means the
     input path sum is unchanged. *)
  let greet_repl =
    match Rules.Variable_replacement.variable_replacement ~debug input with
    | Ok (Some ps) -> ps
    | Ok None -> input
    | Error (Rules.MalformedPathSum message) ->
        Alcotest.fail ("unexpected malformed path sum: " ^ message)
  in
  if debug then
    printf "Test.test_variable_replacement, greet_repl =\n%s%!\n\n"
      (PSS.pretty greet_repl);
  let greet =
    Rules.Simplification.simplify greet_repl
    (* (Rules.Rename.normalise_path_var ~debug greet_repl) *)
  in
  if debug then
    printf "Test.test_variable_replacement, greet =\n%s%!\n\n"
      (PSS.pretty greet);
  let expect = Rules.Simplification.simplify expect in
  if debug then
    printf "Test.test_variable_replacement, expect =\n%s%!\n\n"
      (PSS.pretty expect);
  let greeting = greet = expect in
  let expected = true in
  check bool (sprintf "test_variable_replacement") expected greeting

let test_variable_replacement_returns_typed_replacement () =
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| one ++ Var 1 |]; path_var = [ 1 ] }
  in
  let expected_output : Path_sum.t =
    { phase = Poly.zero; ket = [| Var 1 |]; path_var = [ 1 ] }
  in
  match Rules.Variable_replacement.variable_replacement input with
  | Ok (Some output) ->
      check string "replacement result" (PSS.exact expected_output)
        (PSS.exact output)
  | Ok None -> check bool "replacement expected" true false
  | Error (Rules.MalformedPathSum _) -> check bool "valid path sum" true false

let test_variable_replacement_returns_none () =
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  match Rules.Variable_replacement.variable_replacement input with
  | Ok None -> check bool "no replacement" true true
  | Ok (Some _) -> check bool "no replacement expected" true false
  | Error (Rules.MalformedPathSum _) -> check bool "valid path sum" true false

let test_variable_replacement_ignores_path_variable_under_product () =
  (* Input:    phase = 0, ket = |x0 + y0*y1, y1>
     Expected: no replacement.

     When y1 = 0, the first output is independent of y0. Replacing that output
     with a fresh path variable would therefore not be a bijective change of
     variables. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 0 ++ Qubit.Prod (v 2, v 3); v 3 |];
      path_var = [ 2; 3 ];
    }
  in
  match Rules.Variable_replacement.variable_replacement input with
  | Ok None -> ()
  | Ok (Some output) ->
      Alcotest.fail ("unexpected nonlinear replacement: " ^ PSS.exact output)
  | Error (Rules.MalformedPathSum message) ->
      Alcotest.fail ("unexpected malformed path sum: " ^ message)

let test_variable_replacement_allows_product_independent_of_path_variable () =
  (* A product may occur in Q when the candidate y0 remains a direct XOR term:
     |y0 + x0*x1, x1> has the valid form |y0 + Q(x), x1>. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 2 ++ Qubit.Prod (v 0, v 1); v 1 |];
      path_var = [ 2 ];
    }
  in
  let expected : Path_sum.t =
    { phase = Poly.zero; ket = [| v 2; v 1 |]; path_var = [ 2 ] }
  in
  match Rules.Variable_replacement.variable_replacement input with
  | Ok (Some output) ->
      check string "independent product replacement" (PSS.exact expected)
        (PSS.exact output)
  | Ok None -> Alcotest.fail "expected an independent product replacement"
  | Error (Rules.MalformedPathSum message) ->
      Alcotest.fail ("unexpected malformed path sum: " ^ message)

let test_variable_replacement_rejects_repeated_path_variable_under_product () =
  (* y0 occurs both directly and in y0*y1, so the output is not y0 xor Q with
     Q independent of y0. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 2 ++ Qubit.Prod (v 2, v 3); v 3 |];
      path_var = [ 2; 3 ];
    }
  in
  match Rules.Variable_replacement.variable_replacement input with
  | Ok None -> ()
  | Ok (Some output) ->
      Alcotest.fail
        ("unexpected repeated-variable replacement: " ^ PSS.exact output)
  | Error (Rules.MalformedPathSum message) ->
      Alcotest.fail ("unexpected malformed path sum: " ^ message)

let test_variable_replacement_reports_malformed_path_sum () =
  let malformed_path_sum : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 0 |]; path_var = [ 0 ] }
  in
  match
    Rules.Variable_replacement.variable_replacement malformed_path_sum
  with
  | Error (Rules.MalformedPathSum _) -> check bool "malformed path sum" true true
  | Ok _ -> check bool "malformed path sum expected" true false

(* In the path sums below, [+] denotes XOR in a ket or a substitution, and
   arithmetic addition in a phase. *)
let test_replace_not_path_var_by_var_does_not_mutate_input () =
  (* Input:    phase = 0, ket = |1 + y0>
     Change:   y0 <- 1 + y0
     Expected: phase = 0, ket = |y0> *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| Qubit.SumMod2 (Qubit.One, Qubit.Var 2) |];
      path_var = [ 2 ];
    }
  in
  let input_before = PSS.exact input in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  let expected_output : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Var 2 |]; path_var = [ 2 ] }
  in
  check string "input unchanged" input_before (PSS.exact input);
  check string "replacement result" (PSS.exact expected_output) (PSS.exact output)

let test_replace_not_path_var_by_var_normalizes_constant_shift () =
  (* Input:    phase = 1/2 y0, ket = |1 + y0>
     Change:   y0 <- 1 + y0
     Expected: phase = 1/2 + 1/2 y0, ket = |y0> *)
  let half_path_var =
    Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 1))
  in
  let input : Path_sum.t =
    {
      phase = to_poly half_path_var;
      ket = [| Qubit.One ++ Qubit.Var 1 |];
      path_var = [ 1 ];
    }
  in
  let expected : Path_sum.t =
    {
      phase = Monome.Scal div2 +++ to_poly half_path_var;
      ket = [| Qubit.Var 1 |];
      path_var = [ 1 ];
    }
  in
  let input_before = PSS.exact input in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  let expected = Rules.Simplification.simplify expected in
  check string "input unchanged" input_before (PSS.exact input);
  check string "constant shift" (PSS.exact expected) (PSS.exact output)

let v i = Qubit.Var i
let mdiv s = Monome.Scal s

let test_replace_not_path_var_by_var_normalizes_input_shift () =
  (* Relevant part of the owm-vs-qiskit/dqc_teleportation path sums:
     Input:
       phase = 1/2 x1y0 + 1/2 x2y1
       ket   = |y0, x0 + x1 + y1, x0 + x1>
     Change:
       y1 <- y1 + x0 + x1
     Expected:
       phase = 1/2 x0x2 + 1/2 x1x2 + 1/2 x1y0 + 1/2 x2y1
       ket   = |y0, y1, x0 + x1> *)
  let half_product left_variable right_variable =
    Monome.Prod
      ( Monome.Scal div2,
        Monome.Prod
          ( Monome.Qubit (v left_variable),
            Monome.Qubit (v right_variable) ) )
  in
  let input : Path_sum.t =
    {
      phase = half_product 1 3 +++ to_poly (half_product 2 4);
      ket = [| v 3; v 0 ++ v 1 ++ v 4; v 0 ++ v 1 |];
      path_var = [ 3; 4 ];
    }
  in
  let expected : Path_sum.t =
    {
      phase =
        half_product 0 2
        +++ (half_product 1 2
            +++ (half_product 1 3 +++ to_poly (half_product 2 4)));
      ket = [| v 3; v 4; v 0 ++ v 1 |];
      path_var = [ 3; 4 ];
    }
  in
  let input_before = PSS.exact input in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  let expected = Rules.Simplification.simplify expected in
  check string "input unchanged" input_before (PSS.exact input);
  check string "input-dependent path-variable shift" (PSS.exact expected)
    (PSS.exact output)

let test_replace_not_path_var_by_var_simplifies_quarter_phase () =
  (* Input:    phase = 1/4 x0 + 1/4 y0 + 1/2 x0y0,
               ket = |x0 + y0>
     Change:   y0 <- y0 + x0
     Expected: phase = 1/4 y0, ket = |y0>

     The first path variable has index [width]. The quarter coefficient must
     multiply the complete arithmetic lift of [x0 + y0]. *)
  let quarter_var variable =
    Monome.Prod (Monome.Scal div4, Monome.Qubit (v variable))
  in
  let half_product left_variable right_variable =
    Monome.Prod
      ( Monome.Scal div2,
        Monome.Prod
          ( Monome.Qubit (v left_variable),
            Monome.Qubit (v right_variable) ) )
  in
  let input : Path_sum.t =
    {
      phase =
        quarter_var 0
        +++ (quarter_var 1 +++ to_poly (half_product 0 1));
      ket = [| v 0 ++ v 1 |];
      path_var = [ 1 ];
    }
  in
  let expected : Path_sum.t =
    {
      phase = to_poly (quarter_var 1);
      ket = [| v 1 |];
      path_var = [ 1 ];
    }
  in
  let input_before = PSS.exact input in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  let expected = Rules.Simplification.simplify expected in
  check string "input unchanged" input_before (PSS.exact input);
  check string "quarter phase simplified" (PSS.exact expected)
    (PSS.exact output)

let test_replace_not_path_var_by_var_ignores_non_simplifying_shift () =
  (* Input:    phase = 0, ket = |x0 + y0, y0>
     Change:   y0 <- y0 + x0
     Result:   phase = 0, ket = |y0, x0 + y0>
     Expected: unchanged, because the change only moves the shifted output. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 0 ++ v 2; v 2 |];
      path_var = [ 2 ];
    }
  in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  check string "non-simplifying shift unchanged" (PSS.exact input)
    (PSS.exact output)

let test_replace_not_path_var_by_var_tries_next_candidate () =
  (* Input: phase = 0, ket = |x0 + y0, x1 + y1, y0>

     The first candidate y0 <- x0 + y0 only moves the direct occurrence of y0,
     so it must be ignored. The second candidate y1 <- x1 + y1 creates a new
     output equal to y1 and must be applied. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 0 ++ v 3; v 1 ++ v 4; v 3 |];
      path_var = [ 3; 4 ];
    }
  in
  let expected : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| v 0 ++ v 3; v 4; v 3 |];
      path_var = [ 3; 4 ];
    }
  in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  check string "next affine candidate" (PSS.exact expected) (PSS.exact output)

let test_replace_not_path_var_by_var_ignores_nonlinear_shift () =
  (* Input:            phase = 0, ket = |x0x1 + y0, x1>
     Candidate change: y0 <- y0 + x0x1
     Expected:         unchanged

     Products are valid in the lemma, but deliberately outside the first
     implementation scope. *)
  let input : Path_sum.t =
    {
      phase = Poly.zero;
      ket = [| Qubit.Prod (v 0, v 1) ++ v 2; v 1 |];
      path_var = [ 2 ];
    }
  in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  check string "nonlinear shift unchanged" (PSS.exact input) (PSS.exact output)

let test_replace_not_path_var_by_var_ignores_path_dependent_shift () =
  (* Input:            phase = 0, ket = |y0 + y1>
     Candidate change: y1 <- y1 + y0
     Expected:         unchanged

     A shift containing another path variable is also outside the first
     implementation scope. *)
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| v 1 ++ v 2 |]; path_var = [ 1; 2 ] }
  in
  let output = Rules.Variable_replacement.replace_not_path_var_by_var input in
  check string "path-dependent shift unchanged" (PSS.exact input)
    (PSS.exact output)

(* TODO : restore variable replacement without reordening *)

let variable_replacement =
  [
    ( "variable_replacement returns typed replacement",
      `Quick,
      test_variable_replacement_returns_typed_replacement );
    ( "variable_replacement returns none",
      `Quick,
      test_variable_replacement_returns_none );
    ( "variable_replacement ignores a path variable under a product",
      `Quick,
      test_variable_replacement_ignores_path_variable_under_product );
    ( "variable_replacement allows an independent product",
      `Quick,
      test_variable_replacement_allows_product_independent_of_path_variable );
    ( "variable_replacement rejects a repeated variable under a product",
      `Quick,
      test_variable_replacement_rejects_repeated_path_variable_under_product );
    ( "variable_replacement reports malformed path sum",
      `Quick,
      test_variable_replacement_reports_malformed_path_sum );
    ( "replace_not_path_var_by_var does not mutate input",
      `Quick,
      test_replace_not_path_var_by_var_does_not_mutate_input );
    ( "replace_not_path_var_by_var normalizes constant shift",
      `Quick,
      test_replace_not_path_var_by_var_normalizes_constant_shift );
    ( "replace_not_path_var_by_var normalizes input shift",
      `Quick,
      test_replace_not_path_var_by_var_normalizes_input_shift );
    ( "replace_not_path_var_by_var simplifies quarter phase",
      `Quick,
      test_replace_not_path_var_by_var_simplifies_quarter_phase );
    ( "replace_not_path_var_by_var ignores non-simplifying shift",
      `Quick,
      test_replace_not_path_var_by_var_ignores_non_simplifying_shift );
    ( "replace_not_path_var_by_var tries the next affine candidate",
      `Quick,
      test_replace_not_path_var_by_var_tries_next_candidate );
    ( "replace_not_path_var_by_var ignores nonlinear shift",
      `Quick,
      test_replace_not_path_var_by_var_ignores_nonlinear_shift );
    ( "replace_not_path_var_by_var ignores path-dependent shift",
      `Quick,
      test_replace_not_path_var_by_var_ignores_path_dependent_shift );
    ( "|x0> -> |x0>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| Var 0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Var 0 |]; path_var = [] } );
    ( "|x0+x1> -> |x0+x1>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 1 |];
          path_var = [];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 1 |];
          path_var = [];
        } );
    ( "|y0> -> |y0>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| Var 1 |]; path_var = [ 1 ] }
        { phase = to_poly (Scal Q.zero); ket = [| Var 1 |]; path_var = [ 1 ] }
    );
    ( "|0> -> |0>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.Zero |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.Zero |]; path_var = [] }
    );
    ( "|1> -> |1>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.One |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| Qubit.One |]; path_var = [] }
    );
    ( "|1+x0> -> |1+x0>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| one ++ x0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| one ++ x0 |]; path_var = [] }
    );
    ( "|1+y0> -> |y0>",
      `Quick,
      test_variable_replacement
        { phase = Poly.zero; ket = [| one ++ Var 1 |]; path_var = [ 1 ] }
        { phase = Poly.zero; ket = [| Var 1 |]; path_var = [ 1 ] } );
    ( "|1+y0>,y0 -> |y0>,1+y0",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Qubit (v 1));
          ket = [| one ++ v 1 |];
          path_var = [ 1 ];
        }
        {
          phase = Scal Q.one +++ to_poly (Qubit (v 1));
          ket = [| v 1 |];
          path_var = [ 1 ];
        } );
    ( "|1+y0>,y0/2 -> |y0>,1/2 + y0/2",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Prod (mdiv two, Qubit (v 1)));
          ket = [| one ++ v 1 |];
          path_var = [ 1 ];
        }
        {
          phase = mdiv two +++ to_poly (Prod (mdiv two, Qubit (v 1)));
          ket = [| v 1 |];
          path_var = [ 1 ];
        } );
    ( "|0+x0> -> |x0>",
      `Quick,
      test_variable_replacement
        { phase = to_poly (Scal Q.zero); ket = [| zero ++ x0 |]; path_var = [] }
        { phase = to_poly (Scal Q.zero); ket = [| x0 |]; path_var = [] } );
    ( "|0+y0> -> |y0>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| zero ++ v 1 |];
          path_var = [ 1 ];
        }
        { phase = to_poly (Scal Q.zero); ket = [| v 1 |]; path_var = [ 1 ] } );
    ( "|1+y0,y0> -> |1+y0,y0>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| one ++ Var 2; Var 2 |];
          path_var = [ 2 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| one ++ Var 2; Var 2 |];
          path_var = [ 2 ];
        } );
    (* ( "|1+y0,y1> -> |y0,y1>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| one ++ Var 2; Var 3 |];
          path_var = [ 2; 3 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 2; Var 3 |];
          path_var = [ 2; 3 ];
        } ); *)
    (* ( "|x0+y0,y1> -> |x0+y0,y1>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ v 2; v 3 |];
          path_var = [ 2; 3 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| v 2; v 3 |];
          path_var = [ 2; 3 ];
        } ); *)
    ( "1/4 y0, |x0+y0,y1> -> 1/4 y0, |x0+y0,y1>",
      `Quick,
      let p = to_poly (Prod (Scal div4, Qubit (v 2))) in
      let ps : Path_sum.t =
        { phase = p; ket = [| x0 ++ v 2; v 3 |]; path_var = [ 2; 3 ] }
      in
      test_variable_replacement ps ps );
    ( "1/4 y0, |x0+y0, y0+y1> -> 1/4 y0, |x0+y0, y1>",
      `Quick,
      let p = to_poly (Prod (Scal div4, Qubit (v 2))) in
      let ps : Path_sum.t =
        { phase = p; ket = [| x0 ++ v 2; v 2 ++ v 3 |]; path_var = [ 2; 3 ] }
      in
      let ps' : Path_sum.t =
        { phase = p; ket = [| x0 ++ v 2; v 3 |]; path_var = [ 2; 3 ] }
      in
      test_variable_replacement ps ps' );
    ( "1/4 x0, |x0+y0, y0+y1> -> 1/4 x0, |x0+y0, y0+y1>",
      `Quick,
      let p = to_poly (Prod (Scal div4, Qubit (v 0))) in
      let ps : Path_sum.t =
        { phase = p; ket = [| x0 ++ v 2; v 2 ++ v 3 |]; path_var = [ 2; 3 ] }
      in
      let ps' : Path_sum.t =
        { phase = p; ket = [| x0 ++ v 2; v 3 |]; path_var = [ 2; 3 ] }
      in
      test_variable_replacement ps ps' );
    (* ( "|x0+y0+y1,y1> -> |y0,y1>",
      `Quick,
      test_variable_replacement
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 2 ++ Var 3; Var 3 |];
          path_var = [ 2; 3 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 2; Var 3 |];
          path_var = [ 2; 3 ];
        } ); *)
    ( "|x0+y0+y1,y0,y1> -> |x0+y0+y1,y0,y1>",
      `Quick,
      let ps : Path_sum.t =
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 3 ++ Var 4; Var 3; Var 4 |];
          path_var = [ 3; 4 ];
        }
      in
      test_variable_replacement ps ps );
    ( "|x0+y0+y1> -> |x0+y0+y1>",
      `Quick,
      let ps : Path_sum.t =
        {
          phase = to_poly (Scal Q.zero);
          ket = [| x0 ++ Var 1 ++ Var 2 |];
          path_var = [ 1; 2 ];
        }
      in
      test_variable_replacement ps ps );
  ]

(* let ( ++ ) (m : monome) (p : poly) : poly = Poly.insert m p *)

let test_find_update_pvs (ps : Path_sum.t) update_pvs () =
  let greet =
    Rules.Rename._string_update_pvs (Rules.Rename._find_update_path_var ps)
  in
  let expect = Rules.Rename._string_update_pvs update_pvs in
  let greeting = greet in
  let expected = expect in
  check string (sprintf "generate update pvs ok") expected greeting

let test_path_var_substitute (pvs_input : int list) update pvs_expect () =
  let pvs_greet = Rules.Rename._path_var_substitute pvs_input update in
  let greet = pvs_greet = pvs_expect in
  let greeting = greet in
  let expected = true in
  check bool
    (sprintf
       "Test.test_path_var_substitute,\npvs_greet =\n%s\npvs_expect =\n%s\n"
       (ListBis.string_int pvs_greet)
       (ListBis.string_int pvs_expect))
    expected greeting

let test_substitute_path_var (ps : Path_sum.t) ps_expect () =
  let ps_input = ps in
  let update_pvs = Rules.Rename._find_update_path_var ps_input in
  let ps_greet = Rules.Rename._substitute_path_var ps_input update_pvs in
  let greet = ps_greet = ps_expect in
  let greeting = greet in
  let expected = true in
  check bool
    (sprintf "Test.test_substitute_path_var,\nps_greet =\n%s\nps_expect =\n%s\n"
       (PSS.pretty ps_greet) (PSS.pretty ps_expect))
    expected greeting

let test_substitute_path_var_does_not_mutate_input () =
  let ps_input : Path_sum.t =
    {
      phase = to_poly (Qubit (Qubit.Var 2));
      ket = [| Qubit.Var 2 |];
      path_var = [ 2 ];
    }
  in
  let input_before = PSS.exact ps_input in
  let ps_greet = Rules.Rename._substitute_path_var ps_input [ (2, 1) ] in
  let ps_expect : Path_sum.t =
    {
      phase = to_poly (Qubit (Qubit.Var 1));
      ket = [| Qubit.Var 1 |];
      path_var = [ 1 ];
    }
  in
  check string "input unchanged" input_before (PSS.exact ps_input);
  check string "substitution result" (PSS.exact ps_expect) (PSS.exact ps_greet)

let update_pvs =
  [
    ( "find_pvs ps1",
      `Quick,
      test_find_update_pvs
        { phase = to_poly (Scal Q.zero); ket = [| Var 0 |]; path_var = [ 1 ] }
        [ (1, 1) ] );
    ( "find_pvs ps2",
      `Quick,
      test_find_update_pvs
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0 |];
          path_var = [ 1; 2 ];
        }
        [ (1, 1); (2, 2) ] );
    ( "find_pvs ps3",
      `Quick,
      test_find_update_pvs
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0 |];
          path_var = [ 1; 3 ];
        }
        [ (1, 1); (3, 2) ] );
    ( "find_pvs ps4",
      `Quick,
      test_find_update_pvs
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0 |];
          path_var = [ 1; 5; 10; 15 ];
        }
        [ (1, 1); (5, 2); (10, 3); (15, 4) ] );
    ( "find_pvs ps5",
      `Quick,
      test_find_update_pvs
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 4; Var 10; Var 2 |];
          path_var = [ 4; 10 ];
        }
        [ (4, 4); (10, 5) ] );
    ( "pvs_subst pvs1",
      `Quick,
      test_path_var_substitute [ 4; 10 ] [ (4, 2) ] [ 2; 10 ] );
    ( "pvs_subst pvs1",
      `Quick,
      test_path_var_substitute [ 4; 10 ] [ (10, 5) ] [ 4; 5 ] );
    ( "subst_pv ps1",
      `Quick,
      test_substitute_path_var
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 4; Var 10; Var 2 |];
          path_var = [ 4; 10 ];
        }
        {
          phase = to_poly (Scal Q.zero);
          ket = [| Var 0; Var 4; Var 5; Var 2 |];
          path_var = [ 4; 5 ];
        } );
    ( "subst_pv does not mutate input",
      `Quick,
      test_substitute_path_var_does_not_mutate_input );
  ]

let test_qubit q1 q2 () =
  let greeting = QS.exact q1 in
  let expected = QS.exact q2 in
  check string "same string" expected greeting

let test_qubit_equal_result_returns_true () =
  match Qubit.equal_result ~wq1:1 ~wq2:1 (Var 0) (Var 0) with
  | Ok true -> check bool "equal qubits" true true
  | Ok false -> check bool "equal qubits expected" true false
  | Error _ -> check bool "well-formed comparison expected" true false

let test_qubit_equal_result_returns_false () =
  match Qubit.equal_result Zero One with
  | Ok false -> check bool "different qubits" false false
  | Ok true -> check bool "different qubits expected" false true
  | Error _ -> check bool "well-formed comparison expected" true false

let test_qubit_equal_result_reports_incompatible_widths () =
  match Qubit.equal_result ~wq1:0 ~wq2:1 Zero Zero with
  | Error Qubit.IncompatibleWidths -> check bool "incompatible widths" true true
  | Error Qubit.IncompletePathVariableMap ->
      check bool "incompatible widths expected" true false
  | Ok _ -> check bool "incompatible widths expected" true false

let test_qubit_equal_result_reports_incomplete_path_var_map () =
  let map_path_var1 = IntMap.singleton 1 0 in
  let map_path_var2 = IntMap.empty in
  match
    Qubit.equal_result ~wq1:1 ~wq2:1 ~map_path_var1 ~map_path_var2 (Var 1)
      (Var 1)
  with
  | Error Qubit.IncompletePathVariableMap ->
      check bool "incomplete path variable map" true true
  | Error Qubit.IncompatibleWidths ->
      check bool "incomplete path variable map expected" true false
  | Ok _ -> check bool "incomplete path variable map expected" true false

let test_qubit_remove_result_returns_some () =
  match Qubit.remove_result 1 (Prod (Var 1, Var 2)) with
  | Ok (Some output) -> check string "removed variable" "(Var 2)" (QS.exact output)
  | Ok None -> check bool "removed variable expected" true false
  | Error Qubit.CannotRemoveFromSum ->
      check bool "product expression expected" true false

let test_qubit_remove_result_returns_none () =
  match Qubit.remove_result 3 (Prod (Var 1, Var 2)) with
  | Ok None -> check bool "absent variable" true true
  | Ok (Some _) -> check bool "absent variable expected" true false
  | Error Qubit.CannotRemoveFromSum ->
      check bool "product expression expected" true false

let test_qubit_remove_result_reports_sum () =
  match Qubit.remove_result 1 (SumMod2 (Var 1, Var 2)) with
  | Error Qubit.CannotRemoveFromSum ->
      check bool "sum expression rejected" true true
  | Ok _ -> check bool "sum expression rejection expected" true false

let test_qubit_extract_var_preserves_accumulator_at_zero () =
  (* The traversal finds Var 1 before visiting Zero. Zero must preserve the
     variable already stored in the accumulator. *)
  check (list int) "variable before Zero" [ 1 ]
    (Qubit.extract_var (SumMod2 (Var 1, Zero)))

let test_qubit_extract_path_var_preserves_accumulator_at_one () =
  (* With width 2, Var 3 is a path variable. Visiting One afterwards must not
     erase it from the accumulator. *)
  check (list int) "path variable before One" [ 3 ]
    (Qubit.extract_path_var (Prod (Var 3, One)) 2)

let qubit =
  [
    ( "equal_result returns true",
      `Quick,
      test_qubit_equal_result_returns_true );
    ( "equal_result returns false",
      `Quick,
      test_qubit_equal_result_returns_false );
    ( "equal_result reports incompatible widths",
      `Quick,
      test_qubit_equal_result_reports_incompatible_widths );
    ( "equal_result reports incomplete path variable map",
      `Quick,
      test_qubit_equal_result_reports_incomplete_path_var_map );
    ("remove_result returns some", `Quick, test_qubit_remove_result_returns_some);
    ("remove_result returns none", `Quick, test_qubit_remove_result_returns_none);
    ("remove_result reports sum", `Quick, test_qubit_remove_result_reports_sum);
    ( "extract_var preserves variables at Zero",
      `Quick,
      test_qubit_extract_var_preserves_accumulator_at_zero );
    ( "extract_path_var preserves variables at One",
      `Quick,
      test_qubit_extract_path_var_preserves_accumulator_at_one );
    ( "simplify: x0.(1 ++ x0) -> Zero",
      `Quick,
      test_qubit (Qubit.simplify (Prod (Var 0, One ++ Var 0))) Zero );
    ( "simplify: (x0.(1 ++ x0) ++ x1.x0) -> x0.x1",
      `Quick,
      test_qubit
        (Qubit.simplify
           (SumMod2 (Prod (Var 0, One ++ Var 0), Prod (Var 1, Var 0))))
        (Prod (Var 0, Var 1)) );
    ( "simplify: (x0.(1 ++ x0) ++ x1.One) -> x1",
      `Quick,
      test_qubit
        (Qubit.simplify
           (SumMod2 (Prod (Var 5, One ++ Var 5), Prod (Var 2, One))))
        (Var 2) );
  ]

let test_ket k1 k2 () =
  let greeting = KS.exact k1 in
  let expected = KS.exact k2 in
  check string "same string" expected greeting

let test_ket_equal_result_returns_true () =
  match Ket.equal_result [| Qubit.Var 0 |] [| Qubit.Var 0 |] with
  | Ok (true, _, _) -> check bool "equal kets" true true
  | Ok (false, _, _) -> check bool "equal kets expected" true false
  | Error _ -> check bool "well-formed comparison expected" true false

let test_ket_equal_result_returns_false () =
  match Ket.equal_result [| Qubit.Zero |] [| Qubit.One |] with
  | Ok (false, _, _) -> check bool "different kets" false false
  | Ok (true, _, _) -> check bool "different kets expected" false true
  | Error _ -> check bool "well-formed comparison expected" true false

let test_ket_equal_result_reports_different_output_lengths () =
  match
    Ket.equal_result ~outputs1:[ 0 ] ~outputs2:[ 0; 1 ]
      [| Qubit.Var 0; Qubit.Var 1 |]
      [| Qubit.Var 0; Qubit.Var 1 |]
  with
  | Error Ket.DifferentOutputLengths ->
      check bool "different output lengths" true true
  | Error Ket.InvalidOutputIndex ->
      check bool "different output lengths expected" true false
  | Ok _ -> check bool "different output lengths expected" true false

let test_ket_equal_result_reports_invalid_output_index () =
  match
    Ket.equal_result ~outputs1:[ 1 ] ~outputs2:[ 0 ] [| Qubit.Var 0 |]
      [| Qubit.Var 0 |]
  with
  | Error Ket.InvalidOutputIndex -> check bool "invalid output index" true true
  | Error Ket.DifferentOutputLengths ->
      check bool "invalid output index expected" true false
  | Ok _ -> check bool "invalid output index expected" true false

let test_ket_equal_result_returns_consistent_path_var_maps () =
  (* Both kets have width 2, so Var 0 and Var 1 are free variables. The path
     variables are renamed by the bijection 2 -> 4 and 3 -> 2. *)
  let ket1 = [| v 0 ++ v 2; v 1 ++ v 3 |] in
  let ket2 = [| v 0 ++ v 4; v 1 ++ v 2 |] in
  let expected_map1 = IntMap.of_list [ (2, 4); (3, 2) ] in
  let expected_map2 = IntMap.of_list [ (2, 3); (4, 2) ] in
  match Ket.equal_result ket1 ket2 with
  | Ok (true, map1, map2) ->
      check string "first-to-second path-variable map"
        (Common.to_string_int_map expected_map1)
        (Common.to_string_int_map map1);
      check string "second-to-first path-variable map"
        (Common.to_string_int_map expected_map2)
        (Common.to_string_int_map map2)
  | Ok (false, _, _) -> Alcotest.fail "consistent path-variable renaming rejected"
  | Error _ -> Alcotest.fail "well-formed ket comparison expected"

let test_ket_equal_result_rejects_inconsistent_path_var_renaming () =
  (* The first comparison would map 2 -> 4, but the second would require the
     same source variable 2 to map to 3. *)
  match Ket.equal_result [| v 2; v 2 |] [| v 4; v 3 |] with
  | Ok (false, _, _) -> ()
  | Ok (true, _, _) -> Alcotest.fail "inconsistent path-variable map accepted"
  | Error _ -> Alcotest.fail "well-formed ket comparison expected"

let test_ket_equal_result_rejects_non_injective_path_var_renaming () =
  (* Distinct source variables 2 and 3 cannot both map to variable 4. *)
  match Ket.equal_result [| v 2; v 3 |] [| v 4; v 4 |] with
  | Ok (false, _, _) -> ()
  | Ok (true, _, _) -> Alcotest.fail "non-injective path-variable map accepted"
  | Error _ -> Alcotest.fail "well-formed ket comparison expected"

let test_ket_path_var_order_result_returns_order () =
  (* For a ket of width 2, variables 0 and 1 are input/output variables x0,x1.
     Variables starting at 2 are path variables y0,y1,... *)
  let check_ok name ket path_var_count expected_tmp expected_final =
    match Ket.path_var_order_result ket path_var_count with
    | Ok (tmp_path_vars, path_vars) ->
        check string (name ^ " temporary path vars") expected_tmp
          (ArrayBis.string_int tmp_path_vars);
        check string (name ^ " path vars") expected_final
          (ArrayBis.string_int path_vars)
    | Error Ket.InvalidPathVariableCount ->
        check bool "valid path-variable count expected" true false
    | Error Ket.InvalidPathVariableIndex ->
        check bool "valid path-variable indices expected" true false
  in
  (* The ket contains y0 then y1. The function records their final order
     [2;3] and a temporary negative order [-2;-3] used during renaming. *)
  check_ok "ordered path vars" [| Qubit.Var 2; Qubit.Var 3 |] 2 "-2;-3" "2;3";
  (* With width 1 and no declared path variable, Var 0 is just x0. *)
  check_ok "no path vars" [| Qubit.Var 0 |] 0 "" ""

let test_ket_path_var_order_result_reports_invalid_path_var_count () =
  (* A negative number of declared path variables is malformed metadata. *)
  match Ket.path_var_order_result [||] (-1) with
  | Error Ket.InvalidPathVariableCount ->
      check bool "invalid path-variable count" true true
  | Error Ket.InvalidPathVariableIndex ->
      check bool "invalid path-variable count expected" true false
  | Ok _ -> check bool "invalid path-variable count expected" true false

let test_ket_path_var_order_result_reports_invalid_path_var_index () =
  (* Width is 1, so Var 2 would be y1. With only one declared path variable,
     the only valid path variable is y0, encoded as Var 1. *)
  match Ket.path_var_order_result [| Qubit.Var 2 |] 1 with
  | Error Ket.InvalidPathVariableIndex ->
      check bool "invalid path-variable index" true true
  | Error Ket.InvalidPathVariableCount ->
      check bool "invalid path-variable index expected" true false
  | Ok _ -> check bool "invalid path-variable index expected" true false

let test_ket_substitute_does_not_mutate_input () =
  let input =
    [| Qubit.Var 1; Qubit.SumMod2 (Qubit.Var 1, Qubit.Var 2) |]
  in
  let input_before = KS.exact input in
  let output = Ket.substitute input 1 (Qubit.Var 3) in
  let expected =
    [| Qubit.Var 3; Qubit.SumMod2 (Qubit.Var 2, Qubit.Var 3) |]
  in
  check string "input unchanged" input_before (KS.exact input);
  check string "substitution result" (KS.exact expected) (KS.exact output)

let test_ket_substitute_reuses_input_when_unchanged () =
  let input = [| Qubit.Var 0 |] in
  let output = Ket.substitute input 1 (Qubit.Var 2) in
  check bool "same array when unchanged" true (input == output)

let test_ket_substitute_many_single_pass () =
  let input = [| Qubit.Var 1; Qubit.Var 2 |] in
  let input_before = KS.exact input in
  let output =
    Ket.substitute_many input [ (1, Qubit.Var 2); (2, Qubit.One) ]
  in
  let expected = [| Qubit.Var 2; Qubit.One |] in
  check string "input unchanged" input_before (KS.exact input);
  check string "substitution result" (KS.exact expected) (KS.exact output)

let k1 =
  [|
    Qubit.Var 0;
    SumMod2
      ( Prod (Prod (Var 0, Var 1), Var 2),
        SumMod2 (Prod (Prod (Var 0, Var 1), Var 2), Var 2) );
    Var 3;
  |]

let k1_simplified = Ket.simplify k1
let k2 = [| Qubit.Var 0; Var 2; Var 3 |]

let k3 =
  [|
    Qubit.Var 0;
    SumMod2
      ( Prod (Var 0, Prod (Var 1, Var 2)),
        SumMod2 (Prod (Prod (Var 1, Var 2), Var 0), Var 2) );
    Var 3;
  |]

let k3_simplified = Ket.simplify k3
let k4 = [| Qubit.Var 0; Var 2; Var 3 |]

let ket =
  [
    ("equal_result returns true", `Quick, test_ket_equal_result_returns_true);
    ("equal_result returns false", `Quick, test_ket_equal_result_returns_false);
    ( "equal_result reports different output lengths",
      `Quick,
      test_ket_equal_result_reports_different_output_lengths );
    ( "equal_result reports invalid output index",
      `Quick,
      test_ket_equal_result_reports_invalid_output_index );
    ( "equal_result returns consistent path-variable maps",
      `Quick,
      test_ket_equal_result_returns_consistent_path_var_maps );
    ( "equal_result rejects inconsistent path-variable renaming",
      `Quick,
      test_ket_equal_result_rejects_inconsistent_path_var_renaming );
    ( "equal_result rejects non-injective path-variable renaming",
      `Quick,
      test_ket_equal_result_rejects_non_injective_path_var_renaming );
    ( "path_var_order_result returns order",
      `Quick,
      test_ket_path_var_order_result_returns_order );
    ( "path_var_order_result reports invalid path-variable count",
      `Quick,
      test_ket_path_var_order_result_reports_invalid_path_var_count );
    ( "path_var_order_result reports invalid path-variable index",
      `Quick,
      test_ket_path_var_order_result_reports_invalid_path_var_index );
    ("(x0.x1 ++ (x0.x1 ++ x2) -> x2", `Quick, test_ket k1_simplified k2);
    ("(x0.x1.x2 ++ (x1.x2.x0 ++ x3) -> x3", `Quick, test_ket k3_simplified k4);
    ( "(x0,(x0.(1 ++ x0) ++ x1.One) -> x1",
      `Quick,
      test_ket
        (Ket.simplify
           [| Var 0; SumMod2 (Prod (Var 5, One ++ Var 5), Prod (Var 2, One)) |])
        [| Var 0; Var 2 |] );
    ( "(x0,(x0.(1 ++ x0) ++ x1.x2) -> x1",
      `Quick,
      test_ket
        (Ket.simplify
           [|
             Var 0; SumMod2 (Prod (Var 5, One ++ Var 5), Prod (Var 2, Var 3));
           |])
        [| Var 0; Prod (Var 2, Var 3) |] );
    ( "substitute does not mutate input",
      `Quick,
      test_ket_substitute_does_not_mutate_input );
    ( "substitute reuses input when unchanged",
      `Quick,
      test_ket_substitute_reuses_input_when_unchanged );
    ( "substitute_many is single pass",
      `Quick,
      test_ket_substitute_many_single_pass );
  ]

let test_gates_apply ?(debug = true) (p : Program.t) (ps : Path_sum.t) () =
  let greeting =
    printf "Test.test_gates_apply, ps =\n%s\n\n" (PSS.pretty ps);
    printf "Test.test_gates_apply, p =\n%s\n\n" (ProgS.pretty p);

    let ps_exe = Program.execution p in
    if debug then
      printf "Test.test_apply_gates, ps_exe =\n%s\n\n" (PSS.pretty ps_exe);
    let ps_greet = reduce_valid_path_sum ~debug ps_exe in
    if debug then
      printf "Test.test_apply_gates, ps_greet =\n%s\n\n" (PSS.pretty ps_greet);
    let ps_expect = reduce_valid_path_sum ~debug ps in
    printf "\nTest.test_gates_apply, ps_expect =\n%s\n\n" (PSS.pretty ps_expect);
    Path_sum.equal ~debug ps_greet ps_expect
  in
  let expected = true in
  check bool
    (sprintf "Test.test_gates_apply\np = %s\n" (ProgS.pretty p))
    expected greeting

let test_path_sum_library_h_result_returns_path_sum () =
  (* For width 1, target 0 is x0 and the first path variable is y0 = Var 1. *)
  (* H maps |x0> to sum_y exp(2.pi.i.x0.y0/2)|y0>. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod
          ( Monome.Scal div2,
            Monome.Prod
              (Monome.Qubit (Qubit.Var 0), Monome.Qubit (Qubit.Var 1)) )
        +++ Poly.empty;
      ket = [| Qubit.Var 1 |];
      path_var = [ 1 ];
    }
  in
  match Path_sum_library.h_result 0 1 with
  | Ok path_sum ->
      check string "h gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_h_result_reports_invalid_target () =
  (* Target 1 is outside width 1; the typed constructor reports that directly. *)
  match Path_sum_library.h_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_x_result_returns_path_sum () =
  (* For width 1, target 0 is the only valid input variable: x0. *)
  (* X maps |x0> to |1+x0> and does not introduce phase or path variables. *)
  let expected : Path_sum.t =
    {
      phase = Monome.Scal Q.zero +++ Poly.empty;
      ket = [| Qubit.SumMod2 (Qubit.One, Qubit.Var 0) |];
      path_var = [];
    }
  in
  match Path_sum_library.x_result 0 1 with
  | Ok path_sum ->
      check string "x gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_x_result_reports_invalid_target () =
  (* Target indices are zero-based: target 1 is outside a width-1 path sum. *)
  (* This checks the typed error that replaces the old unchecked xx failure. *)
  match Path_sum_library.x_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_u1_result_returns_path_sum () =
  (* With k=1 and default s=1, U1 adds the phase x0 / 2 and keeps |x0>. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 0))
        +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match Path_sum_library.u1_result 1 0 1 with
  | Ok path_sum ->
      check string "u1 gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_u1_result_reports_invalid_target () =
  (* U1 also relies on xx: target 1 is outside width 1 and must be reported. *)
  match Path_sum_library.u1_result 1 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_z_result_returns_path_sum () =
  (* Z is U1 with k=1: it adds phase x0 / 2 and keeps |x0>. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 0))
        +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match Path_sum_library.z_result 0 1 with
  | Ok path_sum ->
      check string "z gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_z_result_reports_invalid_target () =
  (* z_result delegates target validation to u1_result and reports the same error. *)
  match Path_sum_library.z_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_s_result_returns_path_sum () =
  (* S is U1 with k=2: it adds phase x0 / 4 and keeps |x0>. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod (Monome.Scal div4, Monome.Qubit (Qubit.Var 0))
        +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match Path_sum_library.s_result 0 1 with
  | Ok path_sum ->
      check string "s gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_s_result_reports_invalid_target () =
  (* s_result delegates target validation to u1_result and reports the same error. *)
  match Path_sum_library.s_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_t_result_returns_path_sum () =
  (* T is U1 with k=3: it adds phase x0 / 8 and keeps |x0>. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod (Monome.Scal div8, Monome.Qubit (Qubit.Var 0))
        +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match Path_sum_library.t_result 0 1 with
  | Ok path_sum ->
      check string "t gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_t_result_reports_invalid_target () =
  (* t_result delegates target validation to u1_result and reports the same error. *)
  match Path_sum_library.t_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let test_path_sum_library_zinv_result_returns_path_sum () =
  (* Z inverse is U1 with s=-1 and k=1. The negative angle is normalized to
     1/2, so the expected path sum is the same as Z on one qubit. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 0))
        +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  match Path_sum_library.zinv_result 0 1 with
  | Ok path_sum ->
      check string "zinv gate path sum" (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_zinv_result_reports_invalid_target () =
  (* zinv_result delegates target validation to u1_result and reports the same error. *)
  match Path_sum_library.zinv_result 1 1 with
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

(* These helpers keep the gate-result tests focused on the expected path sum
   instead of repeating the same Ok/Error plumbing in every case. *)
let check_gate_result name expected = function
  | Ok path_sum -> check string name (PSS.exact expected) (PSS.exact path_sum)
  | Error _ -> check bool "valid target expected" true false

let check_gate_invalid_target = function
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      check bool "invalid target rejected" true true
  | Error Path_sum_library.OverlappingGateWires ->
      check bool "invalid target error expected" true false
  | Ok _ -> check bool "invalid target expected" true false

let check_gate_overlapping_wires = function
  | Error Path_sum_library.OverlappingGateWires -> ()
  | Error Path_sum_library.TargetIndexOutOfWidth ->
      Alcotest.fail "in-width overlapping wires reported as an invalid index"
  | Ok _ -> Alcotest.fail "overlapping gate wires were accepted"

(* A gate built for width [w] must return all [w] output qubits, including
   untouched wires. These checks use non-canonical wire positions so a local
   one-, two-, or three-qubit ket cannot accidentally satisfy the test. *)
let check_gate_ket name expected_ket = function
  | Ok path_sum ->
      check string name (KS.exact expected_ket) (KS.exact path_sum.ket)
  | Error _ -> check bool "valid target expected" true false

let test_path_sum_library_h_result_uses_declared_width () =
  (* H on wire 1 replaces x1 with y0 = Var 3 and preserves wires 0 and 2. *)
  check_gate_ket "h gate declared width"
    [| Qubit.Var 0; Qubit.Var 3; Qubit.Var 2 |]
    (Path_sum_library.h_result 1 3)

let test_path_sum_library_x_result_uses_declared_width () =
  (* X on wire 1 changes only x1 in the three-qubit ket. *)
  check_gate_ket "x gate declared width"
    [|
      Qubit.Var 0;
      Qubit.SumMod2 (Qubit.One, Qubit.Var 1);
      Qubit.Var 2;
    |]
    (Path_sum_library.x_result 1 3)

let test_path_sum_library_u1_result_uses_declared_width () =
  (* U1 changes the phase only, so its three output qubits stay unchanged. *)
  check_gate_ket "u1 gate declared width"
    [| Qubit.Var 0; Qubit.Var 1; Qubit.Var 2 |]
    (Path_sum_library.u1_result 1 1 3)

let test_path_sum_library_rz_result_uses_declared_width () =
  (* RZ changes the phase only, so its three output qubits stay unchanged. *)
  check_gate_ket "rz gate declared width"
    [| Qubit.Var 0; Qubit.Var 1; Qubit.Var 2 |]
    (Path_sum_library.rz_result 1 1 3)

let test_path_sum_library_rx_result_uses_declared_width () =
  (* RX on wire 1 replaces x1 with y1 = Var 4 and preserves other wires. *)
  check_gate_ket "rx gate declared width"
    [| Qubit.Var 0; Qubit.Var 4; Qubit.Var 2 |]
    (Path_sum_library.rx_result 1 1 3)

let test_path_sum_library_ry_result_uses_declared_width () =
  (* RY on wire 1 replaces x1 with 1 xor y1 and preserves other wires. *)
  check_gate_ket "ry gate declared width"
    [|
      Qubit.Var 0;
      Qubit.SumMod2 (Qubit.One, Qubit.Var 4);
      Qubit.Var 2;
    |]
    (Path_sum_library.ry_result 1 1 3)

let test_path_sum_library_ch_result_uses_declared_width () =
  (* CH uses wire 2 as control and wire 0 as target; wire 1 is untouched. *)
  let control = Qubit.Var 2 in
  let target = Qubit.Var 0 in
  let path_variable = Qubit.Var 3 in
  let transformed_target =
    Qubit.SumMod2
      ( Qubit.Prod (control, target),
        Qubit.SumMod2 (Qubit.Prod (control, path_variable), target) )
  in
  check_gate_ket "ch gate declared width"
    (Ket.simplify [| transformed_target; Qubit.Var 1; control |])
    (Path_sum_library.ch_result 2 0 3)

let test_path_sum_library_cx_result_uses_declared_width () =
  (* CX uses wire 2 as control and wire 0 as target; wire 1 is untouched. *)
  check_gate_ket "cx gate declared width"
    [|
      Qubit.SumMod2 (Qubit.Var 2, Qubit.Var 0);
      Qubit.Var 1;
      Qubit.Var 2;
    |]
    (Path_sum_library.cx_result 2 0 3)

let test_path_sum_library_crz_result_uses_declared_width () =
  (* CRZ changes the phase only, regardless of the selected control and target. *)
  check_gate_ket "crz gate declared width"
    [| Qubit.Var 0; Qubit.Var 1; Qubit.Var 2 |]
    (Path_sum_library.crz_result 1 2 0 3)

let test_path_sum_library_ccx_result_uses_declared_width () =
  (* CCX uses wires 2 and 0 as controls and wire 1 as target. *)
  check_gate_ket "ccx gate declared width"
    [|
      Qubit.Var 0;
      Qubit.SumMod2
        (Qubit.Prod (Qubit.Var 2, Qubit.Var 0), Qubit.Var 1);
      Qubit.Var 2;
    |]
    (Path_sum_library.ccx_result 2 0 1 3)

let test_path_sum_library_ccz_result_uses_declared_width () =
  (* CCZ changes the phase only, so all three output qubits stay unchanged. *)
  check_gate_ket "ccz gate declared width"
    [| Qubit.Var 0; Qubit.Var 1; Qubit.Var 2 |]
    (Path_sum_library.ccz_result 2 0 1 3)

let test_path_sum_library_ch_result_rejects_overlapping_wires () =
  (* A controlled gate cannot use wire 0 as both control and target. *)
  check_gate_overlapping_wires (Path_sum_library.ch_result 0 0 1)

let test_path_sum_library_cx_result_rejects_overlapping_wires () =
  (* CX has the same distinct-wire invariant as CH. *)
  check_gate_overlapping_wires (Path_sum_library.cx_result 0 0 1)

let test_path_sum_library_crz_result_rejects_overlapping_wires () =
  (* A phase-only controlled gate still requires distinct physical wires. *)
  check_gate_overlapping_wires (Path_sum_library.crz_result 1 0 0 1)

let test_path_sum_library_ccx_result_rejects_duplicate_controls () =
  (* The two controls of a three-wire gate must be distinct. *)
  check_gate_overlapping_wires (Path_sum_library.ccx_result 0 0 1 2)

let test_path_sum_library_ccx_result_rejects_first_control_target_overlap () =
  (* The first control cannot also be the target. *)
  check_gate_overlapping_wires (Path_sum_library.ccx_result 0 1 0 2)

let test_path_sum_library_ccx_result_rejects_second_control_target_overlap () =
  (* The second control cannot also be the target. *)
  check_gate_overlapping_wires (Path_sum_library.ccx_result 0 1 1 2)

let test_path_sum_library_ccz_result_rejects_overlapping_wires () =
  (* CCZ must enforce the same three-wire invariant as CCX. *)
  check_gate_overlapping_wires (Path_sum_library.ccz_result 0 0 1 2)

let test_path_sum_library_invalid_index_precedes_overlap () =
  (* Validate every selected index before reporting that controls 0 and 0
     overlap; target 2 is outside width 2. *)
  check_gate_invalid_target (Path_sum_library.ccx_result 0 0 2 2)

let test_path_sum_library_result_reports_negative_target () =
  (* Public typed gate constructors reject negative indices before building Vars. *)
  check_gate_invalid_target (Path_sum_library.h_result (-1) 1);
  check_gate_invalid_target (Path_sum_library.cx_result (-1) 0 2)

let one_qubit_phase_path_sum scalar : Path_sum.t =
  {
    phase =
      Monome.Prod (Monome.Scal scalar, Monome.Qubit (Qubit.Var 0))
      +++ Poly.empty;
    ket = [| Qubit.Var 0 |];
    path_var = [];
  }

let test_path_sum_library_sinv_result_returns_path_sum () =
  (* S inverse is U1 with s=-1 and k=2; -1/4 is normalized to 3/4. *)
  check_gate_result "sinv gate path sum"
    (one_qubit_phase_path_sum (3 /// 4))
    (Path_sum_library.sinv_result 0 1)

let test_path_sum_library_sinv_result_reports_invalid_target () =
  (* sinv_result reports the same target-width error as u1_result. *)
  check_gate_invalid_target (Path_sum_library.sinv_result 1 1)

let test_path_sum_library_tinv_result_returns_path_sum () =
  (* T inverse is U1 with s=-1 and k=3; -1/8 is normalized to 7/8. *)
  check_gate_result "tinv gate path sum"
    (one_qubit_phase_path_sum (7 /// 8))
    (Path_sum_library.tinv_result 0 1)

let test_path_sum_library_tinv_result_reports_invalid_target () =
  (* tinv_result reports the same target-width error as u1_result. *)
  check_gate_invalid_target (Path_sum_library.tinv_result 1 1)

let test_path_sum_library_rz_result_returns_path_sum () =
  (* RZ with k=1 adds the global term 3/4 and the target phase x0/2. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Scal (3 /// 4)
        +++ (Monome.Prod (Monome.Scal div2, Monome.Qubit (Qubit.Var 0))
            +++ Poly.empty);
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  check_gate_result "rz gate path sum" expected (Path_sum_library.rz_result 1 0 1)

let test_path_sum_library_rz_result_reports_invalid_target () =
  (* rz_result validates its target before building the phase polynomial. *)
  check_gate_invalid_target (Path_sum_library.rz_result 1 1 1)

let test_path_sum_library_rx_result_returns_path_sum () =
  (* With s=0, RX is represented here as the identity path sum on the target. *)
  let expected : Path_sum.t =
    { phase = Monome.Scal Q.zero +++ Poly.empty; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  check_gate_result "rx gate path sum" expected
    (Path_sum_library.rx_result ~s:0 1 0 1)

let test_path_sum_library_rx_result_reports_invalid_target () =
  (* rx_result still validates the target even when s=0 makes the phase trivial. *)
  check_gate_invalid_target (Path_sum_library.rx_result ~s:0 1 1 1)

let test_path_sum_library_ry_result_returns_path_sum () =
  (* With s=0, RY is represented here as the identity path sum on the target. *)
  let expected : Path_sum.t =
    { phase = Monome.Scal Q.zero +++ Poly.empty; ket = [| Qubit.Var 0 |]; path_var = [] }
  in
  check_gate_result "ry gate path sum" expected
    (Path_sum_library.ry_result ~s:0 1 0 1)

let test_path_sum_library_ry_result_reports_invalid_target () =
  (* ry_result still validates the target even when s=0 makes the phase trivial. *)
  check_gate_invalid_target (Path_sum_library.ry_result ~s:0 1 1 1)

let test_path_sum_library_negative_rotation_exponents_are_identity () =
  (* With integer s, k < 0 gives an exact identity and needs no path variables. *)
  let expected : Path_sum.t =
    {
      phase = Monome.Scal Q.zero +++ Poly.empty;
      ket = [| Qubit.Var 0 |];
      path_var = [];
    }
  in
  check_gate_result "negative-exponent u1" expected
    (Path_sum_library.u1_result (-1) 0 1);
  check_gate_result "negative-exponent rz" expected
    (Path_sum_library.rz_result (-1) 0 1);
  check_gate_result "negative-exponent rx" expected
    (Path_sum_library.rx_result (-1) 0 1);
  check_gate_result "negative-exponent ry" expected
    (Path_sum_library.ry_result (-1) 0 1)

let test_path_sum_library_rotation_path_vars_are_width_offset () =
  (* RX/RY introduce y0 and y1, represented as Var width and Var (width + 1). *)
  let check_path_vars name = function
    | Ok path_sum ->
        check string name (ListBis.string_int [ 3; 4 ])
          (ListBis.string_int path_sum.path_var)
    | Error _ -> check bool "valid target expected" true false
  in
  check_path_vars "rx path vars" (Path_sum_library.rx_result 1 2 3);
  check_path_vars "ry path vars" (Path_sum_library.ry_result 1 2 3)

let test_path_sum_library_ch_result_returns_path_sum () =
  (* CH introduces one path variable y0 = Var 2 for width 2. The phase is the
     controlled-Hadamard phase plus its normalization factor. *)
  let control = Qubit.Var 0 in
  let target = Qubit.Var 1 in
  let path_var = Qubit.Var 2 in
  let normalisation =
    Monome.Scal div8
    +++ (Monome.Prod (Monome.Scal divm8, Monome.Qubit control)
        +++ (Monome.Prod
               ( Monome.Scal div4,
                 Monome.Prod (Monome.Qubit control, Monome.Qubit path_var) )
            +++ (Monome.Prod (Monome.Scal divm4, Monome.Qubit path_var)
                +++ Poly.empty)))
  in
  let expected : Path_sum.t =
    {
      phase =
        Poly.simplify
          (Monome.Prod
             ( Monome.Scal div2,
               Monome.Prod
                 ( Monome.Qubit control,
                   Monome.Prod (Monome.Qubit target, Monome.Qubit path_var) ) )
          +++ normalisation);
      ket =
        Ket.simplify
          [|
            control;
            Qubit.SumMod2
              ( Qubit.Prod (control, target),
                Qubit.SumMod2 (Qubit.Prod (control, path_var), target) );
          |];
      path_var = [ 2 ];
    }
  in
  check_gate_result "ch gate path sum" expected (Path_sum_library.ch_result 0 1 2)

let test_path_sum_library_ch_result_reports_invalid_target () =
  (* ch_result validates both selected input variables from left to right. *)
  check_gate_invalid_target (Path_sum_library.ch_result 0 2 2)

let test_path_sum_library_cx_result_returns_path_sum () =
  (* CX maps |x0,x1> to |x0,x0+x1> without phase or path variables. *)
  let expected : Path_sum.t =
    {
      phase = Monome.Scal Q.zero +++ Poly.empty;
      ket = [| Qubit.Var 0; Qubit.SumMod2 (Qubit.Var 0, Qubit.Var 1) |];
      path_var = [];
    }
  in
  check_gate_result "cx gate path sum" expected (Path_sum_library.cx_result 0 1 2)

let test_path_sum_library_cx_result_reports_invalid_target () =
  (* cx_result reports an out-of-width control or target. *)
  check_gate_invalid_target (Path_sum_library.cx_result 0 2 2)

let controlled_phase_path_sum scalar : Path_sum.t =
  {
    phase =
      Monome.Prod
        ( Monome.Scal scalar,
          Monome.Prod (Monome.Qubit (Qubit.Var 0), Monome.Qubit (Qubit.Var 1)) )
      +++ Poly.empty;
    ket = [| Qubit.Var 0; Qubit.Var 1 |];
    path_var = [];
  }

let test_path_sum_library_crz_result_returns_path_sum () =
  (* CRZ with k=1 adds the controlled phase x0.x1/2. *)
  check_gate_result "crz gate path sum"
    (controlled_phase_path_sum div2)
    (Path_sum_library.crz_result 1 0 1 2)

let test_path_sum_library_crz_result_reports_invalid_target () =
  (* crz_result validates both selected input variables. *)
  check_gate_invalid_target (Path_sum_library.crz_result 1 0 2 2)

let test_path_sum_library_cz_result_returns_path_sum () =
  (* CZ is CRZ with k=1. *)
  check_gate_result "cz gate path sum"
    (controlled_phase_path_sum div2)
    (Path_sum_library.cz_result 0 1 2)

let test_path_sum_library_cz_result_reports_invalid_target () =
  (* cz_result reports the same target-width error as crz_result. *)
  check_gate_invalid_target (Path_sum_library.cz_result 0 2 2)

let test_path_sum_library_cs_result_returns_path_sum () =
  (* CS is CRZ with k=2, so the controlled phase is x0.x1/4. *)
  check_gate_result "cs gate path sum"
    (controlled_phase_path_sum div4)
    (Path_sum_library.cs_result 0 1 2)

let test_path_sum_library_cs_result_reports_invalid_target () =
  (* cs_result reports the same target-width error as crz_result. *)
  check_gate_invalid_target (Path_sum_library.cs_result 0 2 2)

let test_path_sum_library_ct_result_returns_path_sum () =
  (* CT is CRZ with k=3, so the controlled phase is x0.x1/8. *)
  check_gate_result "ct gate path sum"
    (controlled_phase_path_sum div8)
    (Path_sum_library.ct_result 0 1 2)

let test_path_sum_library_ct_result_reports_invalid_target () =
  (* ct_result reports the same target-width error as crz_result. *)
  check_gate_invalid_target (Path_sum_library.ct_result 0 2 2)

let test_path_sum_library_ccx_result_returns_path_sum () =
  (* CCX maps |x0,x1,x2> to |x0,x1,x0.x1+x2>. *)
  let expected : Path_sum.t =
    {
      phase = Monome.Scal Q.zero +++ Poly.empty;
      ket =
        [|
          Qubit.Var 0;
          Qubit.Var 1;
          Qubit.SumMod2
            (Qubit.Prod (Qubit.Var 0, Qubit.Var 1), Qubit.Var 2);
        |];
      path_var = [];
    }
  in
  check_gate_result "ccx gate path sum" expected
    (Path_sum_library.ccx_result 0 1 2 3)

let test_path_sum_library_ccx_result_reports_invalid_target () =
  (* ccx_result validates both controls and the target. *)
  check_gate_invalid_target (Path_sum_library.ccx_result 0 1 3 3)

let test_path_sum_library_ccz_result_returns_path_sum () =
  (* CCZ adds the triple controlled phase x0.x1.x2/2. *)
  let expected : Path_sum.t =
    {
      phase =
        Monome.Prod
          ( Monome.Scal div2,
            Monome.Prod
              ( Monome.Qubit (Qubit.Var 0),
                Monome.Prod
                  (Monome.Qubit (Qubit.Var 1), Monome.Qubit (Qubit.Var 2)) ) )
        +++ Poly.empty;
      ket = [| Qubit.Var 0; Qubit.Var 1; Qubit.Var 2 |];
      path_var = [];
    }
  in
  check_gate_result "ccz gate path sum" expected
    (Path_sum_library.ccz_result 0 1 2 3)

let test_path_sum_library_ccz_result_reports_invalid_target () =
  (* ccz_result validates both controls and the target. *)
  check_gate_invalid_target (Path_sum_library.ccz_result 0 1 3 3)

let test_apply_hadamard_does_not_mutate_input () =
  let input = Path_sum.ofSize 1 in
  let input_before = PSS.exact input in
  let _ = Gates.Apply_gates.apply_hadamard input [] 0 in
  check string "input unchanged" input_before (PSS.exact input)

let test_apply_not_does_not_mutate_input () =
  let input = Path_sum.ofSize 1 in
  let input_before = PSS.exact input in
  let _ = Gates.Apply_gates.apply_not input [] 0 in
  check string "input unchanged" input_before (PSS.exact input)

let test_apply_classical_not_does_not_mutate_input () =
  let input : Path_sum.t =
    { phase = Poly.zero; ket = [| Qubit.Zero |]; path_var = [] }
  in
  let input_before = PSS.exact input in
  let _ = Gates.Apply_gates.apply_classical_not input 0 in
  check string "input unchanged" input_before (PSS.exact input)

let gates_apply =
  [
    ( "h_result returns path sum",
      `Quick,
      test_path_sum_library_h_result_returns_path_sum );
    ( "h_result reports invalid target",
      `Quick,
      test_path_sum_library_h_result_reports_invalid_target );
    ( "gate results report negative target",
      `Quick,
      test_path_sum_library_result_reports_negative_target );
    ( "x_result returns path sum",
      `Quick,
      test_path_sum_library_x_result_returns_path_sum );
    ( "x_result reports invalid target",
      `Quick,
      test_path_sum_library_x_result_reports_invalid_target );
    ( "u1_result returns path sum",
      `Quick,
      test_path_sum_library_u1_result_returns_path_sum );
    ( "u1_result reports invalid target",
      `Quick,
      test_path_sum_library_u1_result_reports_invalid_target );
    ( "z_result returns path sum",
      `Quick,
      test_path_sum_library_z_result_returns_path_sum );
    ( "z_result reports invalid target",
      `Quick,
      test_path_sum_library_z_result_reports_invalid_target );
    ( "s_result returns path sum",
      `Quick,
      test_path_sum_library_s_result_returns_path_sum );
    ( "s_result reports invalid target",
      `Quick,
      test_path_sum_library_s_result_reports_invalid_target );
    ( "t_result returns path sum",
      `Quick,
      test_path_sum_library_t_result_returns_path_sum );
    ( "t_result reports invalid target",
      `Quick,
      test_path_sum_library_t_result_reports_invalid_target );
    ( "zinv_result returns path sum",
      `Quick,
      test_path_sum_library_zinv_result_returns_path_sum );
    ( "zinv_result reports invalid target",
      `Quick,
      test_path_sum_library_zinv_result_reports_invalid_target );
    ( "sinv_result returns path sum",
      `Quick,
      test_path_sum_library_sinv_result_returns_path_sum );
    ( "sinv_result reports invalid target",
      `Quick,
      test_path_sum_library_sinv_result_reports_invalid_target );
    ( "tinv_result returns path sum",
      `Quick,
      test_path_sum_library_tinv_result_returns_path_sum );
    ( "tinv_result reports invalid target",
      `Quick,
      test_path_sum_library_tinv_result_reports_invalid_target );
    ( "rz_result returns path sum",
      `Quick,
      test_path_sum_library_rz_result_returns_path_sum );
    ( "rz_result reports invalid target",
      `Quick,
      test_path_sum_library_rz_result_reports_invalid_target );
    ( "rx_result returns path sum",
      `Quick,
      test_path_sum_library_rx_result_returns_path_sum );
    ( "rx_result reports invalid target",
      `Quick,
      test_path_sum_library_rx_result_reports_invalid_target );
    ( "ry_result returns path sum",
      `Quick,
      test_path_sum_library_ry_result_returns_path_sum );
    ( "ry_result reports invalid target",
      `Quick,
      test_path_sum_library_ry_result_reports_invalid_target );
    ( "negative rotation exponents are identity",
      `Quick,
      test_path_sum_library_negative_rotation_exponents_are_identity );
    ( "rotation path vars are width-offset",
      `Quick,
      test_path_sum_library_rotation_path_vars_are_width_offset );
    ( "h_result uses declared width",
      `Quick,
      test_path_sum_library_h_result_uses_declared_width );
    ( "x_result uses declared width",
      `Quick,
      test_path_sum_library_x_result_uses_declared_width );
    ( "u1_result uses declared width",
      `Quick,
      test_path_sum_library_u1_result_uses_declared_width );
    ( "rz_result uses declared width",
      `Quick,
      test_path_sum_library_rz_result_uses_declared_width );
    ( "rx_result uses declared width",
      `Quick,
      test_path_sum_library_rx_result_uses_declared_width );
    ( "ry_result uses declared width",
      `Quick,
      test_path_sum_library_ry_result_uses_declared_width );
    ( "ch_result uses declared width",
      `Quick,
      test_path_sum_library_ch_result_uses_declared_width );
    ( "cx_result uses declared width",
      `Quick,
      test_path_sum_library_cx_result_uses_declared_width );
    ( "crz_result uses declared width",
      `Quick,
      test_path_sum_library_crz_result_uses_declared_width );
    ( "ccx_result uses declared width",
      `Quick,
      test_path_sum_library_ccx_result_uses_declared_width );
    ( "ccz_result uses declared width",
      `Quick,
      test_path_sum_library_ccz_result_uses_declared_width );
    ( "ch_result rejects overlapping wires",
      `Quick,
      test_path_sum_library_ch_result_rejects_overlapping_wires );
    ( "cx_result rejects overlapping wires",
      `Quick,
      test_path_sum_library_cx_result_rejects_overlapping_wires );
    ( "crz_result rejects overlapping wires",
      `Quick,
      test_path_sum_library_crz_result_rejects_overlapping_wires );
    ( "ccx_result rejects duplicate controls",
      `Quick,
      test_path_sum_library_ccx_result_rejects_duplicate_controls );
    ( "ccx_result rejects first control-target overlap",
      `Quick,
      test_path_sum_library_ccx_result_rejects_first_control_target_overlap );
    ( "ccx_result rejects second control-target overlap",
      `Quick,
      test_path_sum_library_ccx_result_rejects_second_control_target_overlap );
    ( "ccz_result rejects overlapping wires",
      `Quick,
      test_path_sum_library_ccz_result_rejects_overlapping_wires );
    ( "invalid index precedes overlap",
      `Quick,
      test_path_sum_library_invalid_index_precedes_overlap );
    ( "ch_result returns path sum",
      `Quick,
      test_path_sum_library_ch_result_returns_path_sum );
    ( "ch_result reports invalid target",
      `Quick,
      test_path_sum_library_ch_result_reports_invalid_target );
    ( "cx_result returns path sum",
      `Quick,
      test_path_sum_library_cx_result_returns_path_sum );
    ( "cx_result reports invalid target",
      `Quick,
      test_path_sum_library_cx_result_reports_invalid_target );
    ( "crz_result returns path sum",
      `Quick,
      test_path_sum_library_crz_result_returns_path_sum );
    ( "crz_result reports invalid target",
      `Quick,
      test_path_sum_library_crz_result_reports_invalid_target );
    ( "cz_result returns path sum",
      `Quick,
      test_path_sum_library_cz_result_returns_path_sum );
    ( "cz_result reports invalid target",
      `Quick,
      test_path_sum_library_cz_result_reports_invalid_target );
    ( "cs_result returns path sum",
      `Quick,
      test_path_sum_library_cs_result_returns_path_sum );
    ( "cs_result reports invalid target",
      `Quick,
      test_path_sum_library_cs_result_reports_invalid_target );
    ( "ct_result returns path sum",
      `Quick,
      test_path_sum_library_ct_result_returns_path_sum );
    ( "ct_result reports invalid target",
      `Quick,
      test_path_sum_library_ct_result_reports_invalid_target );
    ( "ccx_result returns path sum",
      `Quick,
      test_path_sum_library_ccx_result_returns_path_sum );
    ( "ccx_result reports invalid target",
      `Quick,
      test_path_sum_library_ccx_result_reports_invalid_target );
    ( "ccz_result returns path sum",
      `Quick,
      test_path_sum_library_ccz_result_returns_path_sum );
    ( "ccz_result reports invalid target",
      `Quick,
      test_path_sum_library_ccz_result_reports_invalid_target );
    ( "apply_hadamard does not mutate input",
      `Quick,
      test_apply_hadamard_does_not_mutate_input );
    ( "apply_not does not mutate input",
      `Quick,
      test_apply_not_does_not_mutate_input );
    ( "apply_classical_not does not mutate input",
      `Quick,
      test_apply_classical_not_does_not_mutate_input );
    ("id", `Quick, test_gates_apply id (Path_sum.ofSize 0));
    ("h", `Quick, test_gates_apply (h 0) (Path_sum_library.h 0 1));
    ("x", `Quick, test_gates_apply (x 0) (Path_sum_library.x 0 1));
    ("z", `Quick, test_gates_apply (zz 0) (Path_sum_library.z 0 1));
    ("s", `Quick, test_gates_apply (ss 0) (Path_sum_library.s 0 1));
    ("t", `Quick, test_gates_apply (tt 0) (Path_sum_library.t 0 1));
    ("zinv", `Quick, test_gates_apply (zinv 0) (Path_sum_library.zinv 0 1));
    ("sinv", `Quick, test_gates_apply (sinv 0) (Path_sum_library.sinv 0 1));
    ("tinv", `Quick, test_gates_apply (tinv 0) (Path_sum_library.tinv 0 1));
    ("u1 4", `Quick, test_gates_apply (u1 0 0) (Path_sum_library.u1 0 0 1));
    ( "u1 -1",
      `Quick,
      test_gates_apply (u1 ~s:(-1) 1 0) (Path_sum_library.u1 ~s:(-1) 1 0 1) );
    ( "u1 -2",
      `Quick,
      test_gates_apply (u1 ~s:(-1) 2 0) (Path_sum_library.u1 ~s:(-1) 2 0 1) );
    ( "u1 -3 2",
      `Quick,
      test_gates_apply (u1 ~s:(-3) 2 0) (Path_sum_library.u1 ~s:(-3) 2 0 1) );
    ("u1 4", `Quick, test_gates_apply (u1 4 0) (Path_sum_library.u1 4 0 1));
    ("rz 0", `Quick, test_gates_apply (rz 0 0) (Path_sum_library.rz 0 0 1));
    ("rz 4", `Quick, test_gates_apply (rz 4 0) (Path_sum_library.rz 4 0 1));
    ( "rz (-4)",
      `Quick,
      test_gates_apply (rz ~s:(-1) 4 0) (Path_sum_library.rz ~s:(-1) 4 0 1) );
    ("rx 0", `Quick, test_gates_apply (rx 0 0) (Path_sum_library.rx 0 0 1));
    ("rx 1", `Quick, test_gates_apply (rx 1 0) (Path_sum_library.rx 1 0 1));
    ("rx 5", `Quick, test_gates_apply (rx 5 0) (Path_sum_library.rx 5 0 1));
    ( "rx (-5)",
      `Quick,
      test_gates_apply (rx ~s:(-1) 5 0) (Path_sum_library.rx ~s:(-1) 5 0 1) );
    ( "rx (-2,3)",
      `Quick,
      test_gates_apply (rx ~s:(-2) 3 0) (Path_sum_library.rx ~s:(-2) 3 0 1) );
    ("ry 0", `Quick, test_gates_apply (ry 0 0) (Path_sum_library.ry 0 0 1));
    ("ry 1", `Quick, test_gates_apply (ry 1 0) (Path_sum_library.ry 1 0 1));
    ("ry 5", `Quick, test_gates_apply (ry 5 0) (Path_sum_library.ry 5 0 1));
    ( "ry -5",
      `Quick,
      test_gates_apply (ry ~s:(-1) 5 0) (Path_sum_library.ry ~s:(-1) 5 0 1) );
    ( "ry -3/5",
      `Quick,
      test_gates_apply (ry ~s:(-3) 5 0) (Path_sum_library.ry ~s:(-3) 5 0 1) );
    ("ch", `Quick, test_gates_apply (ch 0 1) (Path_sum_library.ch 0 1 2));
    ("cx", `Quick, test_gates_apply (cx 0 1) (Path_sum_library.cx 0 1 2));
    ("cz", `Quick, test_gates_apply (cz 0 1) (Path_sum_library.cz 0 1 2));
    ("cs", `Quick, test_gates_apply (cs 0 1) (Path_sum_library.cs 0 1 2));
    ("ct", `Quick, test_gates_apply (ct 0 1) (Path_sum_library.ct 0 1 2));
    ("ccx", `Quick, test_gates_apply (ccx 0 1 2) (Path_sum_library.ccx 0 1 2 3));
    ("ccz", `Quick, test_gates_apply (ccz 0 1 2) (Path_sum_library.ccz 0 1 2 3));
  ]

let () =
  Alcotest.run "Symbolic execution"
    [
      ("ListBis", list_bis);
      ("Poly Normalise", poly_normalize);
      (* ("Normalise Path Variables", normalise_path_var); *)
      ("HH", hh);
      ("Path-sum equality", path_sum_equality);
      ("Path-sum initialization", path_sum_initialization);
      ("Path-sum substitution", path_sum_substitution);
      ("Poly equality", poly_equality);
      ("Poly conversion", poly_conversion);
      ("Poly algebra", poly_algebra);
      ("Lift Poly", lift_poly);
      ("Lift Monome", lift_monome);
      ("Lift Qubit", lift_qubit);
      ("Monome equality", monome_equality);
      ("Monome simplification", monome_simplification);
      ("Monome to scalar monome", monome_to_scalar_monome);
      ("Variable replacement Factorisation", variable_replacement_factorisation);
      ("Variable replacement", variable_replacement);
      ("Update Path-vars", update_pvs);
      ("Qubit", qubit);
      ("Ket", ket);
      ("Gates application", gates_apply);
    ]
