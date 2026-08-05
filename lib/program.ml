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
module QS = Qubit.String
module PSS = Path_sum.String
open Common
include Rational
open Gates

type t =
  | E
  | Apply of Gates.t * int list * int list
  | Sequence of t * t
  | Measure of int * int
  | It of int list * t
  | InitQ of int
  | Not of int

let format p =
  let rec aux p =
    match p with
    | Sequence (Sequence (p1, p2), p3) ->
        aux (Sequence (aux p1, aux (Sequence (aux p2, aux p3))))
    | Sequence (E, p') | Sequence (p', E) -> aux p'
    | Sequence (p1, p2) -> Sequence (aux p1, aux p2)
    | _ -> p
  in
  let p_formatted = aux p in
  p_formatted

(* (width_classical, width_quantum) *)
let widths (p : t) : int * int =
  if p = E then (0, 0)
  else
    let rec aux (p : t) wc wq =
      match p with
      | Apply (_, co, ta) ->
          (wc, Int.max wq (Int.max (ListBis.max_int co) (ListBis.max_int ta)))
      | InitQ ta -> (wc, Int.max wq ta)
      | Measure (i, j) -> (Int.max wc j, Int.max wq i)
      | It (i, p') ->
          let wc', wq' = aux p' wc wq in
          (Int.max wc' (ListBis.max_int i), wq')
      | Not ta -> (Int.max ta wc, wq)
      | Sequence (p1, p2) ->
          let wc1, wq1 = aux p1 wc wq in
          let wc2, wq2 = aux p2 wc1 wq1 in
          (Int.max wc2 wc, Int.max wq2 wq)
      | E -> (wc, wq)
    in
    let wc, wq = aux p (-1) 0 in
    (wc + 1, wq + 1)

module String = struct
  let u1_exact s k co ta =
    sprintf "Apply (U1 (%s,%d),%s,%s)" (Q.to_string s) k
      (ListBis.string_int co) (ListBis.string_int ta)

  let gp_exact s k co ta =
    sprintf "Apply (GP (%s,%d),%s,%s)" (Q.to_string s) k
      (ListBis.string_int co) (ListBis.string_int ta)

  let pretty p =
    let rec aux p =
      match p with
      | E -> "E"
      | Apply (H, [ co ], [ ta ]) -> sprintf "ch %d %d" co ta
      | Apply (H, [], [ ta ]) -> sprintf "h %d" ta
      | Apply (H, co, ta) ->
          sprintf "Apply (H,%s,%s)" (ListBis.string_int co)
            (ListBis.string_int ta)
      | Apply (X, [ co ], [ ta ]) -> sprintf "cx %d %d" co ta
      | Apply (X, [ co1; co2 ], [ ta ]) -> sprintf "ccx %d %d %d" co1 co2 ta
      | Apply (X, [], [ ta ]) -> sprintf "x %d" ta
      | Apply (X, co, ta) ->
          sprintf "Apply (X,%s,%s)" (ListBis.string_int co)
            (ListBis.string_int ta)
      | Apply (U1 (s, k), [ co ], [ ta ]) ->
          if k < 0 then u1_exact s k [ co ] [ ta ]
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            if Q.equal angle div2 then sprintf "cz %d %d" co ta
            else if Q.equal angle div4 then sprintf "cs %d %d" co ta
            else if Q.equal angle div8 then sprintf "ct %d %d" co ta
            else sprintf "cu1 (2.pi.%s) %d %d" (Q.to_string angle) co ta
          else
            let angle = Q.div_2exp (Q.neg s) k in
            if Q.equal angle div2 then sprintf "czinv %d %d" co ta
            else if Q.equal angle div4 then sprintf "csinv %d %d" co ta
            else if Q.equal angle div8 then sprintf "ctinv %d %d" co ta
            else sprintf "cu1 (-2.pi.%s) %d %d" (Q.to_string angle) co ta
      | Apply (U1 (s, k), [ co1; co2 ], [ ta ]) ->
          if k < 0 then u1_exact s k [ co1; co2 ] [ ta ]
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            sprintf "ccu1 (2.pi.%s) %d %d %d" (Q.to_string angle) co1 co2 ta
          else
            let angle = Q.div_2exp (Q.neg s) k in
            sprintf "cu1 (-2.pi.%s) %d %d %d" (Q.to_string angle) co1 co2 ta
      | Apply (U1 (s, k), [], [ ta ]) ->
          if k < 0 then u1_exact s k [] [ ta ]
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            if Q.equal angle div2 then sprintf "z %d" ta
            else if Q.equal angle div4 then sprintf "s %d" ta
            else if Q.equal angle div8 then sprintf "t %d" ta
            else sprintf "u1 (2.pi.%s) %d" (Q.to_string angle) ta
          else
            let angle = Q.div_2exp (Q.neg s) k in
            if Q.equal angle div2 then sprintf "zinv %d" ta
            else if Q.equal angle div4 then sprintf "sinv %d" ta
            else if Q.equal angle div8 then sprintf "tinv %d" ta
            else sprintf "u1 (-2.pi.%s) %d" (Q.to_string angle) ta
      | Apply (U1 (s, k), co, ta) ->
          if k < 0 then u1_exact s k co ta
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            sprintf "Apply (U1 (2.pi.%s),%s,%s)" (Q.to_string angle)
              (ListBis.string_int co) (ListBis.string_int ta)
          else
            let angle = Q.div_2exp (Q.neg s) k in
            sprintf "Apply (U1 (-2.pi.%s),%s,%s)" (Q.to_string angle)
              (ListBis.string_int co) (ListBis.string_int ta)
      | Apply (GP (s, k), [], []) ->
          if k < 0 then gp_exact s k [] []
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            sprintf "gp (2.pi.%s)" (Q.to_string angle)
          else
            let angle = Q.div_2exp (Q.neg s) k in
            sprintf "gp (-2.pi.%s)" (Q.to_string angle)
      | Apply (GP (s, k), [ co ], []) ->
          if k < 0 then gp_exact s k [ co ] []
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            sprintf "cgp (2.pi.%s) %d" (Q.to_string angle) co
          else
            let angle = Q.div_2exp (Q.neg s) k in
            sprintf "cgp (-2.pi.%s) %d" (Q.to_string angle) co
      | Apply (GP (s, k), co, ta) ->
          if k < 0 then gp_exact s k co ta
          else if Q.leq Q.zero s then
            let angle = Q.div_2exp s k in
            sprintf "Apply (GP (2.pi.%s),%s,%s)" (Q.to_string angle)
              (ListBis.string_int co) (ListBis.string_int ta)
          else
            let angle = Q.div_2exp (Q.neg s) k in
            sprintf "Apply (GP (-2.pi.%s),%s,%s)" (Q.to_string angle)
              (ListBis.string_int co) (ListBis.string_int ta)
      | Measure (k, ta) -> sprintf "m %d %d" k ta
      | InitQ ta -> sprintf "iq0 %d" ta
      | Not ta -> sprintf "notC %d" ta
      | It ([ k ], p') -> sprintf "it %d (%s)" k (aux p')
      | It (k, p') -> sprintf "It (%s,%s)" (ListBis.string_int k) (aux p')
      | Sequence (p1, p2) -> aux p1 ^ "\n" ^ aux p2
    in
    aux p

  let exact p =
    let rec aux p =
      match p with
      | E -> "E"
      | Apply (H, co, ta) ->
          sprintf "Apply (H,%s,%s)" (ListBis.string_int co)
            (ListBis.string_int ta)
      | Apply (X, co, ta) ->
          sprintf "Apply (X,%s,%s)" (ListBis.string_int co)
            (ListBis.string_int ta)
      | Apply (U1 (s, k), co, ta) ->
          sprintf "Apply (U1 (%s,%d),%s,%s)" (Q.to_string s) k
            (ListBis.string_int co) (ListBis.string_int ta)
      | Apply (GP (s, k), co, ta) ->
          sprintf "Apply (GP (%s,%d),%s,%s)" (Q.to_string s) k
            (ListBis.string_int co) (ListBis.string_int ta)
      | Measure (k, ta) -> sprintf "Measure (%d,%d)" k ta
      | InitQ ta -> sprintf "InitQ %d" ta
      | Not ta -> sprintf "Not %d" ta
      | It (k, p') -> sprintf "It (%s,%s)" (ListBis.string_int k) (aux p')
      | Sequence (p1, p2) -> aux p1 ^ " --\n" ^ aux p2
    in
    aux p
end

module Print = struct
  let pretty p = Printf.printf "%s\n" (String.pretty p)

  let print_to_file output_file p =
    let oc = open_out output_file in
    let function_name = StringBis.extract_filename output_file in
    let module_name = StringBis.capitalize_first_character function_name in
    Printf.fprintf oc
      "module %s = struct\n\
       open SQbricks.Program\n\
       include Program.Macros\n\n\
       let circuit = \n\n\
       %s\n\n\
       end"
      module_name (String.exact p);
    close_out oc
end

let rec nb_gate p =
  match p with
  | Apply _ -> 1
  | It (_, p') -> nb_gate p'
  | Sequence (p1, p2) ->
      let nb1 = nb_gate p1 in
      let nb2 = nb_gate p2 in
      (* above 248159 gates : stack overflow *)
      if nb1 > 248158 || nb2 > 200000 then
        failwith (sprintf "Program.nb_gate, nb1 = %d, nb2 = % d" nb1 nb2)
      else nb1 + nb2
  | _ -> 0

(* (nb_ch,nb_cs,nb_cz,nb_ccz,ccx,nb_cu1) *)
let nb_gate_and_gates_decomposition p =
  let rec aux nb_ch nb_cs nb_cz nb_ccz nb_ccx nb_cu1 nb_total (p : t) =
    match p with
    | Apply (H, [ _ ], [ _ ]) ->
        (* CH *)
        (nb_ch + 1, nb_cs, nb_cz, nb_ccz, nb_ccx, nb_cu1, nb_total + 1)
    | Apply (U1 (s, k), [ _ ], [ _ ])
      when Q.equal (Q.div_2exp s k) div4 || Q.equal (Q.div_2exp s k) divm4 ->
        (* CS or CSinv *)
        (nb_ch, nb_cs + 1, nb_cz, nb_ccz, nb_ccx, nb_cu1, nb_total + 1)
    | Apply (U1 (s, k), [ _ ], [ _ ])
      when Q.equal (Q.div_2exp s k) div2 || Q.equal (Q.div_2exp s k) divm2 ->
        (* CZ or CZinv *)
        (nb_ch, nb_cs, nb_cz + 1, nb_ccz, nb_ccx, nb_cu1, nb_total + 1)
    | Apply (U1 (s, k), [ _; _ ], [ _ ])
      when Q.equal (Q.div_2exp s k) div2 || Q.equal (Q.div_2exp s k) divm2 ->
        (* CCZ or CCZinv *)
        (nb_ch, nb_cs, nb_cz, nb_ccz + 1, nb_ccx, nb_cu1, nb_total + 1)
    | Apply (X, [ _; _ ], [ _ ]) ->
        (nb_ch, nb_cs, nb_cz, nb_ccz, nb_ccx + 1, nb_cu1, nb_total + 1)
    | Apply (U1 _, [ _ ], [ _ ]) ->
        (nb_ch, nb_cs, nb_cz, nb_ccz, nb_ccx, nb_cu1 + 1, nb_total + 1)
    | Apply (_, _, _) ->
        (nb_ch, nb_cs, nb_cz, nb_ccz, nb_ccx, nb_cu1, nb_total + 1)
    | Sequence (p1, p2) ->
        let nb_ch', nb_cs', nb_cz', nb_ccz', nb_ccx', nb_cu1', nb_total' =
          aux nb_ch nb_cs nb_cz nb_ccz nb_ccx nb_cu1 nb_total p1
        in
        aux nb_ch' nb_cs' nb_cz' nb_ccz' nb_ccx' nb_cu1' nb_total' p2
    | _ -> (nb_ch, nb_cs, nb_cz, nb_ccz, nb_ccx, nb_cu1, nb_total)
  in
  aux 0 0 0 0 0 0 0 p

(** Execute a program from an initial path-sum.
    @param p program to execute
    @param ps initial path-sum
    @param incr activate incremental reduction *)

type execution_error =
  | EmptyTargetList of t
  | InvalidGateApplication of int * t
  | InputStateTooSmall of int * int
  | HybridProgram of t
  | NonDyadicRotationAngle of t

let execution_error_message = function
  | EmptyTargetList p ->
      sprintf "Program.execution_aux.Apply, empty target list, p = %s"
        (String.pretty p)
  | InvalidGateApplication (width, p) ->
      sprintf "Program.execution_aux.Apply, width = %d, p =%s" width
        (String.pretty p)
  | InputStateTooSmall (program_width, input_width) ->
      sprintf
        "Program.execution, input state width %d is smaller than program width \
         %d"
        input_width program_width
  | HybridProgram p ->
      sprintf "Program.execution_aux, p = %s forbidden" (String.pretty p)
  | NonDyadicRotationAngle p ->
      sprintf "Program.execution.GP/U1, non-dyadic angle, p = %s"
        (String.pretty p)

let execution_result ?(debug = false) ?(input_state = Path_sum.ofSize 0) p =
  let _, wq = widths p in
  let input_width = Array.length input_state.ket in
  let width = Int.max wq input_width in

  let canonical_rotation_angle (s : Q.t) (k : int) : Q.t option =
    let angle = if k < 0 then Q.mul_2exp s (-k) else Q.div_2exp s k in
    let denominator = Q.den angle in
    if
      Z.gt denominator Z.zero
      && Z.(equal (logand denominator (denominator - one)) zero)
    then
      (* Phase coefficients are modulo 1. The non-negative remainder gives the
         representative in [0, 1), including for negative angles. *)
      Some (Q.make (Z.erem (Q.num angle) denominator) denominator)
    else None
  in

  let execution_aux (p : t) (ps : Path_sum.t) =
    let rec apply_forall apply (ps : Path_sum.t) co (ta : int list) =
      match ta with
      | i :: [] -> apply ps co i
      | i :: ta' -> apply_forall apply (apply ps co i) co ta'
      | [] -> ps
    in

    let rec conflict_co_ta co ta =
      match co with
      | i :: _ when List.mem i ta -> true
      | _ :: co' -> conflict_co_ta co' ta
      | _ -> false
    in

    let error_apply co ta =
      let invalid_indices =
        ListBis.min_int co < 0
        || width <= ListBis.max_int co
        || ListBis.min_int ta < 0
        || width <= ListBis.max_int ta
      in
      invalid_indices || conflict_co_ta co ta
    in

    let rec aux p ps =
      if debug then
        printf "Program.execution_aux.aux, p =\n%s\n" (String.pretty p);
      if debug then
        printf "Program.execution_aux.aux, ps =\n%s\n\n" (PSS.pretty ps);
      match p with
      | Apply ((H | X | U1 _), _, []) ->
          Error (EmptyTargetList p)
      | Apply (_, co, ta) when error_apply co ta ->
          Error (InvalidGateApplication (width, p))
      | Measure _ | It _ | InitQ _ | Not _ -> Error (HybridProgram p)
      | Apply (H, co, ta) -> Ok (apply_forall Apply_gates.apply_hadamard ps co ta)
      | Apply (X, co, ta) -> Ok (apply_forall Apply_gates.apply_not ps co ta)
      | Apply (GP (s, k), co, _) -> (
          match canonical_rotation_angle s k with
          | None -> Error (NonDyadicRotationAngle p)
          | Some angle when Q.equal angle Q.zero -> Ok ps
          | Some angle ->
              (* GP is targetless: targets are tolerated in Program.t but ignored. *)
              Ok (Apply_gates.apply_gp angle ps co))
      | Apply (U1 (s, k), co, ta) -> (
          match canonical_rotation_angle s k with
          | None -> Error (NonDyadicRotationAngle p)
          | Some angle when Q.equal angle Q.zero -> Ok ps
          | Some angle ->
              if debug then
                printf "Program.execution.Apply U1, p = %s\n\n%!" (String.exact p);
              Ok (apply_forall (Apply_gates.apply_u1 ~debug angle) ps co ta))
      | E -> Ok ps
      | Sequence (p1, p2) -> (
          match aux p1 ps with Error error -> Error error | Ok ps' -> aux p2 ps')
    in
    aux p ps
  in

  if debug then printf "Program.execution, width = %d\n\n" width;

  if width < 1 then Ok (Path_sum.ofSize 0)
  else if p = E || p = Sequence (E, E) then Ok input_state
  else if input_state = Path_sum.ofSize 0 then
    execution_aux (format p) (Path_sum.ofSize width)
  else if input_width < wq then Error (InputStateTooSmall (wq, input_width))
  else execution_aux (format p) input_state

let execution ?debug ?input_state p =
  match execution_result ?debug ?input_state p with
  | Ok path_sum -> path_sum
  | Error error -> failwith (execution_error_message error)

type inverse_error = NonReversibleProgram of t

let inverse_result ?(debug = false) (p : t) : (t, inverse_error) result =
  let rec aux (p : t) : (t, inverse_error) result =
    if debug then printf "Program.aux\n";
    match p with
    | Sequence (p1, p2) -> (
        match (aux p2, aux p1) with
        | Ok p2_inv, Ok p1_inv -> Ok (Sequence (p2_inv, p1_inv))
        | Error error, _ | _, Error error -> Error error)
    | Apply (U1 (s, k), co, ta) -> Ok (Apply (U1 (Q.neg s, k), co, ta))
    | Apply (GP (s, k), co, ta) -> Ok (Apply (GP (Q.neg s, k), co, ta))
    | Measure _ | It _ | InitQ _ | Not _ ->
        Error (NonReversibleProgram p)
    | _ -> Ok p
  in
  match aux p with Ok p_inv -> Ok (format p_inv) | Error error -> Error error

let inverse ?(debug = false) (p : t) : t =
  match inverse_result ~debug p with
  | Ok p_inv -> p_inv
  | Error (NonReversibleProgram p) ->
      failwith
        (sprintf "Program.aux, p = %s isn't reversible" (String.pretty p))

let create_state ?(circuit = E) (width : int) (inits_0 : int list) =
  execution ~input_state:(Path_sum.ofSize_init width inits_0) circuit

let unitary p =
  let rec aux p =
    match p with
    | Measure _ | It _ | InitQ _ | Not _ -> false
    | Sequence (p1, p2) -> aux p1 && aux p2
    | _ -> true
  in
  aux p

let to_list p =
  let rec aux p =
    match p with Sequence (p1, p2) -> p1 :: aux p2 | _ -> [ p ]
  in
  aux p

(* Returns the list of initialized qubits *)
let rec inits (p : t) =
  match p with
  | InitQ i -> [ i ]
  | Sequence (p1, p2) -> inits p1 @ inits p2
  | _ -> []

let inits_meas (p : t) =
  let rec aux inits_acc meas_acc p =
    match p with
    | InitQ i -> aux (i :: inits_acc) meas_acc E
    | Measure (i, _) -> aux inits_acc (i :: meas_acc) E
    | Sequence (p1, p2) ->
        let inits1_acc, meas1_acc = aux inits_acc meas_acc p1 in
        aux inits1_acc meas1_acc p2
    | _ -> (List.rev inits_acc, List.rev meas_acc)
  in
  aux [] [] p

module Macros = struct
  let ( -- ) p1 p2 : t = Sequence (p1, p2)
  let id : t = E
  let h ta : t = Apply (H, [], [ ta ])
  let x ta : t = Apply (X, [], [ ta ])
  let notC ta : t = Not ta

  let notCl l : t =
    let rec aux l = match l with ta :: l' -> notC ta -- aux l' | [] -> E in
    format (aux l)

  let cx co ta : t = Apply (X, [ co ], [ ta ])
  let cy _ _ : t = failwith "Control Ry doesn't implemented"
  let ccx co1 co2 ta : t = Apply (X, [ co1; co2 ], [ ta ])
  let toffoli co1 co2 ta : t = ccx co1 co2 ta
  let m k ta : t = Measure (k, ta)
  let it k p : t = It ([ k ], p)
  let itl l p : t = It (l, p)

  (* l1 : applies the gate if the control is set to 1 *)
  (* l0 : applies the gate if the control is 0 *)
  let itl2 l0 l1 p : t = notCl l0 -- itl (l0 @ l1) p -- notCl l0
  let iq0 ta : t = InitQ ta

  (* swap 1 2 : |x1,x2> -> |x2,x1> *)
  let swap ta1 ta2 : t = cx ta1 ta2 -- cx ta2 ta1 -- cx ta1 ta2

  (* cswap 1 2 3 : |x1,x2,x3> -> |x1, x2+x1(x2+x3), x3+x1(x2+x3)>*)
  let cswap co ta1 ta2 : t = ccx co ta1 ta2 -- ccx co ta2 ta1 -- ccx co ta1 ta2
  let fredkin co ta1 ta2 : t = cswap co ta1 ta2

  (* \( gp s k  : |x0> -> e^{2.pi.i. s / 2^k} |x0> \) *)
  let gp ?(s = 1) k : t =
    if k < 0 then failwith "Macros.gp, k < 0, forbidden";
    Apply (GP (Q.of_int s, k), [], [])

  let cgp co ?(s = 1) k : t =
    if k < 0 then failwith "Macros.gp, k < 0, forbidden";
    Apply (GP (Q.of_int s, k), [ co ], [])

  (* \( u1 s k  : |x0> -> e^{2.pi.i. x0.s / 2^k} \) |x0> *)
  let u1 ?(s = 1) k ta : t =
    if k < 0 then failwith "Macros.u1, k < 0, forbidden";
    Apply (U1 (Q.of_int s, k), [], [ ta ])

  let zz n : t = u1 1 n
  let zinv n : t = u1 ~s:(-1) 1 n
  let ss n : t = u1 2 n
  let sinv n : t = u1 ~s:(-1) 2 n
  let tt n : t = u1 3 n
  let tinv n : t = u1 ~s:(-1) 3 n

  (* \( rz s k  : |x0> -> e^{2.pi.i. (x0.s / 2^k - 1 / 2^{k+1})} |x0> \) *)
  let rz ?(s = 1) k ta : t =
    if k < 0 then failwith "Macros.rz, k < 0, forbidden";
    gp ~s:(-s) (k + 1) -- u1 ~s k ta

  (* \( rx s k : |x0> -> e^{2.pi.i. (x0.y0 / 2 + y0 / 2^k - 1 / 2^{k+1} + y0.y1 / 2)} |y2> \) *)
  let rx ?(s = 1) k ta : t =
    if k < 0 then failwith "Macros.rx, k < 0, forbidden";
    h ta -- rz ~s k ta -- h ta

  let ry ?(s = 1) k (ta : int) : t =
    if k < 0 then failwith "Macros.ry, k < 0, forbidden";
    x ta -- sinv ta -- rx ~s k ta -- ss ta -- x ta

  (* https://docs.quantum.ibm.com/api/qiskit/qiskit.circuit.library.SXGate *)
  let sx ta : t = gp 3 -- rx 2 ta

  let cu1 ?(s = 1) k co ta : t =
    if k < 0 then failwith "Macros.rz, k < 0, forbidden";
    Apply (U1 (Q.of_int s, k), [ co ], [ ta ])

  let ccu1 ?(s = 1) k co1 co2 ta : t =
    if k < 0 then failwith "Macros.ccu1, k < 0, forbidden";
    Apply (U1 (Q.of_int s, k), [ co1; co2 ], [ ta ])

  let crz ?(s = 1) k co ta : t = u1 ~s:(-s) (k + 1) co -- cu1 ~s k co ta
  let y n : t = gp 2 -- ry 1 n

  (* OpenQASM 2 et QBricks*)
  let cz co ta : t = cu1 1 co ta
  let czinv co ta : t = cu1 ~s:(-1) 1 co ta
  let ccz co1 co2 ta : t = ccu1 1 co1 co2 ta
  let cczinv co1 co2 ta : t = ccu1 ~s:(-1) 1 co1 co2 ta
  let cs co ta : t = cu1 2 co ta
  let csinv co ta : t = cu1 ~s:(-1) 2 co ta
  let ccs co1 co2 ta : t = ccu1 2 co1 co2 ta
  let ccsinv co1 co2 ta : t = ccu1 ~s:(-1) 2 co1 co2 ta
  let ct co ta : t = cu1 3 co ta
  let ctinv co ta : t = cu1 ~s:(-1) 3 co ta
  let cct co1 co2 ta : t = ccu1 3 co1 co2 ta
  let ch co ta : t = Apply (H, [ co ], [ ta ])

  (* Decomposition of CH in {u1, cx, h}. *)
  let chdecomp co ta : t =
    ss ta -- h ta -- tt ta -- h ta -- sinv ta -- cx co ta -- ss ta -- h ta
    -- tinv ta -- h ta -- sinv ta -- cx co ta

  let ccxoq2 co1 co2 ta : t =
    let output =
      h ta -- cx co2 ta -- tinv ta -- cx co1 ta -- tt ta -- cx co2 ta -- tinv ta
      -- cx co1 ta -- tt co2 -- tt ta -- h ta -- cx co1 co2 -- tt co1
      -- tinv co2 -- cx co1 co2
    in
    output

  (* V = H.S.H satisfies V^2 = X. *)
  let c3xdecomp co1 co2 co3 ta : t =
    let controlled_controlled_v s control1 control2 target =
      h target -- ccu1 ~s 2 control1 control2 target -- h target
    in
    controlled_controlled_v 1 co2 co3 ta
    -- ccx co1 co2 co3
    -- controlled_controlled_v (-1) co2 co3 ta
    -- ccx co1 co2 co3
    -- controlled_controlled_v 1 co1 co2 ta

  let crzdecomp ?(s = 1) k co ta : t =
    u1 ~s (k + 1) ta -- cx co ta -- u1 ~s:(-s) (k + 1) ta -- cx co ta

  let cu1decomp ?(s = 1) k co ta : t = u1 ~s (k + 1) co -- crzdecomp ~s k co ta

  let ccu1decomp ?(s = 1) k co1 co2 ta : t =
    cu1 ~s (k + 1) co1 co2
    -- cu1 ~s (k + 1) co1 ta
    -- ccx co1 co2 ta
    -- cu1 ~s:(-s) (k + 1) co1 ta
    -- ccx co1 co2 ta

  let xdecomp ta = h ta -- zz ta -- h ta

  (* QBricks *)
  let ccx_qb co1 co2 ta : t = h ta -- ccz co1 co2 ta -- h ta
  let h_qb ta : t = x ta -- ry 2 ta
  let h_qb2 ta : t = ry 3 ta -- x ta -- ry ~s:(-1) 3 ta
  let x_qb ta : t = h ta -- zz ta -- h ta
  let ry_not k ta : t = x ta -- ry k ta -- x ta
  let rz_not k ta : t = x ta -- rz k ta -- x ta
  let cch co1 co2 ta : t = Apply (H, [ co1; co2 ], [ ta ])

  let rec h_n' ta n =
    if n < 1 then failwith (Printf.sprintf "error in Program.h, n = %d" n)
    else if n = 1 then h ta
    else h ta -- h_n' ta (n - 1)

  let h_n ta n = h_n' ta n

  let rec x_n ta n =
    if n < 1 then failwith (Printf.sprintf "error in Program.h, n = %d" n)
    else if n = 1 then x ta
    else x ta -- x_n ta (n - 1)

  let rec rz_n k ta n =
    if n < 1 then failwith (Printf.sprintf "error in Program.h, n = %d" n)
    else if n = 1 then rz k ta
    else rz k ta -- rz_n k ta (n - 1)

  let rec cx_n co ta n =
    if n < 1 then failwith (Printf.sprintf "error in Program.h, n = %d" n)
    else if n = 1 then cx co ta
    else cx co ta -- cx_n co ta (n - 1)

  let s_n ta n = rz_n 2 ta n
  let t_n ta n = rz_n 3 ta n

  type apply_swap_error = InvalidSwapPlace | DifferentSwapLengths

  (* [sources] and [destinations] describe a wire mapping pair by pair:

       sources      = [source_0; source_1; ...]
       destinations = [destination_0; destination_1; ...]

     The logical value initially at [source_i] must finish at [destination_i].
     The lists used by Equiv contain no duplicates. Their lengths are checked
     below; a direct caller must respect the same uniqueness invariant.

     Overlapping lists require tracking current positions. For example, the
     mapping [0; 1] -> [1; 2] means:

       initial ket       = |x0, x1, x2>
       swap 0 1          = |x1, x0, x2>
       swap 0 2          = |x2, x0, x1>

     After [swap 0 1], logical value [x1] is no longer on source wire 1: it is
     on wire 0. The remaining source list must therefore change from [1] to
     [0] before selecting the second swap.

     Pairing the original indices independently would instead emit
     [swap 0 1; swap 1 2] and produce |x1, x2, x0>. Neither requested logical
     value would reach its destination. *)

  let update_remaining_sources source destination remaining_sources =
    (* A swap exchanges both physical positions. Therefore, every pending
       logical source currently located at [destination] moves to [source].
       Other pending source positions are unchanged. *)
    List.map
      (fun current_source ->
        if Int.equal current_source destination then source else current_source)
      remaining_sources

  (* Build the swaps in execution order without attaching them to the user
     program yet. Keeping these two operations separate is important for
     [place = "before"]: repeatedly prefixing swaps would reverse their order.

     An invalid placement is reported only when a non-identity swap is needed,
     preserving the historical behavior for an empty or identity mapping. *)
  let rec swaps_for_mapping_result place sources destinations =
    match (sources, destinations) with
    | [], [] -> Ok []
    | source :: remaining_sources, destination :: remaining_destinations ->
        if Int.equal source destination then
          swaps_for_mapping_result place remaining_sources remaining_destinations
        else if place <> "after" && place <> "before" then Error InvalidSwapPlace
        else
          let updated_sources =
            update_remaining_sources source destination remaining_sources
          in
          (match
             swaps_for_mapping_result place updated_sources
               remaining_destinations
           with
          | Error error -> Error error
          | Ok remaining_swaps ->
              Ok (swap source destination :: remaining_swaps))
    | _ -> Error DifferentSwapLengths

  (* Insert the complete permutation around [p] while preserving the order
     returned by [swaps_for_mapping_result].

     - [after] uses a left fold to build [p; swap_0; swap_1; ...].
     - [before] uses a right fold to build [swap_0; swap_1; ...; p].

     In both cases, [swap_0] is executed before [swap_1]. *)
  let apply_swap_result ?(place = "after") p sources destinations =
    match swaps_for_mapping_result place sources destinations with
    | Error error -> Error error
    | Ok [] -> Ok p
    | Ok swaps when place = "after" ->
        Ok
          (List.fold_left
             (fun program swap_program -> program -- swap_program)
             p swaps)
    | Ok swaps when place = "before" ->
        Ok
          (List.fold_right
             (fun swap_program program -> swap_program -- program)
             swaps p)
    | Ok _ -> Error InvalidSwapPlace

  let apply_swap ?place p targets1 targets2 =
    match apply_swap_result ?place p targets1 targets2 with
    | Ok p -> p
    | Error InvalidSwapPlace -> failwith "Program.equiv, place forbidden"
    | Error DifferentSwapLengths ->
        failwith "Macros.apply_swap, targets1.length <> targets2.length"

  let apply_measure p targets wc =
    let rec aux p targets ic =
      match targets with
      | ta :: [] -> p -- m ta ic
      | ta :: targets_remain -> aux (p -- m ta ic) targets_remain (ic + 1)
      | [] -> p
    in
    aux p targets wc

  let apply_inits p targets =
    List.fold_left (fun acc ta -> iq0 ta -- acc) p targets

  let cs_feynman co ta : t = tt co -- tt ta -- cx co ta -- tinv ta -- cx co ta
  let cz_feynman co ta : t = ss co -- ss ta -- cx co ta -- sinv ta -- cx co ta

  let ccz_feynman co1 co2 ta : t =
    tt co1 -- tt co2 -- tt ta -- cx co1 co2 -- cx co2 ta -- cx ta co1
    -- tinv co1 -- tinv co2 -- tt ta -- cx co2 co1 -- tinv co1 -- cx co2 ta
    -- cx ta co1 -- cx co1 co2

  let ccx_feynman co1 co2 ta : t = h ta -- ccz co1 co2 ta -- h ta
end
