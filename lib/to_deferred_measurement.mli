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

(** This module translates quantum programs to deferred measurement form, where
    all measurements are moved to the end of the program. *)

type deferred_measurement_error =
  | InvalidClassicalBit of int * int
      (** A classical bit is outside the computed classical width. The integers
          are [(width, bit)]. *)
  | InvalidQubitIndex of int * int
      (** A qubit index is outside the computed quantum width. The integers are
          [(width, qubit)]. *)
  | ResetOfUsedQubitUnsupported of int
      (** A reset/initialization targets a qubit that was already used.
          SQbricks currently supports [InitQ] only for fresh qubits, not as
          dynamic reset or discard/reuse. *)
  | MeasuredQubitUsedAfterMeasurement of Program.t
      (** A quantum operation uses a qubit after it has been measured. *)
  | UnsupportedConditionalProgram of Program.t
      (** The conditional program shape is not supported by this translation. *)

val deferred_measurement_error_message : deferred_measurement_error -> string
(** [deferred_measurement_error_message error] explains why deferred
    measurement translation failed. *)

val to_deferred_measurements_result :
  ?debug:bool ->
  Program.t ->
  (Program.t * int list * int list, deferred_measurement_error) result
(** [to_deferred_measurements_result ?debug prog] converts [prog] to deferred
    measurement form, or reports the first unsupported or malformed construct.
    Classical bits are initially zero; a measurement replaces that constant
    value with the corresponding deferred quantum control. *)

val to_deferred_measurements :
  ?debug:bool -> Program.t -> Program.t * int list * int list
(** [to_deferred_measurements ?debug prog] converts [prog] to deferred
    measurement form. Returns:
    - A transformed program without measurement
    - List of initialized qubits
    - List of measured qubits

    Example:
    {[
      to_deferred_measurements (m 0 1)  (* Original program with measurement *)
      -> (E, [], [0])
    ]}

    {b Note}: The transformation preserves program semantics while restructuring
    measurement operations. It is the historical wrapper around
    [to_deferred_measurements_result] and raises [Failure] on translation
    errors. *)
