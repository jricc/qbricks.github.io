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

(** Module for simplifying quantum-circuit path sums with the SQbricks
    reduction pipeline. *)

module PSS = Path_sum.String

val reduction_algorithm :
  ?debug:bool -> Path_sum.t -> (Path_sum.t, Rules.reduction_error) result
(** [reduction_algorithm ?debug ps] applies the configured reduction
    sequence and returns an explicit error when a rule receives a malformed
    path sum. The sequence is:

    - simplify the ket and phase;
    - apply HH and direct variable replacement;
    - apply variable-replacement factorization and affine ket conversion;
    - restart while these transformations make progress;
    - once they reach a fixed point, try Omega;
    - restart after an Omega match, or rename path variables after its final
      non-match.

    Omega is deliberately last because the cheaper existing reductions often
    eliminate its candidates first. For example, if the stable phase is
    [1/4 y0] and [y0] is absent from the ket, Omega replaces it with [1/8],
    removes [y0], and restarts the sequence. *)

(**/**)

val _condition_to_continue : ?debug:bool -> Path_sum.t -> Path_sum.t -> bool
(** Internal function for determining whether to continue reduction. Returns
    [true] if the output state is simpler than input. *)
