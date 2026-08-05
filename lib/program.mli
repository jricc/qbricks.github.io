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

(** This module provides types and functions for working with quantum programs
    in the context of path sum calculations for quantum circuit verification. *)

(** {1 Quantum Programs} *)

(** Type of quantum programs, defined recursively as follows:
    - {b E}: Empty program.
    - {b Apply(G, controls, targets)}: Applies gate [G] to [targets], controlled
      by qubits in [controls].
    - {b Sequence(P1, P2)}: Sequential composition of programs [P1] then [P2].
    - {b Measure(q, b)}: Measures qubit [q] and stores result in classical bit
      [b].
    - {b It(bits, P)}: Conditional execution of [P] if all bits in [bits] are
      [1].
    - {b InitQ(q)}: Initializes qubit [q] to |0⟩.
    - {b Not(b)}: Classical NOT on bit [b]. Note: Qubit and bit indices are
      integers. *)
type t =
  | E
  | Apply of Gates.t * int list * int list
  | Sequence of t * t
  | Measure of int * int
  | It of int list * t
  | InitQ of int
  | Not of int

(** {2 Basic Operations} *)

val format : t -> t
(** [format prog] transforms [prog] into a normal form where sequences are
    left-nested: [Sequence(Ins1, Sequence(Ins2, Ins3))] instead of
    [Sequence(Sequence(Ins1, Ins2), Ins3)]. *)

val widths : t -> int * int
(** [widths prog] returns (classical width, quantum width) of the program.
    It computes widths from the largest indices used by [prog]; it does not
    validate that gate applications are well formed.

    Example: [widths (Apply (H, [], [0]))] returns [(0, 1)]. *)

(** {2 String Conversion} *)

module String : sig
  val pretty : t -> string
  (** [pretty prog] converts a quantum program to a human-readable string.

      Example: [pretty (h 0)] returns ["h 0"]. *)

  val exact : t -> string
  (** [exact prog] returns the exact constructor representation.

      Example: [exact (h 0)] returns ["Apply (H,[],[0])"]. *)
end

(** {2 Program Execution} *)

type execution_error =
  | EmptyTargetList of t
      (** A target gate such as [H], [X], or [U1] has no target. *)
  | InvalidGateApplication of int * t
      (** A gate uses invalid indices or overlaps controls and targets.
          The integer is the execution width used for validation. *)
  | InputStateTooSmall of int * int
      (** The provided input path-sum is smaller than the program to execute.
          The integers are [(program_width, input_width)]. *)
  | HybridProgram of t
      (** A hybrid construct was sent to unitary symbolic execution. *)
  | NonDyadicRotationAngle of t
      (** A [GP] or [U1] angle has a denominator that is not a power of two.
          Negative angles remain valid when their denominator is dyadic. *)

val execution_result :
  ?debug:bool -> ?input_state:Path_sum.t -> t -> (Path_sum.t, execution_error) result
(** [execution_result ?debug ?input_state prog] runs [prog] on the optional
    [input_state] path-sum or an initialized path sum, or reports why execution
    cannot proceed.

    [H], [X], and [U1] applications require a non-empty target list.
    [Apply (GP _, controls, targets)] ignores [targets]: [GP] is semantically
    targetless, even when a target list is present in the program value.

    Dyadic [GP] and [U1] angles are normalized modulo one, from zero included
    to one excluded. Negative coefficients and negative exponents are therefore
    valid when their effective angle is dyadic. *)

val execution : ?debug:bool -> ?input_state:Path_sum.t -> t -> Path_sum.t
(** [execution ?debug ?input_state prog] is the historical wrapper around
    [execution_result]. It raises [Failure] on execution errors. *)

type inverse_error = NonReversibleProgram of t

val inverse_result : ?debug:bool -> t -> (t, inverse_error) result
(** [inverse_result ?debug prog] computes the inverse of [prog] or reports the
    first non-reversible subprogram. *)

val inverse : ?debug:bool -> t -> t
(** [inverse ?debug prog] computes the inverse of the quantum program.

    Example: [inverse (h 0 -- x 0 -- u1(1,2))] returns [u1(-1,2) -- x 0 -- h 0]
    (Hadamard is self-inverse). *)

(** {2 Program Analysis} *)

val unitary : t -> bool
(** [unitary prog] checks only that [prog] has no measurements or classical
    operations. It does not validate gate indices, target lists, control/target
    overlaps, or whether rotation angles are dyadic.

    Example: [unitary (h 0)] returns [true]. *)

val to_list : t -> t list
(** [to_list prog] decomposes [prog] into a list of components.

    Example: [to_list (h 0 -- x 1)] returns [h 0; x 1]. *)

val inits : t -> int list
(** [inits prog] returns initialized qubits in [prog].

    Example: [inits (iq0 0)] returns [0]. *)

val inits_meas : t -> int list * int list
(** [inits_meas prog] returns (initialized qubits, measured qubits).

    Example: [inits_meas (iq0 0 -- m 1 1)] returns ([0], [1]). *)

val nb_gate : t -> int

val nb_gate_and_gates_decomposition :
  t -> int * int * int * int * int * int * int
(** [nb_gate_and_gates_decomposition prog] returns gate counts: (nb_ch, nb_cs,
    nb_cz, nb_ccz, nb_ccx, nb_cu1, total_gates). *)

(** {2 State Creation} *)

val create_state : ?circuit:t -> int -> int list -> Path_sum.t
(** [create_state ?circuit width qubits] creates a quantum state.

    It's an alias for
    [execution ~input_state:(Path_sum.ofSize_init width inits_0) circuit] *)

(** {1 Gate Constructors} *)

module Macros : sig
  val ( -- ) : t -> t -> t
  (** [P1 -- P2] is the infix notation for [Sequence(P1,P2)] *)

  val id : t
  (** Alias for empty program). *)

  (** {2 Single Qubit Gates} *)

  val h : int -> t
  (** [h target] Hadamard gate on target qubit. *)

  val x : int -> t
  (** [x target] Pauli X gate on target qubit. *)

  val notC : int -> t
  (** [notC target] Classical NOT on target bit. *)

  val notCl : int list -> t
  (** [notCl bits] Classical NOT on multiple bits. *)

  val zz : int -> t
  (** [zz target] Pauli Z gate on target. *)

  val zinv : int -> t
  (** [zinv target] Inverse Pauli Z gate. *)

  val ss : int -> t
  (** [ss target] S gate on target. *)

  val sinv : int -> t
  (** [sinv target] Inverse S gate. *)

  val tt : int -> t
  (** [tt target] T gate on target. *)

  val tinv : int -> t
  (** [tinv target] Inverse T gate. *)

  val sx : int -> t
  (** [sx target] SX gate (sqrt(X) gate). *)

  val y : int -> t
  (** [y target] Y gate (implemented as GP-ry). *)

  val rz : ?s:int -> int -> int -> t
  (** [rz ?s target k] Z-rotation with angle s*2π/2^k. *)

  val rx : ?s:int -> int -> int -> t
  (** [rx ?s target k] X-rotation with angle s*2π/2^k. *)

  val ry : ?s:int -> int -> int -> t
  (** [ry ?s target k] Y-rotation with angle s*2π/2^k. *)

  val gp : ?s:int -> int -> t
  (** [gp ?s k] Global phase gate with angle s*2π/2^k. *)

  val u1 : ?s:int -> int -> int -> t
  (** [u1 ?s target k] Phase gate with angle s*2π/2^k. *)

  (** {2 Initialization} *)

  val iq0 : int -> t
  (** [iq0 target] Initializes qubit [target] to |0⟩. *)

  (** {2 Measurement} *)

  val m : int -> int -> t
  (** [m qubit bit] Measures qubit into bit. *)

  (** {2 Controlled Gates} *)

  val cx : int -> int -> t
  (** [cx control target] CNOT gate. *)

  val cy : 'a -> 'b -> t
  (** [cy control target] Controlled Y gate (not implemented). *)

  val ch : int -> int -> t
  (** [ch control target] Controlled Hadamard. *)

  val chdecomp : int -> int -> t
  (** [chdecomp control target] CH gate decomposition. *)

  val cz : int -> int -> t
  (** [cz control target] Controlled Z gate. *)

  val czinv : int -> int -> t
  (** [czinv control target] Inverse CZ gate. *)

  val cs : int -> int -> t
  (** [cs control target] Controlled S gate. *)

  val csinv : int -> int -> t
  (** [csinv control target] Inverse CS gate. *)

  val ct : int -> int -> t
  (** [ct control target] Controlled T gate. *)

  val ctinv : int -> int -> t
  (** [ctinv control target] Inverse CT gate. *)

  (** {2 Two-Control Gates} *)

  val ccx : int -> int -> int -> t
  (** [ccx c1 c2 target] Toffoli gate. *)

  val toffoli : int -> int -> int -> t
  (** [toffoli c1 c2 target] Toffoli gate (alias for ccx). *)

  val ccxoq2 : int -> int -> int -> t
  (** [ccxoq2 c1 c2 target] CCX gate with specific decomposition. *)

  val c3xdecomp : int -> int -> int -> int -> t
  (** [c3xdecomp c1 c2 c3 target] decomposes a three-controlled X gate. *)

  val ccz : int -> int -> int -> t
  (** [ccz c1 c2 target] CCZ gate. *)

  val cczinv : int -> int -> int -> t
  (** [ccz c1 c2 target] Inverse CCZ gate. *)

  val ccs : int -> int -> int -> t
  (** [ccs c1 c2 target] Controlled-controlled S gate. *)

  val ccsinv : int -> int -> int -> t
  (** [ccs c1 c2 target] Inverse CCS gate. *)

  val cct : int -> int -> int -> t
  (** [cct c1 c2 target] Controlled-controlled T gate. *)

  (** {2 Controlled Rotations} *)

  val cu1 : ?s:int -> int -> int -> int -> t
  (** [cu1 ?s control target k] Controlled phase gate. *)

  val ccu1 : ?s:int -> int -> int -> int -> int -> t
  (** [ccu1 ?s c1 c2 target k] Controlled-controlled phase gate. *)

  val crz : ?s:int -> int -> int -> int -> t
  (** [crz ?s control target k] Controlled Z-rotation. *)

  val crzdecomp : ?s:int -> int -> int -> int -> t
  (** [crzdecomp ?s control target k] CRZ gate decomposition. *)

  val cgp : int -> ?s:int -> int -> t
  (** [cgp control ?s target] Controlled global phase. *)

  (** {2 Feynman-Style Gates} *)

  val cs_feynman : int -> int -> t
  (** [cs_feynman control target] CS gate in Feynman style. *)

  val cz_feynman : int -> int -> t
  (** [cz_feynman control target] CZ gate in Feynman style. *)

  val ccz_feynman : int -> int -> int -> t
  (** [ccz_feynman c1 c2 target] CCZ gate in Feynman style. *)

  val ccx_feynman : int -> int -> int -> t
  (** [ccx_feynman c1 c2 target] CCX gate in Feynman style. *)

  (** {2 QBricks-Style Gates} *)

  val x_qb : int -> t
  (** [x_qb target] X gate in QBricks style (H-Z-H decomposition). *)

  val ccx_qb : int -> int -> int -> t
  (** [ccx_qb c1 c2 target] CCX gate in QBricks style. *)

  val h_qb : int -> t
  (** [h_qb target] Hadamard gate in QBricks style. *)

  val h_qb2 : int -> t
  (** [h_qb2 target] Alternative Hadamard in QBricks style. *)

  val xdecomp : int -> t
  (** [xdecomp target] X gate decomposition. *)

  val cu1decomp : ?s:int -> int -> int -> int -> t
  (** [cu1decomp ?s control target k] CU1 gate decomposition. *)

  val ccu1decomp : ?s:int -> int -> int -> int -> int -> t
  (** [ccu1decomp ?s c1 c2 target k] CCU1 gate decomposition. *)

  val ry_not : int -> int -> t
  val rz_not : int -> int -> t
  val cch : int -> int -> int -> t

  (** {2 Repeated Gate Application} *)

  val h_n : int -> int -> t
  (** [h_n ta n] Applies Hadamard gate n times to target [ta] *)

  val x_n : int -> int -> t
  (** [x_n ta n] Applies X gate n times to target [ta] *)

  val rz_n : int -> int -> int -> t
  (** [rz_n k ta n] Applies RZ(k) gate n times to target [ta] *)

  val cx_n : int -> int -> int -> t
  (** [cx_n co ta n] Applies CNOT gate n times *)

  val s_n : int -> int -> t
  (** [s_n ta n] Applies S gate n times to target [ta] *)

  val t_n : int -> int -> t
  (** [t_n ta n] Applies T gate n times to target [ta] *)

  (** {2 Utility Functions} *)

  val apply_inits : t -> int list -> t
  (** [apply_inits prog qubits] Adds initialization to qubits. *)

  val apply_measure : t -> int list -> int -> t
  (** [apply_measure prog qubits register] Adds measurements. *)

  type apply_swap_error = InvalidSwapPlace | DifferentSwapLengths
  (** Errors reported while constructing a wire permutation.

      - [InvalidSwapPlace] means [place] is neither ["before"] nor ["after"]
        when a non-identity swap is required.
      - [DifferentSwapLengths] means the source and destination lists do not
        describe the same number of logical wires. *)

  val apply_swap_result :
    ?place:string -> t -> int list -> int list -> (t, apply_swap_error) result
  (** [apply_swap_result ?place prog sources destinations] moves the logical
      value initially on [sources[i]] to [destinations[i]]. The two lists must
      have the same length and must not contain duplicates. Overlap between
      the source and destination lists is supported: current wire positions
      are updated after each selected swap. The complete permutation is placed
      after [prog] by default, or before it when [place = "before"]. *)

  val apply_swap : ?place:string -> t -> int list -> int list -> t
  (** [apply_swap ?place prog sources destinations] is the compatibility
      wrapper around {!apply_swap_result}. It raises [Failure] when the mapping
      cannot be constructed. *)

  (** {2 Conditional Execution} *)

  val it : int -> t -> t
  (** [it qubit prog] Conditional execution on single qubit. *)

  val itl : int list -> t -> t
  (** [itl qubits prog] Conditional execution on multiple qubits. *)

  val itl2 : int list -> int list -> t -> t
  (** [itl2 zero_bits one_bits prog] executes [prog] when every bit in
      [zero_bits] is [0] and every bit in [one_bits] is [1]. *)

  (** {2 Swap Operations} *)

  val swap : int -> int -> t
  (** [swap a b] Swaps two qubits. *)

  val cswap : int -> int -> int -> t
  (** [cswap control a b] Controlled swap. *)

  val fredkin : int -> int -> int -> t
  (** [fredkin control a b] Fredkin gate (controlled swap). *)
end

(** {1 Printing} *)

module Print : sig
  val pretty : t -> unit
  (** [pretty prog] prints program to stdout.

      Example: [pretty (h 0)] prints "h 0". *)

  val print_to_file : string -> t -> unit
  (** [print_to_file filename prog] writes program to file as OCaml module. *)
end
