# SQbricks: technical documentation

[Français](SQbricks.md) | [English](SQbricks.en.md)

## Purpose

This document complements the [`README.md`](../README.md). The README presents
installation and the main commands; this document describes the SQbricks
architecture, the stable contracts of its subsystems, and their known limits.

The `odoc` comments, `.mli` interfaces, and tests remain the source of truth
for function-level details. Proofs, commit studies, and experimental
measurements are outside the scope of this public documentation.

## Overview

SQbricks is a research prototype for verifying hybrid quantum circuits. A
program may combine quantum gates, measurements, initializations, and classical
control.

The main workflow has two parts:

- **SQbricks-Lift (SQL)** transforms a hybrid circuit by deferring measurements
  and isolating a usable unitary representation;
- **SQbricks-Verif (SQV)** symbolically executes two unitary programs, reduces
  their path sums, and attempts to establish their equivalence.

The core uses `Program.t` to represent circuits. Qubit and classical-bit
indices are flat integers. Symbolic execution produces a `Path_sum.t`, then
`Reduction_algorithm` applies reduction rules before the equivalence decision.

The two equivalence algorithms are:

- `Equiv.seq`, which composes the first circuit with the inverse of the second;
- `Equiv.parallel`, which executes and reduces the two circuits separately
  before comparing their path sums.

`Equiv.seq` therefore requires the inverted circuit to be reversible. Parallel
mode also applies to comparisons where this inversion is not possible.

## Provenance and references

SQbricks was first developed in the
[Qbricks repository](https://github.com/Qbricks/qbricks.github.io), under
`Artifacts/SQbricks/SQbricks`. The current repository places SQbricks at its
root while preserving the subproject history, copyright notices, and LGPL 2.1
license.

The main references are:

- Jérôme Ricciardi, Sébastien Bardin, Christophe Chareton, and Benoît Valiron,
  [*Quantum Circuit Equivalence Checking: A Tractable Bridge From Unitary to
  Hybrid Circuits*](https://arxiv.org/abs/2511.22523), 2025;
- Jérôme Ricciardi,
  [*Practical verification of quantum circuit
  transformations*](https://theses.hal.science/tel-05681895v1/document), PhD
  thesis, 2026.

The inspection prototype uses Quantikz2. Its reference is Alastair Kay,
[*Tutorial on the Quantikz Package*](https://arxiv.org/abs/1809.03842).
[`CITATION.cff`](../CITATION.cff) provides the project citation metadata.

## Non-regression benchmarks

SQbricks provides three benchmark levels that do not invoke the external
verification tools used by the historical benchmark:

| Level | Purpose | Main command |
| --- | --- | --- |
| Light | short functional and performance guard | `make regression-light-check` |
| Selected large | costlier cases near known resource boundaries | `make regression-large-check` |
| Long | SQbricks-only campaign over historical families | `make benchmarks-sqbricks` |

The `qiskit-hybrid` and `owm-vs-qiskit` families still use Qiskit to produce a
transformed circuit. Qiskit is not used as a verifier and is not reported as a
verification result in the SQbricks-only CSV files.

### Light benchmark

The entry points are:

| Command | Role |
| --- | --- |
| `make regression-light` | run the cases and write the current CSV |
| `make regression-light-baseline` | regenerate `benchmarks/baseline/light.csv` |
| `make regression-light-check` | compare a run with this baseline |
| `make tests_regression_light` | test the runner with a fake SQbricks executable |

Cases are described in:

- `scripts/paths/light/pairs.csv` for direct circuit comparisons;
- `scripts/paths/light/transforms.csv` for circuits transformed before the
  comparison.

The manifest is the functional oracle. Each enabled mode must preserve its
expected status: `EQ`, `NE`, `NC`, or an explicit failure status. A successful
but empty or unknown output becomes `UNEXPECTED_OUTPUT` and fails the check.

Rows selected for performance tracking run for three rounds by default. The
best observed time is compared with the baseline. A regression is reported
only when both the relative `SQBRICKS_LIGHT_PERF_THRESHOLD` and the absolute
`SQBRICKS_LIGHT_MIN_SLOWDOWN_SECONDS` thresholds are exceeded. Very short
times, below `SQBRICKS_LIGHT_MIN_PERF_SECONDS`, do not provide a useful
measurement.

Performance depends on the machine. A baseline must therefore be regenerated
after an intentional change to the manifest, timing policy, or reference
environment.

### Selected large regression

The selected large regression reuses `scripts/benchmarks-sqbricks.sh` with the
shorter lists under `scripts/paths/regression-large/`. Its baselines are stored
per family in `benchmarks/baseline/regression-large/`.

The check verifies:

- that expected baseline rows are still present;
- that no known functional capability is lost;
- that a time does not exceed both the relative and absolute thresholds.

A functional improvement is displayed without failing the check. For families
ordered by size, the selection keeps several cases around the resource
boundary so that a fluctuation on the largest case does not hide the whole
family.

### Long benchmark

`scripts/benchmarks-sqbricks.sh` runs one family with:

```text
make benchmark-sqbricks TYPE=owm
```

`make benchmarks-sqbricks` iterates over all families in `LONG_TYPES`. Each
family writes a separate CSV under `benchmarks/result/<month>/`.

By default, the runner applies a 600-second CPU limit and an approximately
6-GiB memory limit to each process. They can be configured with
`SQBRICKS_LONG_TIMEOUT` and `SQBRICKS_LONG_MEMORY_KB`.

Within an ordered series, Sequence and Parallel are tracked independently.
After a timeout or out-of-memory result, larger cases are skipped only for the
affected mode and receive `SKIP_AFTER_RESOURCE_FAILURE`. A conversion resource
failure stops both modes in that series, but not other families.

Progress bars are written to `stderr`, while CSV data is written to `stdout`.
They therefore do not contaminate redirected result files.

## OpenQASM parser and exporter

The OpenQASM parser translates circuits to `Program.t`. Multiple named
registers are flattened into separate quantum and classical index spaces. Each
declaration receives a global offset, so `q[i]` becomes `offset(q) + i`.

An OpenQASM condition `if (c == n)` preserves the size and offset of register
`c`. Index zero is the least significant bit. The translation distinguishes
bits expected to be zero from bits expected to be one in order to preserve the
condition exactly. A value that does not fit the register or the SQbricks
integer representation is rejected explicitly.

Some malformed inputs from benchmark libraries are accepted with a warning so
that external datasets do not need to be modified. This tolerance does not
make the circuit well formed, and later stages may still reject it.

Current compatibility limits are:

- `include "...";` is accepted, but the referenced file is not loaded;
- `OPENQASM 3.0;` enables only the legacy subset already understood by the
  parser, not general OpenQASM 3 support;
- `barrier ...;` is treated as a no-op up to the next semicolon;
- angle denominators must be powers of two;
- unknown gates and constructs without SQbricks semantics are rejected.

File input channels are closed after both successful parsing and parsing
exceptions.

During export, `one_creg=true` groups the classical bits without removing the
quantum register or circuit. Applications with several targets are split and
then validated one target at a time. An unsupported combination is rejected
explicitly.

`Program.Apply` semantics accepts an arbitrary control list. For the current
OpenQASM/OWM subset, `Program.Macros.c3xdecomp` provides an exact decomposition
of a three-controlled X gate without ancillas or global phase. H gates with
several controls remain symbolically executable but do not yet have a general
OpenQASM decomposition.

## Deferred measurement translation

`To_deferred_measurement.to_deferred_measurements_result` transforms a hybrid
program into a program without intermediate measurements. It returns:

- the translated program;
- initialized qubits;
- measured qubits;
- or a typed error.

Classical bits are initialized to zero. A measurement replaces this value with
the qubit carrying the deferred result. `Not` negates the condition, a known
false condition is removed, and a partially measured condition retains only
the quantum controls that are still required.

A classical bit may be reused: the next control depends on its latest
measurement. The complete measured-qubit list is nevertheless preserved so
that reusing the classical bit does not erase quantum history.

`InitQ` is accepted for a fresh qubit, notably for ancillas introduced by OWM.
Resetting a qubit that has already been used is not supported yet.

The public errors are:

- `InvalidClassicalBit`;
- `InvalidQubitIndex`;
- `ResetOfUsedQubitUnsupported`;
- `MeasuredQubitUsedAfterMeasurement`;
- `UnsupportedConditionalProgram`.

`to_deferred_measurements` remains the historical wrapper. It returns the same
triple but raises `Failure` when a typed error is encountered.

## Equivalence checking

### Metadata and well-formedness

The `inputs`, `outputs`, and `meas` lists describe the logical correspondence
between the two circuits. Their indices must be valid and their lengths
compatible. Measured outputs are compared by logical position, not by equality
of their physical indices.

Before symbolic execution, `Equiv` distinguishes:

- incompatible metadata, reported by `NotEquivDiffInputs`,
  `NotEquivDiffOutputs`, `NotEquivDiffInputsOutputs`, or
  `ErrorInvalidQubitIndex`;
- remaining hybrid constructs, reported by `ErrorCircuitNotUnitary`;
- malformed unitary gate applications, reported by `ErrorInvalidProgram`.

For `H`, `X`, and `U1`, a target is required, every index must lie within the
circuit width, and controls must be distinct from targets. Effective `GP` and
`U1` angles must be dyadic. They are normalized modulo one during symbolic
execution; the original `Program.t` is not rewritten.

### Path sums and reduction

`Program.execution_result` produces a path sum or a typed execution error.
`Reduction_algorithm.reduction_algorithm` then applies the reduction rules. A
rule that does not apply to a valid path sum is distinguished from a malformed
path sum; the latter reaches `Equiv` as `ErrorMalformedPathSum`.

Qubit, monomial, polynomial, ket, and path-sum comparisons use `*_result`
functions when metadata inconsistency must be distinguished from genuine
inequality.

Input variables preserve their indices. Path variables may be bijectively
renamed between two kets. The same bijection is then reused to compare phases;
an incomplete table is treated as a malformed path sum.

Variable-change reduction currently recognizes a limited affine form: one path
variable XOR a constant and input variables, without products or another path
variable in the offset. The substitution is applied to the whole phase and ket
only when it isolates more output components. This restriction is a limit of
the current implementation, not of the general mathematical change of
variable.

## Inspection prototype

`scripts/inspect-sqbricks.sh` orchestrates existing commands to inspect a
comparison between two QASM files.

- `--mode auto` uses the automatic `-sq` workflow;
- `--mode manual` uses `-sqv` with explicit metadata.

Results are written by default under `_tmp/inspection/<timestamp>/`. The main
artifacts are:

- `report.txt`, an execution summary;
- `commands.sh`, replayable commands;
- `sqv.stdout` and `sqv.stderr`, the complete trace;
- input path sums and extracted final path sums;
- LaTeX sources and, when possible, Quantikz2 PDFs for circuits and path sums.

Path-sum extraction still depends on the debug text format. Circuit export
supports a simple OpenQASM 2 subset and limits circuit size to avoid unreadable
PDFs. Thresholds are configured with the `SQBRICKS_INSPECT_*` variables
described in the script.

The planned next step is a graphical interface for loading two circuits,
editing metadata, choosing automatic or manual mode, running SQV, and browsing
the generated artifacts.

## Where to find details

- Public OCaml interfaces: `lib/*.mli` files.
- Validated behavior: Alcotest suites under `test/`.
- Benchmark runners and formats: `scripts/benchmarks-light.sh`,
  `scripts/benchmarks-sqbricks.sh`, and `scripts/check-regression-large.sh`.
- Project planning: [`ROADMAP.md`](../ROADMAP.md) and
  [`TODO.md`](../TODO.md).
