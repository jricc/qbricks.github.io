# TODO

## Next: release 0.1.0

- [x] Declare the direct OCaml dependencies and test dependencies in the opam
  package metadata.
- [x] Make CI install dependencies from the package metadata.
- [x] Correct obsolete repository paths in `README.md` and add `CHANGELOG.md`.
- [x] Present 0.1.0 as a research prototype without a global performance-gain
  claim while the post-HH large benchmark remains inconclusive.
- [ ] Run the final unit tests and light-regression check on the release
  commit; both must pass.
- [ ] Run the selected large-regression check and review all functional
  statuses. Treat resource-bound timing variation as documented experimental
  uncertainty, not as evidence of a global performance gain.
- [ ] Create and push the `0.1.0` tag after the final checks and CI pass.

## Completed phase 4 correctness bugs

- [x] Make `Path_sum_library` gate constructors return a ket with the declared
  circuit width for arbitrary valid wires, and reject overlapping controls and
  targets.
- [x] Make `Program.Macros.apply_swap_result` implement the requested wire
  mapping when source and destination lists overlap.
- [x] Compare observable measurements in `Equiv` by corresponding output
  positions rather than by raw physical qubit indices.
- [x] Implement the consistent path-variable renaming documented for
  `Ket.equal_result` and used by `Path_sum.equal_result`.

## Completed phase 4 API and robustness bugs

- [x] Fix OpenQASM export with `one_creg=true` so it keeps the quantum register
  and circuit, and reject unsupported controlled gates without recursive
  expansion loops.
- [x] Align `ListBis.check_bounds` and the bound-extraction helpers with their
  documented return values and list order, after checking for external users.
- [x] Close parser input channels on both successful parses and parser errors.

## Later roadmap work

- [ ] On a dedicated `owm-multicontrol` branch, audit and correct the
  multi-controlled-gate pipeline used by `owm-vs-qiskit`: make the lowering
  before OWM explicit instead of relying implicitly on `To_openqasm`
  serialization, preserve exact semantics, and measure the gate-count growth.
- [ ] On a separate `reduction-performance` branch, locate and correct the
  large-benchmark slowdowns where the baseline and current gate counts are
  identical, notably in `owm`, `tele`, and `owm-vs-tele`, before changing their
  performance baselines.
- [x] Reduce repeated HH candidate scans on `owm/gf2^9mult_89_413`: profiling
  observed 3,596 `hh_aux` calls from 4 `HH.hh` calls in Sequence, and 1,798
  `hh_aux` calls from 3 `HH.hh` calls in Parallel. The validated phase prefilter
  preserves path-variable order and reduces the normalized Sequence-specific
  cost by approximately 16%.
- [x] Avoid two redundant polynomial simplifications during successive HH
  reductions. `hh_aux` already simplifies the substituted phase and ket, and
  the final phase simplification also handles an unsimplified `R`. On the
  focused uninstrumented `owm/gf2^9mult_89_413` case, Sequence decreased from
  `399.31 s` to `336.88 s` and Parallel from `232.86 s` to `203.17 s`.
- [ ] Profile the denominators and monomial counts received by `Poly.lift` on
  representative `1/4` and `1/8` cases before deciding whether to truncate
  dyadic lifts by degree.
- [ ] Recheck the optimized HH implementation with the large `owm` regression
  under stable machine conditions. The 2026-08-02 run improved the targeted
  `gf2` cases, but remained inconclusive because unrelated cases close to the
  600-second resource boundary became `OutOfMemory` or were skipped. Do not
  update the large baseline from that run.
- [ ] Make the large regression checker report a changed gate-count signature
  as a workload change instead of comparing its execution time with the old
  performance baseline.
- [ ] Add flexible unit-test runtime regression tracking that remains useful as
  new tests are added, instead of comparing only the total test-suite duration.
- [ ] Add OpenQASM export support for `H` gates with an arbitrary number of
  controls. Symbolic execution already supports them, but the OpenQASM 2
  exporter currently rejects them because it has no decomposition for them.
- [ ] Measure the performance impact of recursive variable substitution under
  `Qubit.Prod`; it unlocks additional reductions and equivalence proofs, but
  traversing larger qubit expressions may increase reduction time.
- [ ] Add real OpenQASM include handling instead of only accepting
  `include "...";` as a compatibility no-op.
- [ ] Turn the phase 9 inspection prototype into a graphical interface for
  loading two QASM files, editing metadata, choosing auto/manual mode, running
  SQV, and browsing generated artifacts.
- [ ] Replace the circuit images generated with Qiskit in the documentation
  with images produced by the SQbricks Quantikz2 PDF generator.

## Validated before phase 5

- [x] Make `HH` remove its matched path-variable pair atomically, and stop
  applying `Elim` as an independent reduction on canonical path sums. Audit
  the remaining calls after `HH` and variable-replacement factorisation.
- [x] Restrict `variable_replacement` to the proved form `y xor Q`, where `y`
  does not occur in `Q`.
- [x] Make `Path_sum.equal_result` require equal ket widths only when no
  explicit output lists are supplied.
- [x] Normalize negative rational phase coefficients modulo one with a direct
  Euclidean remainder.
- [x] Complete phase normalization in `Poly.Monome.simplify` by multiplying
  scalar factors exposed by recursive simplification before reduction modulo
  one.
- [x] Give OpenQASM `if (creg == value)` exact semantics by preserving bits
  equal to zero and one, and reject out-of-width values or oversized literals
  explicitly.
- [x] Preserve already collected variables in `Qubit.extract_path_var` and
  `Qubit.extract_var` when a constant node is visited.
- [x] Make `ListBis.remove` preserve list order.
- [x] Confirm that `ListBis.remove_list` has no internal caller, then remove it
  instead of retaining unused code.

## Done

- [x] Complete roadmap phase 4: step-by-step correctness and robustness fixes.
- [x] Recenter the fork on SQbricks, preserve the former full tree on
  `archive/full-qbricks-main`, and leave the original Qbricks repository
  untouched.
- [x] Start phase 4 bug fixes one issue at a time, with focused regression
  tests before or with each fix.
- [x] Make direct `Program.t` execution report non-dyadic angles explicitly,
  preserve valid negative angles, and normalize valid dyadic angles modulo one.
- [x] Treat negative rotation exponents in `Path_sum_library` as exact
  identities while keeping negative angle coefficients valid.
- [x] Restore a timed `Equiv.parallel` result for
  `owm-vs-qiskit/dqc_teleportation`, validated by the large regression check.
- [x] Reject missing baselines and incomplete performance samples in light
  check mode.
- [x] Refuse to write an invalid light baseline and replace valid baselines
  atomically.
- [x] Treat unrecognized or empty successful SQbricks output as
  `UNEXPECTED_OUTPUT`.
- [x] Consolidate `scripts/benchmarks-light.sh` in quality mode.
- [x] Simplify the light benchmark around functional status and tracked
  performance checks.
- [x] Review and validate the light benchmark entry points in `Makefile`.
- [x] Create the incremental SQbricks technical documentation in French and
  English.
- [x] Require both documentation versions to be updated after each function
  validated in quality mode.
- [x] Review the simplified light benchmark at workflow level.
- [x] Regenerate the local light baseline with the new three-round performance
  cases.
- [x] Run the light non-regression check successfully in a complete SQbricks
  development environment.
- [x] Complete roadmap phase 1: minimal regression benchmark.
- [x] Add long SQbricks-only benchmarks without external verification tools.
- [x] Make the light performance check less sensitive to local load spikes by
  using the best observed timing across three rounds.
- [x] Start the large regression path selection separately from the light
  runner.
- [x] Start the Equiv audit by replacing some uncontrolled parameter mismatch
  failures with explicit equivalence results.
- [x] Stop ordered long benchmark series after timeout or memory failure while
  keeping other series running.
- [x] Add baseline/check behavior to the selected large regression workflow.
- [x] Rerun and inspect the long SQbricks-only benchmark with ordered-series
  cutoff.
- [x] Complete roadmap phase 2: safe long benchmark runner.
- [x] Audit the phase 3 reduction target.
- [x] Audit the phase 3 equivalence-checking target.
- [x] Audit the phase 3 separation target.
- [x] Audit the phase 3 projection target.
- [x] Audit the phase 3 benchmark-scripts target.
- [x] Audit the phase 3 AST/Program, parser, deferred-measurement, and path-sum
  generation targets.
- [x] Complete roadmap phase 3: correctness audit.
- [x] Keep the reduction entry-point names short after validating the typed
  reduction results.
- [x] Route Equiv initial-state construction through
  `Path_sum.ofSize_init_result` so invalid initialization data becomes
  `ErrorInvalidQubitIndex`.
- [x] Route observable-qubit comparison in
  `Equiv.compare_inputs_with_identity` through `Qubit.equal_result` so malformed
  comparison metadata becomes
  `ErrorMalformedPathSum`.
- [x] Reject non-unitary `Program` constructs such as `InitQ` during Equiv
  parameter preparation before symbolic execution.
- [x] Reject malformed unitary gate applications in Equiv parameter preparation
  with `ErrorInvalidProgram`.
- [x] Reject empty targets for gates that need one in Equiv parameter
  preparation with `ErrorInvalidProgram`.
- [x] Keep `GP` targets valid in Equiv parameter preparation because they do
  not affect symbolic execution.
- [x] Reject controlled `GP` applications whose targets overlap controls, so
  their well-formedness constraints match other controlled gates.
- [x] Document the well-formed circuit constraints enforced by Equiv.
- [x] Keep malformed `GP/U1` programs printable so Equiv diagnostics can report
  `ErrorInvalidProgram`.
- [x] Route the sequential phase classification through `Poly.equal_result` so
  malformed phase comparisons become `ErrorMalformedPathSum`.
- [x] Route path-sum phase comparison through `Poly.equal_result` so malformed
  phase metadata is not reported as plain inequality.
- [x] Make Equiv separability checks validate ket width and output indices
  before extracting variables.
- [x] Add `Program.Macros.apply_swap_result` and use it from Equiv so swap
  preparation errors do not escape as `failwith`.
- [x] Add `Program.inverse_result` and use it from Equiv so non-reversible
  programs are reported explicitly.
- [x] Document and test that `Program.widths` computes classical and quantum
  index extents without validating full program well-formedness.
- [x] Count `CCZ` and `CCZinv` in the total gate count reported by
  `Program.nb_gate_and_gates_decomposition`.
- [x] Document and test that `Program.execution` tolerates `GP` targets but
  ignores them because `GP` is semantically targetless.
- [x] Add `Program.execution_result` with typed execution errors, and keep
  `Program.execution` as a compatibility wrapper.
- [x] Reject empty targets in direct `Program.execution_result` calls for
  target gates (`H`, `X`, and `U1`) so malformed programs do not execute as
  silent no-ops.
- [x] Document and test that `Program.unitary` is only a hybrid syntax check,
  not a full well-formedness validator.
- [x] Route Equiv symbolic execution through `Program.execution_result` so
  execution failures are converted to `Equiv.result` instead of escaping as
  `Failure`.
- [x] Reject negative indices in typed `Path_sum_library` gate constructors
  before they can become invalid `Var (-n)` path-sum nodes.
- [x] Fix `Path_sum_library.rx_result` and `ry_result` path-variable metadata
  so introduced variables are offset by the circuit width.
- [x] Fix `Parser_help.den_to_k` to compare Zarith denominators structurally
  instead of physically.
- [x] Treat OpenQASM `barrier ...;` as a no-op statement instead of a line
  comment that can hide following gates.
- [x] Fix OpenQASM register handling so declared `qreg`/`creg` names and
  offsets are preserved when translating to SQbricks indices.
- [x] Keep parsing malformed OpenQASM register widths and out-of-range register
  accesses with warnings, so external benchmark libraries are not modified.
- [x] Document that OpenQASM `include "...";` is currently accepted as a
  compatibility no-op, not loaded as an external gate library.
- [x] Add typed deferred-measurement errors so malformed hybrid programs are
  reported explicitly instead of escaping through uncontrolled exceptions.
- [x] Keep classical-bit reuse correct in deferred measurement by separating
  the current `bit -> measured qubit` mapping from the list of all measured
  qubits.
- [x] Allow `InitQ` on fresh ancilla qubits during deferred measurement, but
  reject reset/init on qubits that have already been used.
- [x] Start roadmap phase 9 with a CLI inspection prototype for two QASM files,
  auto/manual SQV runs, captured traces, input path-sums, final path-sums, and
  replayable commands.
- [x] Add prototype LaTeX/PDF path-sum export to the inspection workflow, with
  compact tables for phase monomials, output qubits, and path variables.
- [x] Add prototype Quantikz2 LaTeX/PDF circuit export to the inspection
  workflow for circuits below configurable size limits.
- [x] Add LaTeX packages to the Docker image so inspection PDFs can be built in
  the container.
- [x] Run Docker containers from the current worktree with a disposable
  `docker run --rm` invocation.

## Notes

- The light baseline is stored in `benchmarks/baseline/light.csv`.
- The manifest remains the functional oracle.
- The light check uses three complete rounds and best-observed timings.
- The light runner default timeout is `240s`.
- The technical documentation is maintained in `doc/SQbricks.md` and
  `doc/SQbricks.en.md`.
- The light baseline no longer stores a benchmark definition hash. If the
  manifest changes intentionally, regenerate the local baseline.
- Performance is tracked on multiple complementary cases per supported task
  family, selected from `benchmarks/result/benchmarks_Thesis.ods`; smaller cases
  remain functional status checks.
- The long SQbricks-only benchmark reuses the historical path files, including
  `qiskit-hybrid` and `owm-vs-qiskit`; it does not run external verification
  tools.
- The long runner writes `SKIP_AFTER_RESOURCE_FAILURE` only for the mode that
  reached `TO` or `OutOfMemory` in an ordered source-scoped series. A conversion
  resource failure stops both modes.
- SQbricks-only benchmark levels are now split into light, selected large
  regression, and full long benchmark.
- Vigilance: `Path_sum.equal_result` now has defensive phase-comparison errors
  (`IncompatiblePhaseWidths`, `IncompletePhasePathVariableMap`). They are
  propagated to Equiv, but they are not directly covered by a natural
  `Path_sum.equal_result` test yet because the current ket comparison does not
  expose the incomplete phase-mapping situation.
- The global benchmark audit compared the long SQbricks-only runner with
  `scripts/benchmarks.sh`, checked manifests, CSV shapes, Makefile entry
  points, and light runner validation tests.
- The large regression selection is in `scripts/paths/regression-large/`.
  It now has per-family baselines in `benchmarks/baseline/regression-large/`
  and check targets separate from the light benchmark.
- The large baseline intentionally keeps `tele/adder_n34 Sequence` and
  `owm-vs-tele/qft_29 Parallel` as `OutOfMemory`: their successful runs around
  `600s` are too close to the timeout to promote as stable capabilities.
- Size-ordered families in the large regression keep up to the three largest
  representatives, plus isolated watchlist cases.
- The current Equiv cleanup introduces typed reduction failures so malformed
  path sums are not confused with rules that simply do not apply. The reduction
  pipeline now uses typed results directly, including the public reduction
  entry point.
- Watch the behavior change in `Path_sum.substitute_result`: target path
  variables are now protected. This is intended, but it may expose a bug if an
  external caller relied on the old `except_path_var=true` behavior still
  substituting inside `phase` or `ket`.
- Watch the stricter `Ket.path_var_order_result` behavior: a ket containing
  path variables while the declared path-variable count is zero now reports
  malformed metadata instead of silently returning empty ordering arrays.
- Watch the recent direct `Poly.t` construction in tests: replacing local
  `to_poly` uses with `+++ Poly.empty` should be equivalent, but it may expose a
  hidden ordering or duplicate-insertion assumption in polynomial tests.
- Watch the new typed `Path_sum_library` gate constructors: the non-typed
  wrappers should preserve the old path sums and failure order, but this broad
  mechanical pass may expose a mismatch in one gate formula or target-index
  validation path.
- The phase 9 inspection prototype is in `scripts/inspect-sqbricks.sh`. Its
  path-sum LaTeX export is intentionally simple and still depends on SQbricks'
  current textual path-sum/debug output.
- Circuit LaTeX export in the inspection prototype uses Quantikz2 and is
  intentionally limited to small OpenQASM 2 circuits.
