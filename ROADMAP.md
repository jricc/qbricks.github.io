# SQbricks roadmap

## Goal

Make SQbricks robust, measurable, and extensible for hybrid quantum circuit equivalence checking, then explore query-driven symbolic simulation and bounded dynamic circuits.

## Guiding principle

Work in layers:

1. stabilize;
2. measure;
3. improve reduction;
4. improve input support;
5. prototype simulation;
6. explore bounded dynamic circuits.

Avoid large feature additions before the benchmark and regression setup are reliable.

## Current checkpoint

As of 2026-08-08:

- the light non-regression benchmark exists and checks both functional status
  and tracked performance;
- light performance comparison uses three rounds and the best observed timing
  to reduce false positives from local machine load;
- the SQbricks-only long benchmark runner exists;
- the long runner stops source-scoped ordered-size series after timeout or
  memory failure, while continuing other series;
- the large regression path selection exists and has a separate baseline/check
  workflow;
- phases 1, 2, 3, and 4 are done;
- phase 3 covered reduction, equivalence checking, separation, projection,
  benchmark scripts, AST/Program invariants, OpenQASM parser behavior,
  deferred measurement translation, and path-sum generation.
- the 2026-08-05 `main` checkpoint passed all unit-test suites and the light
  non-regression check;
- the large regression workflow is operational, but its latest post-HH run
  remains inconclusive for a global performance comparison because some
  Sequence cases are close to the memory and timeout limits;
- version 0.1.0 is therefore presented as a research prototype without a
  global performance-improvement claim;
- phase 9 has a first prototype script for inspecting two QASM files, collecting
  SQV traces, path-sums, final path-sums, and prototype LaTeX/PDF path-sum
  exports.

## ~~Phase 1 — Minimal regression benchmark~~

Goal: create a short benchmark that detects regressions and measures capability improvements.

Expected properties:

- fast enough to run often;
- deterministic;
- small enough to debug;
- representative of SQbricks core features.

It should cover:

- basic unitary equivalence;
- path-sum reduction;
- hybrid lifting;
- discard and partial equivalence;
- parser behavior;
- omega-specific cases;
- case-rule-specific cases.

Expected output:

- `smoke` benchmark;
- `regression` benchmark;
- clear result format;
- documented expected status for each case.

## ~~Phase 2 — Safe long benchmark runner~~

Goal: make the long benchmark robust and usable.

Required features:

- per-case timeout;
- per-case memory limit when possible;
- one case per process when possible;
- save results after each case;
- resume mode;
- skip rest of a size-ordered series after repeated timeout or OOM.

The long benchmark must not saturate memory or waste time running all larger circuits after the first ones already timeout.

Expected output:

- safe benchmark runner;
- documented configuration;
- result file with explicit statuses.

## ~~Phase 3 — Correctness audit~~

Goal: identify bugs and fragile assumptions before feature work.

Audit targets:

- ~~parser~~;
- ~~AST~~;
- ~~deferred measurement~~;
- ~~path-sum generation~~;
- ~~reduction~~;
- ~~equivalence checking~~;
- ~~separation~~;
- ~~projection~~;
- ~~benchmark scripts~~.

Look especially for:

- unsafe exceptions;
- index mistakes;
- mutation of shared structures;
- unsupported constructs accepted silently;
- incorrect global phase handling;
- edge cases on empty or small circuits;
- discard/projection soundness issues.

Expected output:

- list of issues;
- severity classification;
- proposed minimal fix for each issue.

## ~~Phase 4 — Step-by-step bug fixes~~

Goal: fix correctness and reproducibility issues.

Current correctness queue, in order:

- [x] finish scalar multiplication before modulo-one phase normalization in
  `Poly.Monome.simplify`;
- [x] preserve the exact semantics of OpenQASM whole-register conditions, or
  reject unsupported forms explicitly;
- [x] make `Path_sum_library` honor declared circuit widths and reject
  overlapping gate wires;
- [x] make multi-wire swaps implement overlapping source/destination mappings
  correctly;
- [x] compare measurements through the correspondence between observable
  outputs;
- [x] implement the documented consistent renaming of path variables during
  ket and path-sum equality.

API and robustness bugs to handle after the correctness queue:

- [x] preserve the full circuit in `one_creg` OpenQASM exports and terminate
  cleanly on unsupported controlled gates;
- [x] align the remaining `ListBis` helper implementations with their public
  contracts;
- [x] close parser input channels on success and failure.

The phase 4 queue is complete. Unsupported multi-controlled Hadamard export is
rejected explicitly and remains tracked as later OpenQASM work.

Rules:

- one bug per patch when possible;
- add regression test before or with the fix;
- do not mix bug fixes with refactoring;
- keep changes local.

Expected output:

- passing regression tests;
- documented fixes;
- no hidden behavior changes.

## Phase 5 — Omega reduction rule

Goal: integrate Amy's omega rule into SQbricks.

Process:

1. write targeted tests;
2. add capability benchmark cases;
3. prototype the matcher and transformation;
4. validate on small examples;
5. integrate into the reduction pipeline;
6. compare before/after benchmark results.

Expected output:

- omega rule implementation;
- tests for matches and non-matches;
- benchmark comparison;
- documented limitations.

## Phase 6 — Case reduction rule

Goal: integrate Amy's case rule into SQbricks.

Process:

1. write targeted tests;
2. add capability benchmark cases;
3. prototype independently;
4. validate on small Clifford+T examples;
5. integrate into the reduction pipeline;
6. compare before/after benchmark results.

Expected output:

- case rule implementation;
- tests for matches and non-matches;
- benchmark comparison;
- documented limitations.

## Phase 7 — Benchmark comparison

Goal: measure the effect of new reduction rules.

Record:

- result status;
- runtime;
- memory usage when available;
- path variables before and after reduction;
- phase terms before and after reduction;
- rules applied.

Expected comparison:

- baseline;
- after omega;
- after case.

Do not claim improvement without measured data.

## Phase 8 — Bottleneck audit

Goal: optimize only after the reduction system and benchmarks are stable.

Potential bottlenecks:

- polynomial simplification;
- substitution;
- path-sum equality;
- separation test;
- projection;
- renaming of path variables;
- parser conversion;
- benchmark runner overhead.

Process:

1. profile representative cases;
2. identify dominant costs;
3. prototype one optimization;
4. measure before/after;
5. integrate only if useful.

## Phase 9 — Interactive inspection workflow

Goal: make SQbricks easier to run, inspect, and explain without changing the
core verification algorithms.

User workflow:

- load two QASM files;
- load or enter the metadata needed for partial equivalence;
- provide a graphical interface for this workflow once the CLI prototype has
  stabilized;
- provide a full-auto mode that derives the available metadata and runs the
  standard equivalence workflow;
- provide a manual mode where inputs, outputs, measurements, and equivalence
  options can be edited explicitly;
- show the generated intermediate artifacts that explain what SQbricks is
  actually comparing.

Export workflow:

- export circuits to LaTeX using Quantikz2;
- cite the appropriate Quantikz2 reference or paper in the documentation once
  the exact package/reference is selected;
- export path-sums to LaTeX, both before and after reduction when useful;
- keep exports deterministic so they can be used in papers, reports, and bug
  reports.

Expected output:

- a small inspection interface;
- a graphical interface to load the two QASM files, edit metadata, choose
  auto/manual mode, run SQV, and browse generated artifacts;
- clear auto/manual modes;
- documented metadata format;
- LaTeX circuit export;
- LaTeX path-sum export;
- examples showing the full workflow on small circuits.

Non-goals at this stage:

- replacing the CLI;
- replacing the benchmark runners;
- building a large IDE before the core workflows are stable.

## Phase 10 — Query-driven symbolic simulation

Goal: prototype simulation as a query-driven symbolic process, not as a general statevector simulator.

Intended workflow:

1. parse or build circuit;
2. instantiate symbolic inputs if provided;
3. generate path-sum;
4. reduce aggressively;
5. project to observable outputs when sound;
6. enumerate only remaining useful path variables;
7. answer a specific query.

Target queries:

- amplitude of one output;
- probability of one event;
- distribution on selected outputs;
- counterexample for failed equivalence;
- debugging information for reductions.

Expected output:

- prototype;
- small examples;
- comparison with known results or external simulator on tiny circuits;
- documented limitations.

## Phase 11 — OpenQASM 2 cleanup

Goal: make OpenQASM 2 support clearer and safer.

Tasks:

- distinguish accepted syntax from supported semantics;
- reject unsupported constructs explicitly;
- improve tests;
- handle OpenQASM `include` files instead of only accepting include statements
  as compatibility no-ops;
- consider separating OpenQASM AST from SQbricks core AST.

Possible constructs to clarify:

- custom gates;
- opaque gates;
- barriers;
- resets;
- measured-qubit reuse, first with explicit reset semantics and later with a
  clearer discard/reuse model;
- measurements;
- classical conditionals;
- parameterized gates;
- non-dyadic or unsupported angles.

Expected output:

- parser tests;
- explicit unsupported-feature errors;
- cleaner conversion path.

## Phase 12 — Useful OpenQASM 3 subset

Goal: prototype only the OpenQASM 3 features useful for SQbricks.

Initial target:

- bounded loops;
- classical conditionals;
- measurements;
- reset;
- gate calls.

Non-goals at this stage:

- full OpenQASM 3 support;
- unbounded loops;
- complete classical computation;
- full timing/pulse support.

For loops, start with unrolling when bounds are concrete.

## Phase 13 — Bounded repeat-until-success

Goal: explore bounded RUS examples before unbounded semantics.

Steps:

1. encode bounded RUS by unrolling;
2. simulate symbolically;
3. check partial equivalence when possible;
4. identify missing reductions;
5. prototype new reductions if needed.

Unbounded RUS requires a separate theoretical design.

Before unbounded RUS, define:

- success probability;
- output state conditioned on success;
- failure behavior;
- exact or approximate equivalence;
- termination assumptions.

## Non-goals for now

- Full OpenQASM 3 support.
- General-purpose statevector simulation.
- Unbounded repeat-until-success semantics.
- Large refactoring without tests.
- Performance claims without benchmark data.
