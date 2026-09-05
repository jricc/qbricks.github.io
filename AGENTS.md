# AGENTS.md

## General principles

- Tell the truth only.
- Never invent facts, sources, files, commands, results, or code behavior.
- If information is missing or uncertain, say so clearly.
- Separate facts, assumptions, and uncertainties.
- Do not perform hidden or autonomous actions.
- Do not make risky, destructive, or irreversible changes without explicit approval.
- Treat all user data, source code, documents, logs, and conversations as confidential.
- Prefer local analysis.
- Do not share data with third parties.
- Do not reuse data outside the current task.
- If external access is needed, explain why first.
- Do not modify files unless I explicitly ask for a modification.
- Do not use apply_patch unless I explicitly approve the exact change.
- Do not run tests or build commands. The user runs tests manually.
- Do not run Git staging or commit commands. The user runs Git commands manually;
  provide only the suggested commit message when useful.
- Read-only inspection commands are allowed when needed.

## Answer style

- Be short, direct, and precise.
- Use simple vocabulary.
- Avoid unnecessary context.
- Prefer clear explanations over jargon.
- When discussing a specific benchmark, benchmark family, mode, or test case,
  name that context explicitly. If it was introduced earlier, briefly remind
  the reader instead of relying on implicit context.
- Act as a senior scientist and senior software engineer.
- Prioritize correctness, rigor, reproducibility, and simplicity.

## Simplicity and anti-overengineering

* Prefer the simplest correct solution.
* Solve the current problem only.
* Do not generalize for hypothetical future needs.
* Do not introduce abstractions unless they clearly simplify the current code.
* Do not create classes, modules, functors, interfaces, helper layers, registries, callbacks, or frameworks unless they are necessary now.
* Avoid future-proof code.
* Avoid clever code.
* Prefer direct, readable code over generic code.
* A small duplicated expression is acceptable if abstraction would make the code harder to understand.
* Add a helper function only when it removes real duplication or clarifies the logic.
* Before adding a helper, look for an existing function that already expresses
  the same idea.
* Put broadly reusable helper functions in `Common` instead of keeping local
  copies in feature modules.
* If a task can be solved with a simple function, use a simple function.
* Do not build a class or framework for a simple operation.
* Do not add extra features.
* Do not refactor unrelated code.

## SQbricks style notes

* Prefer explicit domain logic over generic framework-like code.
* Keep new code close to the existing SQbricks vocabulary: programs, path sums, reductions, equivalence checks, manifests, and CSV results.
* Small, concrete helpers are better than broad abstractions when they make the current behavior easier to read.
* Scripts should stay procedural and readable; avoid making benchmark runners more generic than the current workflow requires.
* When starting a new roadmap step, consider creating or switching to a dedicated branch before changing code.
* For LaTeX quantum-circuit output, use Quantikz2 instead of ad hoc
  TikZ/Qcircuit code. In the Docker image, install the current CTAN Quantikz
  files at the end of the `Dockerfile` so rebuilds keep the heavier layers
  cached.
* In quality mode, keep `doc/SQbricks.md` and `doc/SQbricks.en.md` aligned when
  a stable public behavior, contract, limitation, or workflow changes.
* Keep function-level details in `odoc` comments, public interfaces, and tests.
  Keep unpublished proofs, commit studies, and experimental measurements in a
  private technical report outside this repository.

Before coding, state the minimal solution in 2–5 lines.
If a more generic design is possible, mention it only as an alternative, but do not implement it unless explicitly requested.


## Coding principles

- Write readable code.
- Prefer simple functions with clear responsibilities.
- Prefer robust solutions over clever solutions.
- Follow the existing style and architecture.
- Make minimal, localized changes.
- Do not rewrite unrelated code.
- Do not rename, move, delete, reformat, commit, push, reset, or rebase without explicit approval.
- Do not run `git add`, `git commit`, or equivalent staging/commit commands.
  The user handles Git operations manually.
- Document important choices, limitations, and edge cases.
- Avoid comments that merely repeat the code.
- Prefer one or two short, concrete examples over a long abstract comment when
  examples make the behavior or invariant easier to understand.
- Use descriptive variable and function names. Avoid generic names such as
  `f`, `input`, `output`, or `aux` when a domain-specific name would make the
  code easier to read.
- If a helper is reusable across several modules, put it in `Common` instead of
  keeping a local copy.
- Add minimal comments for non-obvious implementation choices, especially when
  two similar data structures must be handled differently.
- As soon as code or tests are even slightly non-trivial, add a short comment
  explaining the intention, invariant, or reason for the case.
- Before each function, add a concise comment explaining why it exists and
  give a small concrete example. Apply this rule to local and internal helper
  functions as well.
- In branch-heavy domain logic, comment each non-obvious branch with the
  semantic case it handles.
- Keep branch comments short: name the case and, when useful, its result or the
  next case tried. Prefer precise names over long walkthroughs.

## OCaml-specific rules

- Prefer a functional style.
- Prefer pure functions when possible.
- Prefer existing infix notations when they make the code more readable; for
  example, in tests, prefer the local `+++` notation over direct `Poly.insert`
  calls.
- When a `Poly.t` can be built directly and readably, avoid wrapping a single
  monome with `to_poly`; for example, prefer `monome +++ Poly.empty` in tests.
- Use explicit types when they improve readability or documentation.
- Use small functions and clear pattern matching.
- Avoid unnecessary mutation.
- Be very careful with dedicated equality functions such as `Qubit.equal`,
  `Poly.equal`, and `Path_sum.equal`: in SQbricks they usually express a
  semantic/domain equality, not just structural OCaml equality. Do not replace
  them with `=` unless the code is explicitly checking physical/structural
  identity of an implementation value and this is justified in a comment.
- Use `.mli` files for public signatures when appropriate.
- Use odoc-compatible documentation comments.
- Use `(** ... *)` comments for public functions, modules, types, and values.
- Keep public interfaces minimal.
- Keep implementation details out of public interfaces unless necessary.

## Work modes

There are three modes:

1. Simple mode.
2. Prototype mode.
3. Quality mode strict.

If the user does not specify a mode, use simple mode.
Do not ask which mode to use unless the task is ambiguous.

## Mode 1: Simple mode

Default mode.

Use this for small changes, bug fixes, reviews, explanations, and local improvements.

Rules:

* Understand the current code first.
* Propose the minimal useful change.
* Avoid long plans.
* Avoid generalization.
* Avoid new abstractions.
* Avoid unrelated refactoring.
* Keep explanations short.
* Do not add extra features.
* Do not make the code more generic than needed.

Expected output before changes:

* What you intend to change.
* Which file(s) are likely involved.
* Why this is the minimal solution.

Expected output after changes:

* What changed.
* Which files changed.
* What remains to check.


## Mode 2: Prototyping

Goal: test an idea quickly.

Rules:

- Move fast.
- Keep changes small and reversible.
- Prefer the simplest working experiment.
- Clearly mark temporary code.
- Avoid premature abstraction.
- Do not modify stable architecture unless necessary.
- Explain what is experimental.
- Explain what would need to be cleaned before production use.
- Do not run checks unless explicitly requested by the user.

Expected output:

- Idea tested.
- Files changed.
- How to run the prototype.
- Known limitations.

## Mode 3: Quality code

Goal: produce maintainable, tested, documented code.

Use this mode only when the user explicitly asks for quality mode strict, or after proposing it and receiving approval.

### Step 1: Action plan

Before coding, produce a concise action plan.

Output:

- Notes suitable for review.
- No code modification yet.

The plan must include:

- Goal.
- Assumptions.
- Risks.
- Files likely involved.
- Validation strategy.

### Step 2: Task decomposition

Split the work into small tasks.

Output:

- Notes suitable for review.
- No code modification yet.

Each task must be small enough to review independently.

### Step 3: Function-level design

Refine the tasks until the necessary functions are identified.

Output:

- Notes suitable for review.
- Signature/interface proposal.
- Public comments suitable for odoc.
- Explicit types.
- No implementation yet.

For OCaml:

- Prefer `.mli` signatures.
- Use odoc-compatible comments.
- Use `(** ... *)` for public documentation.
- Keep signatures minimal.

### Step 4: Tests first

Once the signatures are accepted, write tests before implementation.

Tests must cover:

- Normal cases.
- Edge cases.
- Error cases when relevant.
- Regression cases if relevant.

Do not implement the function yet.

### Step 5: User test review

Stop after writing the tests.

The user must read the tests and confirm that they express the intended behavior.

Do not continue before validation.

### Step 6: Function implementation

After test validation, implement one function only.

Rules:

- Follow the accepted signature.
- Keep the implementation simple.
- Add odoc comments when public.
- Add internal comments only when they clarify non-obvious logic.
- Do not change tests unless explicitly requested.

Output:

- Files changed.
- Short explanation.
- Known limitations or uncertainty.

### Step 7: User code review

Stop after implementation.

The user must read the code.

Do not continue before validation.

### Step 8: User test execution

The user must run and validate the tests.

Do not claim the tests passed unless the user reports it or you actually ran them locally and show the exact command and result.

### Step 9: Living SQbricks documentation

After the user validates a stable public behavior, contract, limitation, or
workflow, update both `doc/SQbricks.md` (French) and `doc/SQbricks.en.md`
(English) before moving to the next subsystem.

These two versions form the same public technical overview and must remain
functionally aligned in the same change. This documentation is distinct from
the concise `README.md`. It must:

- remain pedagogical and make the purpose and execution flow easy to grasp;
- describe stable architecture, workflows, observable contracts, errors, and
  supported or unsupported behavior;
- avoid exhaustive function-by-function implementation histories;
- leave function-level details to `odoc` comments, `.mli` interfaces, and tests;
- leave unpublished proofs, commit investigations, raw experiments, and
  machine-specific measurements in a private technical report outside this
  repository;
- document only reviewed behavior and clearly mark incomplete public features.

### Step 10: Next function

After validation and its documentation update, move to the next function and
repeat from Step 6.

## Lightweight quality process

For trivial edits, typo fixes, formatting fixes, or very small bug fixes:

1. Explain the intended change.
2. Make the minimal change.
3. Run checks only if explicitly requested by the user.
4. Summarize the result.

Use the full quality process for non-trivial changes.

## Testing rules

- Prefer small, deterministic tests.
- Keep tests minimal: use the smallest circuit and input data that isolate the
  behavior being checked.
- For typed returns such as `result`, `option`, or a custom variant, add minimal
  tests for each observable return possibility.
- Avoid brittle tests.
- Avoid over-mocking.
- Tests should document the intended behavior.
- If a test depends on randomness, fix the seed or justify why not.
- If tests cannot be run, explain exactly why.
- The Docker container does not provide `/usr/bin/time`. Use Bash `SECONDS`
  for simple elapsed-time measurements or the existing benchmark timers.

## Documentation rules

- Public functions, modules, classes, values, and types must have documentation comments.
- Documentation must explain purpose, inputs, outputs, errors, and important invariants.
- Keep documentation concise.
- Documentation must be compatible with odoc for OCaml projects.
- In quality mode, maintain both language versions of the pedagogical project
  documentation, `doc/SQbricks.md` and `doc/SQbricks.en.md`, as described in
  Step 9.

## Review behavior

When reviewing code:

- Identify correctness issues first.
- Then discuss design, readability, tests, and documentation.
- Distinguish blocking issues from suggestions.
- Avoid subjective preferences unless they affect maintainability.
- Propose minimal fixes.

## Final response after code changes

Always summarize:

- What changed.
- Which files changed.
- Which tests were added or modified.
- Which commands were run.
- What remains uncertain.



# SQbricks-specific instructions

Follow the global `AGENTS.md`.

## Project focus

SQbricks is a research prototype for equivalence checking of hybrid quantum circuits.

The current goal is to make SQbricks more robust, measurable, and extensible before adding large new features.

## Current priority order

Unless explicitly told otherwise, work in this order:

1. Minimal regression benchmark.
2. Safe long benchmark runner.
3. Correctness audit.
4. Step-by-step bug fixes.
5. Omega reduction rule.
6. Case reduction rule.
7. Benchmark comparison before and after new rules.
8. Bottleneck audit.
9. Query-driven symbolic simulation prototype.
10. OpenQASM 2 cleanup.
11. Useful OpenQASM 3 subset.
12. Bounded repeat-until-success examples.

Do not skip earlier steps without explicit approval.

## Project rules

- When moving to a new roadmap step, consider switching to a dedicated git branch before starting new work.
- Do not mix unrelated changes in one patch.
- Prefer tests before implementation.
- For non-trivial changes, propose quality mode strict, but use it only after user approval.
- For prototypes, keep changes isolated and clearly marked.
- Do not optimize before benchmark results are reliable.
- Do not add parser support without clear supported/unsupported semantics.
- Do not silently accept unsupported constructs.
- Prefer small, reviewable changes.

## Benchmark rules

Maintain three benchmark levels:

- `smoke`: very small, fast non-regression tests.
- `regression`: meaningful short benchmark.
- `long`: full evaluation benchmark.

The long benchmark must avoid wasting resources:

- use per-case timeout;
- use per-case memory limit when possible;
- save results after each case;
- support resume;
- skip the rest of a size-ordered series after repeated timeout or OOM.

Benchmark statuses must distinguish:

- `EQ`: equivalence proved;
- `NE`: non-equivalence detected;
- `NC`: inconclusive;
- `TIMEOUT`: timeout;
- `OOM`: memory limit exceeded;
- `CRASH`: unexpected crash;
- `PARSE_ERROR`: parsing failed;
- `SKIPPED`: deliberately skipped.

Do not hide failures by deleting cases silently.

## Reduction-rule rules

Reduction rules are correctness-critical.

For omega and case rules:

1. Add targeted tests.
2. Add capability benchmark cases.
3. Prototype in isolation.
4. Validate on small examples.
5. Integrate step by step.
6. Compare before/after results.

Develop a new reduction rule one semantic case at a time: add the smallest
failing test, implement only that case, validate it, and only then add the next
more general test. Do not anticipate later matcher cases in an earlier
implementation.

Each rule must have:

- a clear matching condition;
- a clear transformation;
- tests for positive matches;
- tests for non-matches;
- tests under context when relevant;
- regression tests for known examples.

## Parser rules

Keep a clear distinction between:

- syntax accepted by the parser;
- semantics supported by SQbricks.

Rejected constructs must fail cleanly with explicit errors.

For OpenQASM work, prefer eventually separating:

- OpenQASM AST;
- SQbricks core AST.

Parser changes must include tests for:

- accepted syntax;
- rejected unsupported syntax;
- meaningful error cases;
- conversion to the internal representation.

## Simulation rules

Simulation is not intended to replace general statevector simulators.

The intended direction is query-driven symbolic simulation:

- instantiate symbolic inputs;
- reduce path-sums aggressively;
- project to observable outputs when sound;
- enumerate only remaining useful path variables;
- answer specific queries.

Useful queries include:

- amplitude of one output;
- probability of one observable event;
- distribution on selected outputs;
- counterexample generation for failed equivalence;
- debugging of reductions.

Prototype simulation before integrating it into stable code.

## OpenQASM 3 and loops

OpenQASM 3 support must be incremental.

Start with useful features only:

- bounded loops;
- classical conditionals;
- measurements;
- reset;
- gate calls.

For loops, prefer unrolling first when bounds are concrete.

Do not attempt unbounded repeat-until-success semantics before bounded examples work.

## Repeat-until-success rules

Handle repeat-until-success in stages:

1. bounded RUS by unrolling;
2. symbolic simulation of bounded RUS;
3. partial equivalence on bounded RUS;
4. only later, investigate unbounded RUS.

Before implementing unbounded RUS, define the intended equivalence notion:

- same success probability;
- same output conditioned on success;
- same failure behavior;
- exact or approximate equivalence;
- bounded or unbounded number of repetitions.

## Audit checklist

When auditing the code, look first for:

- unsafe `failwith` or uncaught exceptions;
- incorrect assumptions on indices;
- mutation that can affect shared structures;
- unsupported OpenQASM constructs accepted silently;
- missing edge cases for empty or one-qubit circuits;
- inconsistent treatment of quantum and classical registers;
- global phase handling;
- discard and projection soundness;
- timeout and memory blowups in benchmarks.

Report issues as:

- blocking correctness issue;
- benchmark/reproducibility issue;
- performance issue;
- cleanup suggestion.

Propose minimal fixes first.
