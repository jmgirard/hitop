# M054: A saved module descriptor that reads back for scoring

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP3, GP4
- **Branch/PR:** `m054-module-descriptor-file`

## Goal

Let a researcher save a HiTOP-SR module to a file beside the export they field
and read it back at scoring time, instead of retyping every scale name.

## Scope

Surface tier: **user-facing** — two new exports and a file format researchers
hold, edit, and pass to collaborators.

**In:** a documented JSON descriptor format for a `hitop_module`, carrying a
format version, the package version and build date that wrote it, the
instrument, the scale names, the item numbers, and a reserved `itemOrder`
slot; exported `write_module()` and `read_module()`; keying rebuilt on read
from the package's own tables through `hitop_module()`, with the file's
recorded items cross-checked and any disagreement raised as a classed error
rather than trusted (IP1: a file never injects keying); {jsonlite} moved from
Suggests to Imports so the browser builder, where Suggests are not installed,
can write the format; help pages, a vignette section, NEWS, and the pkgdown
reference index.

**Out:** the generators writing the descriptor as a sidecar, and the builder
page offering it as a fourth download → M055 (planned, depends on this).
Populating `itemOrder` from a shuffled Word form → M055, which is what closes
DESIGN known issue 8; this milestone only reserves and validates the field.
Deriving the `items` argument of `score_hitopsr()` from the descriptor's
recorded column names → a candidate row, since those names exist only once a
generator has run. Working out a module from column names with no descriptor
in hand → the standing M037 candidate row, a different problem. Descriptors
for the HiTOP-BR or PID-5 → the standing modularization-generalization row;
`hitop_module()` supports one instrument today.

## Acceptance criteria

- [ ] AC1 For each module in the set a test enumerates by iterating the rows of
      `available_scales("hitopsr")` — one single-scale module per row, one
      module naming every row, and one four-scale module of non-adjacent rows —
      `write_module()` produces a file whose parsed `instrument`, `scales`, and
      `items` fields equal values derived directly from `hitopsr_scales` and
      `hitopsr_items`, never read off the module object under test, and
      `read_module()` on that file returns an object `identical()` to it.
- [ ] AC2 A hand-written descriptor committed as a test fixture, produced by no
      package function and carrying its provenance per the PROFILE fixture
      rule, reads back into a `hitop_module` whose `instrument`, `scales`,
      `camelCase`, `items`, `reverse`, and `nItems` each equal values derived
      directly from `hitopsr_scales` and `hitopsr_items`.
- [ ] AC3 `read_module()` aborts with a classed `cli` error naming the file and
      the disagreeing field for a descriptor whose recorded `items` disagrees
      with what `hitop_module()` derives from that file's own `scales` — tested
      in a wrong-value form and a wrong-length form — and for one whose
      `nItems` disagrees with its `items`. Where the fields agree, the returned
      module's `items` and `reverse` equal those of
      `hitop_module(instrument, scales)`.
- [ ] AC4 `read_module()` aborts with a classed `cli` error naming the file for
      each of: a file that is not valid JSON; each field the documented format
      requires (`format`, `instrument`, `scales`) omitted in turn, one test per
      field; an `instrument` the package does not support; a scale name
      `hitop_module()` does not recognize. Every class introduced is named in
      D-039, per D-034(c).
- [ ] AC5 `write_module()` writes a `format` field whose value equals the
      version string this release writes, asserted in the test against that
      literal and not against `write_module()`'s own output; `read_module()`
      aborts on a `format` carrying a higher version than this release writes,
      and on a non-version value such as `42`.
- [ ] AC6 A descriptor whose `itemOrder` is a permutation of its `items` reads
      back with that order preserved on the returned object's `item_order`
      attribute, matching the attribute name `generate_docx_hitopsr()` already
      returns; one whose `itemOrder` is present but is not a permutation of
      `items` is rejected with a classed error; and a descriptor
      `write_module()` wrote carries no `itemOrder`, because a module object
      records no printed order.
- [ ] AC7 `score_hitopsr(collected, items = names(collected), module = read_module(f))`
      returns a tibble `identical()` to the same call passing the in-memory
      module, for the four-scale module scored on the matching `sim_hitopsr`
      columns; `reliability_hitopsr()` likewise.
- [ ] AC8 `?write_module` and `?read_module` each carry an example that runs
      under `R CMD check` and writes only to `tempfile()`;
      `vignettes/articles/modules-hitopsr.Rmd` gains a section running
      generate → write → read → score; `NEWS.md` records both exports and the
      {jsonlite} Imports move; both functions appear in `_pkgdown.yml` and
      `pkgdown::check_pkgdown()` passes.
- [ ] AC9 `devtools::document()` produces no diff, and `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T2, T3, T5
- AC2 → T4, T5
- AC3 → T3, T5
- AC4 → T3, T5
- AC5 → T1, T2, T3, T5
- AC6 → T2, T3, T5
- AC7 → T5
- AC8 → T6
- AC9 → T6, T7

## Tasks

- [x] T1 Settle and document the format: field list, types, the version string,
      and what is authoritative versus advisory on read. Move {jsonlite} from
      Suggests to Imports in `DESCRIPTION`, and extend D-039 with the names of
      the condition classes this milestone introduces, which D-034(c) requires
      a D-entry to carry.
- [ ] T2 Implement `write_module()` in a new `R/module_file.R`, using the
      `validate_*()` helpers in `R/util.R` and the `call = rlang::current_env()`
      convention `hitop_module()` follows (`R/module.R:63`).
- [ ] T3 Implement `read_module()`: parse, check `format`, check the required
      fields, rebuild through `hitop_module()`, cross-check the recorded
      `items`/`nItems`, validate and attach `item_order`, and raise each
      failure as its own classed condition.
- [ ] T4 Commit the hand-written descriptor fixture under
      `tests/testthat/fixtures/` with its provenance comment.
- [ ] T5 Tests in `tests/testthat/test-module_file.R` covering AC1–AC7, with
      the enumerated module set built from `available_scales("hitopsr")` rather
      than any hand-written list.
- [ ] T6 Docs: roxygen for both functions, the vignette section, the NEWS
      entries, and the `_pkgdown.yml` reference lines.
- [ ] T7 Run the PROFILE verify slot: `devtools::document()`,
      `devtools::test()`, `devtools::check()`, `pkgdown::check_pkgdown()`.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate chose JSON with {jsonlite} in Imports over base-R DCF, RDS, and a bare CSV of scale names because the format must be human-readable and writable from the browser builder, where Suggests are not installed — the D-035 {zip} precedent; falsified by {jsonlite} failing to build under emscripten, which would return the format to the base-R DCF option.
- 2026-08-24: plan gate chose shipping only `write_module()`/`read_module()` here, with generator sidecars and the builder download in M055, over doing all of it at once, because three call sites should not be written against a format that has never round-tripped; falsified by the sidecar work finding the format inadequate, which would make the split two edits to one design instead of one.
- 2026-08-24: plan gate chose reserving `itemOrder` in the format now and populating it in M055 over omitting it and over giving `write_module()` an `item_order` argument, because a module object carries no printed order and a caller-supplied one has nothing to check it against; falsified by M055 finding the reserved field's shape wrong for what `generate_docx_hitopsr()` actually returns.
- 2026-08-24: plan gate chose leaving `items` required on `score_hitopsr()` over deriving it from the descriptor, because the column names only exist once a generator has run and matching them silently is the failure GP2's warnings exist to prevent; falsified by a researcher reporting the descriptor still leaves the tedious half of the typing.
- 2026-08-24: plan chose rebuilding keying from the package's tables on read, with the file's recorded items cross-checked and a disagreement raised, over trusting the file (IP1: a descriptor could otherwise inject keying) and over recording no items at all (a descriptor a researcher cannot read is worth less); falsified by a legitimate workflow where the package's tables move and an old descriptor must still score.
- 2026-08-24: fresh-context criteria audit ran in FULL mode (user-facing tier) over the pre-gate AC draft and returned findings on four of six criteria; all four were repaired before this file was written. The round-trip criterion asserted `write_module()`'s output through `read_module()` and nothing else, which is code-as-truth under IP2 and is satisfied by a format recording only scale names — now the written file's fields are asserted against `hitopsr_scales`/`hitopsr_items` and a hand-written fixture is read independently (AC1, AC2). The keying criterion stated an implementation path rather than an observable property and probed one field in one form — now behavioral, per corrupted field, in two forms (AC3). "A JSON file missing a required field" was one exemplar for a family — now one test per required field, omitted in turn (AC4). "Aborts on a `format` value it does not support" was unbounded on a single probe — now a stated pair, with the written value asserted against a literal (AC5).
- 2026-08-24: /milestone-implement started; branch `m054-module-descriptor-file` cut from main.
- 2026-08-24: implementation gate settled four open format choices, all as recommended: camelCase field names in the file, a read that refuses a version stamp newer than this release writes, unknown scale/instrument names surfaced by wrapping `hitop_module()`'s own refusal with the file name, and `file` as the path argument on both functions.
- 2026-08-24: T1 done — {jsonlite} moved from Suggests to Imports in DESCRIPTION, and D-039 extended with the format version string, the field list, and the seven condition classes `read_module()` introduces.

## Decisions

## Review
