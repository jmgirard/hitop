# M055: The HiTOP-SR generators write a descriptor beside the file they build

- **Status:** planned
- **Priority:** normal
- **Depends on:** M054
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP3
- **Branch/PR:** —

## Goal

Let one generator call produce both the file a researcher fields and the
descriptor that scores the data it comes back as, including the printed order
of a shuffled Word form.

## Scope

Surface tier: **user-facing** — a new argument on three exported generators and
a file researchers keep.

**In:** a `descriptor` argument on `generate_docx_hitopsr()`,
`generate_qualtrics_hitopsr()`, and `generate_redcap_hitopsr()` writing M054's
format to the given path; the descriptor of a `module = NULL` call naming every
scale, so a full administration is described too; `itemOrder` populated from
the `item_order` the Word generator already returns
(`R/generate_docx.R:311`), which gives a shuffled form the record DESIGN known
issue 8 says the researcher is currently told to keep and not given; help
pages, vignette, NEWS.

**Out:** the browser builder offering the descriptor as a download → M056
(planned, depends on this), which closes the app half of known issue 8.
Recording the export's own column names (`HSR_007`, `hsr_007`) so scoring can
resolve `items` → the candidate row M054 opened. HiTOP-BR and PID-5 generators
→ the standing modularization-generalization row; they take no `module`.

## Acceptance criteria

- [ ] AC1 For each of the three named HiTOP-SR generators, a call passing a
      four-scale `module` and a `descriptor` path writes a file whose
      `format`, `instrument`, `scales`, and `items` fields, parsed with
      `jsonlite::fromJSON()` rather than through `read_module()`, equal values
      derived directly from `hitopsr_scales` and `hitopsr_items`, and which
      `read_module()` returns a module `identical()` to that `module` from —
      one test per generator, all three named in the test.
- [ ] AC2 For each of the same three generators, a call passing `descriptor`
      and no `module` writes a descriptor naming exactly the `Scale` column of
      `available_scales("hitopsr")`, and `score_hitopsr()` given that
      descriptor's module over all 405 `sim_hitopsr` item columns returns a
      tibble `identical()` to the same call passing no `module`.
- [ ] AC3 The descriptor's `itemOrder` records original HSR item numbers. For
      `generate_docx_hitopsr(randomize = TRUE, descriptor = )` under a seed
      fixed in the test, run once with a module and once without and at each of
      `renumber = TRUE` and `renumber = FALSE`, scoring responses laid out in
      printed order after reordering by that `itemOrder` returns a tibble
      `identical()` to scoring the same responses laid out in instrument order
      — the expected side read off the responses, never back through the
      mapping under test (the M046 lesson).
- [ ] AC4 With `randomize = FALSE`, and for the two online generators, which
      never shuffle, the written descriptor carries no `itemOrder`.
- [ ] AC5 For each of the three generators, a `descriptor` that is not a
      length-one character string aborts with a classed `cli` error naming the
      argument, and an unwritable `descriptor` path aborts naming the path
      before any instrument file is written, asserted by checking that the
      `file` path does not exist after the abort.
- [ ] AC6 The three help pages document `descriptor`,
      `vignettes/articles/modules-hitopsr.Rmd` shows the one-call form,
      `NEWS.md` records the argument, and DESIGN known issue 8 is rewritten to
      the app-only remainder M056 carries.
- [ ] AC7 `devtools::document()` produces no diff, and `devtools::test()` and
      `devtools::check()` are clean (0 errors, 0 warnings; NOTEs justified).

## Coverage

- AC1 → T1, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T1, T3
- AC6 → T4
- AC7 → T4, T5

## Tasks

- [ ] T1 Add `descriptor` to the three generators, resolving the full-instrument
      case to a module over every scale, and guard it beside the existing
      `file` checks.
- [ ] T2 Thread `item_order` from `generate_docx_hitopsr()`'s existing
      computation (`R/generate_docx.R:217`, returned at `:311`) into the
      descriptor writer.
- [ ] T3 Tests in `tests/testthat/test-module_file.R` or a sibling, covering
      AC1–AC5.
- [ ] T4 Docs: the three help pages, the vignette, NEWS, and the DESIGN known
      issue 8 rewrite.
- [ ] T5 Run the PROFILE verify slot.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan gate split the sidecar work from M054's format work so the format round-trips before three call sites depend on it; the alternative, one milestone doing both, was rejected at the gate and is recorded on M054.
- 2026-08-24: plan chose splitting the builder-page download into M056 rather than carrying it here, because the two live in different repos and the combined scope trips the sizing tripwires; falsified by the app change proving to be a few lines that would have been cheaper alongside these.
- 2026-08-24: fresh-context criteria audit ran in FULL mode (user-facing tier) and returned findings on AC1, AC3, and AC5; all three were repaired before this file was committed. AC1 checked the writer only through the package's own reader (IP2) — now the written fields are parsed independently and asserted against `hitopsr_scales`/`hitopsr_items`. AC3 left the shuffle seed and `renumber` unvaried, the axis where an `itemOrder` recorded in printed numbers rather than original HSR numbers would pass — now seeded and run at both `renumber` values, with the numbering stated. AC5 named an internal call site and left the abort ordering to the implementation, which constrains nothing — now behavioral, with the ordering fixed. AC2, AC4, AC6, and AC7 returned nothing.

## Decisions

## Review
