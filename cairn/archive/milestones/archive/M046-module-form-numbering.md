# M046: Renumbered and optionally shuffled HiTOP-SR module Word forms

**Status:** done (2026-08-22, PR #52 https://github.com/jmgirard/hitop/pull/52)

**Goal:** A HiTOP-SR module Word form numbers its items 1..n rather than the full
instrument's gapped numbers, and can optionally shuffle them, keeping the map back.

**Outcome:** `generate_docx_hitopsr()` gained `renumber` (default `TRUE`) and
`randomize` (default `FALSE`). One printed-order map built after `apply_module()`
drives the items table, the scoring table, the subscale rows, the crosswalk, and
the return value; `remap_itemdata()` re-sorts each scoring row by printed number.
A shuffled *module* form prints an arrow-joined crosswalk to the original HSR
numbers, independent of `include_scoring`; a shuffled full instrument prints none
(405 pairs, outside D-036). Every call returns `item_order` — originals in printed
order, integer. Qualtrics/REDCap unchanged by design, divergence on both help
pages. New test helpers parse the DOCX back.

**Decisions:** none milestone-local; the IP1 sign-off is D-036, from plan time.

**Review:** three-lens fan-out; blame-history and prior-review found nothing
actionable. Diff-bug returned ten, all reproduced and fixed; three returned the
milestone — shuffled-form data mis-scoring through `score_hitopsr()` undocumented,
the crosswalk suppressed under `include_scoring = FALSE`, AC5 asserted by an
identity. Gate: crosswalk module-only, hazard documented. Hygiene corrected the
M024 CRLF lesson's stale file count.
