# M054: A saved module descriptor that reads back for scoring

**Status:** done (2026-08-24, PR #60 https://github.com/jmgirard/hitop/pull/60)

**Goal:** Let a researcher save a HiTOP-SR module beside the export they field
and read it back at scoring time, instead of retyping every scale name.

**Outcome:** `write_module()` and `read_module()` ship in a new `R/module_file.R`,
exchanging a versioned JSON descriptor — `format`, `package`, `packageVersion`,
`buildDate`, `instrument`, `scales`, `items`, `nItems`, and a reserved `itemOrder`
returned on the `item_order` attribute `generate_docx_hitopsr()` already uses.
`read_module()` rebuilds keying by passing the file's scale names through
`hitop_module()`, never taking items from the file, and raises seven
`hitop_module_file_*` conditions, each naming the file. Items are compared as a
set; a repeat, a mismatch, a version outside `1.0`, a malformed number field, or a
top-level JSON array is refused. {jsonlite} moved to Imports so the browser
builder can write the format. Help pages, a vignette section, NEWS, pkgdown rows.

**Decisions:** D-039, extended here with the format version, the field list, and
the seven condition classes D-034(c) requires it to carry.

**Review:** Three-lens fan-out; the blame-history and prior-review lenses found
nothing. The diff-bug lens raised 11 — F9 refuted against `cli_assert()`, seven
fixed on the branch with a test each (every defect planted and observed red), and
F6, F10, F11 absorbed into the M054 descriptor candidate row. Nothing retired.
