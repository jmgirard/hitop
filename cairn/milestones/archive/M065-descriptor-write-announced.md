# M065: The generators announce the descriptor they write

**Status:** done (2026-08-29, PR #72 https://github.com/jmgirard/hitop/pull/72)

**Goal:** say on the console that a descriptor sidecar was written, so a caller
reading the generator's output can answer whether it was saved and where.

**Outcome:** the three `generate_*_hitopsr()` functions each emit a
`cli::cli_alert_success()` naming the descriptor with `{.file }`, after
`built <- TRUE` — past the `on.exit` rollback window M055 built and after the
builder's instrument alert — so it follows the form's message and never names a
descriptor the call removes again; `if (!is.null(descriptor))` keeps a call passing
none silent. `write_descriptor_sidecar()` (`R/module_file.R`) is untouched: it runs
before the instrument exists, which is why the alert lives at the call sites. No
scored value, descriptor field or write order changed. Tests add the announcement, a
silent no-`descriptor` control, and three failure forms each naming its failure.

**Decisions:** none.

**Review:** three fresh-context lenses; blame-history and prior-review found no
conflict, the diff-bug lens no correctness bug in `R/` and twelve findings, all in
tests, docs or tracking. 1, 2, 3, 4 and 6 fixed at the gate (loop guards so a red
run reports three generators not one; severity and `{.file }` assertions; cli widths
pinned; muffled warnings made load-bearing), each shown able to fail; 5 rejected as
house style; 8, 10, 11 and 12 logged; 7 deferred to the docs candidate row.
