# M074: The builder page's naming prose states the conditions it actually depends on

**Status:** done (2026-08-31, PR #80 https://github.com/jmgirard/hitop/pull/80)

**Goal:** The browser module builder's copy states the condition each of its download-naming claims actually rests
on, and tells a visitor holding pre-rename downloads which names replaced theirs.

**Outcome:** Shipped in jmgirard/hitop-builder#11 (`903dee1`), deployed byte-identical (`b4c8018b…`); the `hitop`
package is untouched. The page hint and its README twin condition the scoring-file name collision on same format,
same scope, same shuffle — a predicate agreeing with `downloadStem` on all 64 ordered pairs of its eight builds —
instead of asserting it for every rebuild. The README states that dropping `-module` also needs the start-up
`tilesExactly` probe `wholeInstrument()` gates on, verified by four builds with the gate true and forced false. A
dated `<p id="renameNote">`, referenced nowhere else and deletable as one block, names the two pre-2026-08-29
stems and the eight replacing them. The naming-table paragraph and the `downloadStem` comment no longer claim the
scoring file records the paper size, item numbering, online naming values or REDCap's required flag — it records
none of them. A 16-cell build matrix run on the branch and deployed pages in one session matched in every cell.

**Decisions:** none.

**Review:** three-lens fan-out (user-facing tier). Blame-history and prior-PR-comments found no regression and no
prior-review contradiction; the GitHub inline-comment probe was empty on both repos. The diff-bug lens returned
six findings: four fixed before merge at Jeff's disposition (`9dcef4b`) — a false "one name per build" claim
contradicting the same notice, the ticked-every-scale vs whole-instrument conflation, an over-general "only
record" claim, and REDCap's required flag missing from two enumerations; one deferred to a candidate row (two
comments claiming the probe detects scale overlap, which it cannot); one rejected as plan-owned text. No criterion failed; the return floor did not fire.
