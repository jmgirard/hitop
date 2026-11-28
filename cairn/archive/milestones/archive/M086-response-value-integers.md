# M086: Every response value the package ships is an integer

**Status:** done (2026-09-04, PR #94 https://github.com/jmgirard/hitop/pull/94)

**Goal:** Every response value the package ships — the `ku_*` datasets' 550 answer columns,
`hitophsum_choices$Value`, the instruction objects' options — is an integer, with nothing else moved.

**Outcome:** `ku_hitopsr` (405), `ku_hitopbr` (45), `ku_pid5sf` (100), `hitophsum_choices$Value` and
`options$value` in `hitopsr_instructions`/`hitopbr_instructions` are integer, each rebuilt from its
real source with `readr` collectors in `data-raw/{ku_data,hitophsum_info,sysdata}.R`. Three tests hold
it: `test-item-number-type.R`'s sweep reports an empty double-path set over every shipped dataset;
`test-item-number-merge-base.R` proves retype-only reproduces each committed object, through a new
`merge_base_sysdata()` helper; `test-response-value-no-move.R` rebuilds all eleven built artifacts
byte for byte and re-runs every scoring entry point. Also fixed: `hitophsum_choices`'s `@format` row
count (42 stated, 185 actual), and the generators, which now emit LF on every platform.

**Decisions:** D-060 (the retype and its pre-1.0 waiver of a deprecation cycle, on D-056). The
`ku_hitopsr` item mismapping this milestone uncovered shipped separately as the PR #93 hotfix.

**Review:** Three fresh-context lenses (user-facing tier); the Sonnet lenses returned none, [O]
diff-bug eleven. Five fixed at triage (loop-abandoning `skip_if()`, a vacuity guard that could never
fire on `ku_pid5sf`, a global flag masking a per-object failure, `$` partial matching, a redundant
assertion), one rejected as the accepted AC5 amendment, three to a candidate row, one settled on
D-028; two more at Jeff's direction. Windows CI then failed 11 times on CRLF-vs-LF; the gate fixed the
generators rather than relax AC5.
