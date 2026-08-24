# M050: Naming and response controls for the builder's online exports

**Status:** done (2026-08-24, PR #56 https://github.com/jmgirard/hitop/pull/56)

**Goal:** A researcher can name what the builder's Qualtrics and REDCap exports create — block, column prefix, REDCap form — and choose whether REDCap marks items required, with the package guarding those arguments instead of writing whatever it is handed into the artifact.

**Outcome:** `build_qualtrics_txt()`/`build_redcap_zip()` guard `block_name` (NULL allowed, matching the no-block-line branch), `id_prefix`, `include_instructions`, `form_name`, `required` and `breaks` with the `validate_*` helpers, `call` defaulting to `rlang::caller_env()` so the abort blames the exported wrapper. `generate_redcap_hitophsum()`, which builds its dictionary without those builders, carries `form_name`/`required` directly — all eleven exported generators swept. `validate_count()` gained `min`/`allow_null` (defaults unchanged) for `breaks`'s documented `0`/`NULL` disable value, and states a floor rather than a range when `max` is `Inf`. `tests/testthat/test-export-arg-guards.R` derives its domain from `formals()`, not a hand-list; artifacts rebuild byte-identical. Builder (jmgirard/hitop-builder#1): a *Qualtrics and REDCap naming* fieldset, each value passed only into its own format's call, free text bound with `globalEnv.bind()`, boxes prefilled from the package's own formals.

**Decisions:** None milestone-local. Three gated amendments: guards in the shared builders; AC3's `Required Field?` corrected from empty to `n`; scope widened to `generate_redcap_hitophsum()`.

**Review:** Three lenses inline (session configured not to spawn agents). One finding, rejected: `validate_count()`'s `max` gained an `Inf` default where M031 made it required — deliberate, every call site checked. M024's `isTRUE()` lesson not reintroduced. CI green on 8 jobs. Nothing retired; the M045/M049 webR lesson extended.
