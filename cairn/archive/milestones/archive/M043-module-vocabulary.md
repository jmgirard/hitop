# M043: Rename the HiTOP-SR subset family to modules

**Status:** done (2026-08-21, PR #48 https://github.com/jmgirard/hitop/pull/48)

**Goal:** Rename the scale-subset descriptor family to "module" across the
exported API, keep the old names as deprecation shims, add a scale browser.

**Outcome:** `R/subset.R` becomes `R/module.R` — `hitop_module()` returns a
`hitop_module`; `apply_module()`/`module_engine_inputs()` replace their `subset`
names; `is_module()` accepts both classes. New `R/deprecated.R` holds
`hitop_subset()` (warns, returning the same object under the legacy class), a
non-warning `print.hitop_subset()`, and `resolve_module_arg()`, giving the five
exported consumers a `module` argument that still takes `subset` and errors on
both. New `available_scales()` lists scales; `module_scale_tables()` supplies the
supported set and every table.

**Decisions:** D-034 — module vocabulary, shims, the four condition classes as a
public contract, hand-rolled deprecation over {lifecycle}.

**Review:** three fresh-context lenses; blame-history and prior-review found
nothing. Diff-bug proved no scored value moves, then returned 12; nine actioned,
two real bugs — `m = ` became ambiguous with `missing`, and the deprecated
constructor blamed `hitop_module()`. AC3's first evidence was invalid. Hygiene
retired M024's truthiness lesson (an `isTRUE()` guard fails
`test-generate_docx.R:173`) and folded M027's cli-plural lesson into M030's.
