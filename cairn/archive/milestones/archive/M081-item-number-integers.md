# M081: Every item number the package ships is an integer

**Status:** done (2026-09-02, PR #88 https://github.com/jmgirard/hitop/pull/88)

**Goal:** Every item-number column the package ships stores whole numbers as integers, so a
caller comparing item numbers across the package's own boundaries meets one type and the
compensating coercions are gone, with no shipped value or exported artifact moved.

**Outcome:** The four keying-table readers under `data-raw/` name
`readr::col_integer()` on their item-number columns, so all 337 item-number paths
across the four `*_items` and four `*_scales`/`*_subscales` tables are integer at
every nesting depth, readr `spec` collectors included, with every other value, row
and attribute identical to `8dd3942`; `hitop_module()$items` is integer too, and
seven coercions in `generate_docx.R` and `module_file.R` are gone. New
`test-item-number-type.R` sweeps every dataset `data(package = "hitop")` lists,
recursing into list-columns and nested frames, and `test-item-number-merge-base.R`
pins that nothing but the type moved; `inst/extdata/` and the pkgdown downloads are
byte-unchanged.

**Decisions:** D-056 records the retype and the pre-1.0 waived deprecation cycle.

**Review:** three-lens fan-out; blame-history and prior-review returned nothing,
the diff-bug lens ten ranked findings. All six criteria passed on fresh evidence;
five findings were fixed on the branch — `write_module()` newly aborting on a
module with double `items` chief among them — one filed, four rejected.
