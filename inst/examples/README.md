# Example files

The hitop-form web page (<https://github.com/jmgirard/hitop-form>) saved or
read these files. They install with the package, so the vignettes and articles
read them with `system.file("examples", <file>, package = "hitop")`. The
package's tests read them too.

| File | Source and generator |
|---|---|
| `responses-pid5.csv` | Copied from `tests/fixtures/` in the hitop-form repository at commit `bfb1d9c` (2026-09-20). Generated there by `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js`. It is the full PID-5 form the page saved for study `fixture` and participant `p001`. A fixed pattern in that repository's `tests/helpers.mjs` answers every item. On a form whose options are worth 0 to 3, it writes 3, 2, 1, 0 repeating down the form. The page writes CRLF row endings. This repository stores the file as LF under its line-ending policy. |
| `module-shuffled.json` | Copied from `tests/fixtures/module-shuffled.json` in the hitop-form repository at commit `21a1d1c` (2026-09-20). Generated there by `Rscript tests/fixtures/make-descriptors.R`: `hitop_module("hitopsr", scales = c("Distress-Dysphoria", "Agoraphobia"))` written by `write_module()` with an `itemOrder` drawn under `set.seed(95)`. |
| `responses-module-shuffled.csv` | Copied from `tests/fixtures/` in the same hitop-form commit (`21a1d1c`). Generated there by `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js`. It is the form the page saved from `module-shuffled.json` for study `fixture` and participant `p001`, every item answered by the same pattern (4, 3, 2, 1 repeating down the form). Stored as LF, as above. |
