# Test fixtures

Provenance for the files here, per the package profile's fixture rule. A file
with a `_provenance` field carries its own note.

| File | Source and generator |
|---|---|
| `module-handwritten.json` | See its `_provenance` field. |
| `module-shuffled.json` | Copied from `tests/fixtures/module-shuffled.json` in the hitop-form repository (<https://github.com/jmgirard/hitop-form>, commit `21a1d1c`, 2026-09-20). Generated there by `Rscript tests/fixtures/make-descriptors.R`: `hitop_module("hitopsr", scales = c("Distress-Dysphoria", "Agoraphobia"))` written by `write_module()` with an `itemOrder` drawn under `set.seed(95)`. |
| `responses-hitopbr.csv`, `responses-hitopsr.csv`, `responses-module-shuffled.csv` | Copied from `tests/fixtures/` in the same hitop-form commit. Generated there by `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js`. Each is one form the page saved for study `fixture` and participant `p001`. Every item is answered by the fixed pattern in that repository's `tests/helpers.mjs` (4, 3, 2, 1 repeating down the form). The module file was saved from `module-shuffled.json`. The page writes CRLF row endings. This repository stores them as LF under its line-ending policy, and `test-read_form_responses.R` covers CRLF with a file it writes itself. |
