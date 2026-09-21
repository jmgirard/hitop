# Test fixtures

Provenance for the files here, per the package profile's fixture rule. A file
with a `_provenance` field carries its own note. The tests also read three
files the package installs under `inst/examples/`, whose provenance is in
`inst/examples/README.md`.

| File | Source and generator |
|---|---|
| `module-handwritten.json` | See its `_provenance` field. |
| `responses-hitopbr.csv`, `responses-hitopsr.csv` | Copied from `tests/fixtures/` in the hitop-form repository (<https://github.com/jmgirard/hitop-form>, commit `21a1d1c`, 2026-09-20). Generated there by `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js`. Each is one form the page saved for study `fixture` and participant `p001`. Every item is answered by the fixed pattern in that repository's `tests/helpers.mjs` (4, 3, 2, 1 repeating down the form). The page writes CRLF row endings. This repository stores them as LF under its line-ending policy, and `test-read_form_responses.R` covers CRLF with a file it writes itself. |
| `responses-pid5sf.csv`, `responses-pid5bf.csv` | Copied from `tests/fixtures/` in the hitop-form repository at commit `bfb1d9c` (2026-09-20). Generated there by the same command, `WRITE_FIXTURES=1 npx playwright test tests/save.spec.js`. Each is one PID-5 form (short, brief) the page saved for study `fixture` and participant `p001`. The same pattern answers every item, and on these forms, whose options are worth 0 to 3, it writes 3, 2, 1, 0 repeating down the form. Stored as LF, as above. |
