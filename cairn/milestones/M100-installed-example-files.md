# M100: The shipped vignette and article read their example files from the installed package

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — a shipped vignette, a site article and a new installed folder
- **Branch/PR:** m100-installed-example-files

## Goal

`vignette("pid5_scoring")` and the HiTOP-SR modules article read their example response files from `inst/examples/`, so their code runs on a user's machine.

## Scope

**In:**

- Move `responses-pid5.csv`, `responses-module-shuffled.csv` and `module-shuffled.json` from `tests/testthat/fixtures/` to a new `inst/examples/` with `git mv`.
- Read them with `system.file("examples", ..., package = "hitop")` in `vignettes/pid5_scoring.Rmd:207-208` and `vignettes/articles/modules-hitopsr.Rmd:390-391,410`.
- Remove the "ships with the package's tests" sentences (`pid5_scoring.Rmd:204`, `modules-hitopsr.Rmd:386-387`).
- Point the tests that read the three files at the installed path, and move the provenance of the three files to `inst/examples/`.
- Add one NEWS entry.

**Out:** The other response fixtures stay in `tests/testthat/fixtures/`, because no shipped page reads them. These are `responses-pid5sf.csv`, `responses-pid5bf.csv`, the HiTOP-SR and HiTOP-BR files, and `module-handwritten.json`. Roxygen `@examples` do not use the example files. That becomes a candidate row if a user asks. `inst/extdata/` keeps only the checksum-locked downloads (D-016), so no manifest row is added.

## Acceptance criteria

- [ ] AC1: `vignettes/pid5_scoring.Rmd` and `vignettes/articles/modules-hitopsr.Rmd` read their example files with `system.file("examples", <file>, package = "hitop")`, and `grep -rnE '"tests"|tests/testthat|package.s tests' vignettes/` returns no hit.
- [ ] AC2: With the package reinstalled from the branch (`devtools::install()`) and its Suggests present, the R code that `knitr::purl()` extracts from each of the two files runs to completion under `Rscript` from a temporary working directory outside the repo.
- [ ] AC3: Each of the three files in `inst/examples/` is byte-identical to its `e5c72925:tests/testthat/fixtures/` original (`git diff --name-status -M100% e5c72925 HEAD` lists each as `R100`), and a provenance note in `inst/examples/` names each file's hitop-form source and generator.
- [ ] AC4: NEWS.md under the development version says that the PID-5 scoring vignette and the modules article now read their example files from the installed package.
- [ ] AC5: `devtools::test()` and `devtools::check()` are clean (0 errors and 0 warnings, with each NOTE justified).

## Coverage

- AC1 → T2
- AC2 → T4
- AC3 → T1
- AC4 → T3
- AC5 → T1, T4

## Tasks

- [x] T1: `git mv` the three files to `inst/examples/`. Move their provenance rows from `tests/testthat/fixtures/README.md` into `inst/examples/README.md`. Split the two shared rows, so the PID-5-SF, PID-5-BF, HiTOP-SR and HiTOP-BR files keep theirs. Point these sites at `system.file("examples", ...)`: in `tests/testthat/test-read_form_responses.R`, the calls at :441, :446, :467 and :469 (through the `fixture()` helper at :358), the `pid5_cases` loop at :512-522 (the FULL case alone) and the header comment at :3-4, and `tests/testthat/test-layout.R:424-425`. Add one test that reads `system.file("examples", "responses-pid5.csv", package = "hitop")` to one row with 220 item columns. Run `devtools::test()`.
- [x] T2: Rewrite `vignettes/pid5_scoring.Rmd:204-208` and `vignettes/articles/modules-hitopsr.Rmd:385-391,410` to read through `system.file()`, dropping the tests sentences. Run the AC1 grep.
- [x] T3: Add the NEWS.md entry (no milestone numbers).
- [ ] T4: `devtools::install()`, then purl each file and run it with `Rscript` from `tempdir()`. Make sure that the ggplot2-gated chunks at `pid5_scoring.Rmd:166,179,190` run with ggplot2 present. Run `devtools::check()`.

## Work log

- 2026-09-21: created by /milestone-plan.
- 2026-09-21: criteria audit (full mode, [O] fresh reader) returned five findings. AC1 was unreachable as scoped because `modules-hitopsr.Rmd:390` has the same bug, and the gate widened the scope. `--stat` prints no rename percentage, so AC3 uses `--name-status`. The testthat half of AC2 bound a harness property, because `system.file()` reads the source `inst/` under `devtools::test()`, so it moved to T1. AC2 now states the reinstall and the Suggests. AC1 now covers the stale "ships with the package's tests" prose.
- 2026-09-21: second criteria audit (full mode, [O] fresh reader) of the widened AC1-AC3 found no criterion finding. It showed that `-M100%` prints `R100` in a scratch repo and that both files purl and run outside the repo. It returned two task gaps, both fixed in T1: the module fixture shares a provenance row with the HiTOP-SR/BR files, and the `pid5_cases` loop reads at :522, not :512.
- 2026-09-21: plan gate chose `inst/examples/` over `inst/extdata/`, because extdata holds only the D-016 checksum-locked downloads. Falsified by a need to distribute the examples through the download pages.
- 2026-09-21: plan gate chose to move the files over keeping a second copy in `tests/`, because two copies can drift. Falsified by a test that needs a fixture state the shipped example must not have.
- 2026-09-21: plan gate kept this a milestone over `/hotfix`, because it adds an installed folder. Falsified by nothing beyond cost.
- 2026-09-21: T1 done. The three files moved to `inst/examples/` with provenance in `inst/examples/README.md`. Tests read them through a new `example_file()` helper (`tests/testthat/helper-examples.R`). One new test reads each example through `system.file()`. `read_form_responses` and `layout` tests pass.
- 2026-09-21: T2 done. Both files call `system.file("examples", <file>, package = "hitop")` once per file, and the tests sentences are gone. The AC1 grep returns no hit (exit 1).
- 2026-09-21: T3 done. NEWS.md entry added under "Documentation and website".
