# Lessons

<!-- Append-only, capped at 50 lines (D-015). Durable repo lessons — build
     quirks, testing tricks — captured at milestone end, surfaced at plan time.
     A *choice* is a D-entry, not a lesson. -->

- 2026-07-16 (M18): DESIGN.md principle bullets must use the validator-canonical `- IPn: …` / `- GPn: …` form — `cairn_validate`'s "principles slot valid" check parses exactly that pattern and first fires when a milestone cites a principle.
- 2026-07-16 (M18): data-raw scripts need readr (+ usethis) installed locally; they are not package dependencies, so a fresh machine must `install.packages("readr")` before regenerating `data/*.rda`.
- 2026-07-16 (M19): the undocumented Qualtrics `GET /survey-definitions/{id}?format=qsf` endpoint returns a genuine importable QSF — validate by shape (SurveyEntry + SurveyElements) and never re-serialize it through jsonlite (round-trips aren't byte-faithful).
- 2026-07-16 (M19): Qualtrics QSF exports serialize a dense zero-based choice map (values 0..n) as a JSON *array*, dropping the keys — recover them positionally from ChoiceOrder when parsing.
- 2026-07-16 (M20): byte-locked repo artifacts need `.gitattributes` `-text` — Windows `actions/checkout` CRLF-converts anything git sniffs as text (.txt/.qsf), silently mutating committed bytes before tests see them.
- 2026-07-16 (M20): DOCX and REDCap zip builds are not byte-deterministic (zip member mtimes, date stamps) — only flat text outputs can be md5-compared across rebuilds; content comparisons must parse inside the container.
- 2026-07-17 (M21): HTML emitted from a knitr chunk via `cat()`/`results='asis'` is still parsed as markdown by pandoc — `_1.0_` became emphasis. Wrap generated HTML in a ```` ```{=html} ```` raw fence so it passes through verbatim.
- 2026-07-17 (M21): pkgdown download-page vignettes render `hitop::hitop_artifacts` from the *installed* package, and `pkgdown/extra.css` copies to `docs/` only on `init_site()`/`build_site()` — after editing data or CSS, `devtools::install()` + a full/`init_site()` build is needed or `build_article()` shows stale content.
- 2026-07-17 (M22): `pkgdown::build_article("name")` (singular) errors "Can't find article" for a newly added `.Rmd` until the article index is rebuilt — use `build_articles()` (plural), or `init_site()`/`build_site()`, after adding a new article.
