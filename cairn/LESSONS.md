# Lessons

<!-- Append-only, capped at 50 lines (D-015). Durable repo lessons — build
     quirks, testing tricks — captured at milestone end, surfaced at plan time.
     A *choice* is a D-entry, not a lesson. -->

- 2026-07-16 (M18): DESIGN.md principle bullets must use the validator-canonical `- IPn: …` / `- GPn: …` form — `cairn_validate`'s "principles slot valid" check parses exactly that pattern and first fires when a milestone cites a principle.
- 2026-07-16 (M18): data-raw scripts need readr (+ usethis) installed locally; they are not package dependencies, so a fresh machine must `install.packages("readr")` before regenerating `data/*.rda`.
