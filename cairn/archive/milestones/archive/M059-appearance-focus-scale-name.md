# M059: The HiTOP-SR's Body Focus scale is named Appearance Focus

**Status:** done (2026-08-28, PR #65 https://github.com/jmgirard/hitop/pull/65)

**Goal:** The HiTOP-SR scale this package calls `Body Focus` carries the name the introduction paper's Table 1 prints, `Appearance Focus`, wherever the package prints or returns it.

**Outcome:** `Appearance Focus` replaces `Body Focus` in the two source CSVs and the four
keying tables built from them, so `score_hitopsr()` returns `hsr_appearanceFocus`/`_se` at
positions 408/484 where `hsr_bodyFocus`/`_se` sat at 412/488; `hitop_module()` and
`read_module()` reject the retired name. The two Word questionnaires, their
`pkgdown/assets/downloads/` copies and two `hitop_artifacts` rows were rebuilt; the
Qualtrics and REDCap exports print no scale name and were not. New
`data-raw/hitopsr_table1.R` is a shared Table 1 extractor that never sees a committed
name — it strips the proof's rotated watermark and handles both extracted row shapes;
`verify_hitopsr_scale_name.R` now covers both renamed scales pinned by item numbers rather
than item text, and new `verify_hitopsr_names.R` reconciles all 93 Table 1 labels, the 13
section headers and the 8-member Superspectra block against both shipped name tables and a
count read from the paper's own prose.

**Decisions:** none milestone-local; the rename runs on D-042, recorded at plan time, which
widens D-041's one-name allowance to this source's scale names.

**Review:** One pass, no returns. Three-lens fan-out: both Sonnet lenses clean, the Opus
diff lens 13 findings (its first two spawns died on environment failures). None showed a
criterion failing; 5 rejected, 10 filed as follow-ups.
