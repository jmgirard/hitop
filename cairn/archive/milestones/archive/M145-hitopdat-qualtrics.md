# M145: HiTOP-DAT Qualtrics import file

**Status:** done (2026-10-17, PR #181 https://github.com/jmgirard/hitop/pull/181)

**Goal:** Researchers can import the HiTOP-DAT into Qualtrics from a file that the package builds from the M143 tables.

**Outcome:** `generate_qualtrics_hitopdat()` writes an Advanced Format `.txt`: questions `hdat_001` to `hdat_382` in battery order, each with its own answer set as `[[Choice:<value>]]` tags from `hitopdat_choices`, and six `<id_prefix>_<measure>_instructions` text questions (none for the CAPE; the DUDIT's written as its lines). Each measure after the first starts a page, and `breaks` counts within a measure. Unlike the REDCap file, an in-measure break does not repeat the instruction (AC1 binds one copy). It checks `file` and `id_prefix` itself. `build_qualtrics_txt()` gained `choices` and `sections` (refused with `page_header`, unsorted rows, or a wrong-length `choices`); the other nine `.txt` files rebuild byte for byte. Manifest row, site copy, download-page card, import article, NEWS, reference index, README row, DESIGN.md. Tests: a `read_qualtrics_questions()` parser, parse-back and refusal tests, and the DAT added to the hand-kept lists (LESSONS M158).

**Decisions:** none promoted. Implement choices (`hdat` prefix, page per measure, no repeated instructions) are in git history.

**Review:** No amendments. Claim audit corrected 6 of 37 claims. Three-lens fan-out found 22 findings: 12 fixed on the branch (sections and choices guards, help wording, import article, cross-links, stale DESIGN.md and test comments, a test title), 5 to candidate row "DAT online file gaps" (no repeated instructions, harm values 0/2/4 and the 10-line DUDIT text unconfirmed by a live import, no source notice), 5 rejected (3 false, 2 planned). Local suite 0 failures, `check()` 0/0/0, CI 8 of 8 green; the first CI wait timed out at the session limit and resumed the next day. No lesson added or retired (LESSONS.md is at its byte budget).
