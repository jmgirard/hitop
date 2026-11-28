# M168: Study link length-check gaps

**Status:** done (2026-10-06, PR #176 https://github.com/jmgirard/hitop/pull/176; companion hitop-form PR #31 https://github.com/jmgirard/hitop-form/pull/31)

**Goal:** The Study Link Builder tells each researcher the true length of a study link, from sourced counts, in a line a screen reader announces.

**Outcome:** The work is in hitop-form `link.html`.
- `HOST_PATH_MAX` is 8,177. A cached page takes 8,192, but on a cache miss GitHub's server refuses 8,178 with 400, as `fastly2026limits.md` records in 47 logged requests.
- The count adds `PROLIFIC_APPENDED_LENGTH` (the three IDs again, 108 characters) and counts SONA's `%SURVEY_CODE%` as `SONA_CODE_LENGTH` 7. Each Prolific ID stays 24 characters, now sourced to the API reference page "Get submission".
- The refusal reads "This link counts N characters after the host name…".
- `#long` names the file's address for a setup-file link. It opens empty with `#result`, and its text is written two `requestAnimationFrame` calls later. A `longWrite` counter, bumped by `build()` and `hideResult()`, cancels a pending write.
- Tests: `hostCount()` and `hostRefusal()` in `tests/helpers.mjs`, LF9 for no site, SONA and Prolific, LF12, two L43 tests, and the new `tests/host-limit.spec.js` H1. H1 measures 8,177 and 8,178 on a fresh slash-padded path in the weekly deployed run.
- The README, `online-collection.Rmd` and NEWS follow, and the M153 NEWS bullet was corrected in place.

**Decisions:** D-093 annotates D-087(d). It covers the miss limit, the 24, 108 and 7 counts, and a refusal that names the count. AC4 was amended once for the cache, with two re-audits. Milestone-local: none.

**Review:**
- The full criteria audit gave 12 points, all fixed. The claim audit read 87 claims and corrected 3.
- The three-lens review gave 25 findings: 12 fixed, 2 follow-up and 11 rejected. The fixes covered the "at their longest" refusal wording, H1's miss check and slash depths, frame and screen-reader wording, and LF12's margin.
- The follow-ups are a new row, "Study link length count for Connect and "Another site"", and the existing "Hosted setup file gaps" row.
- There was no defect return. CI passed: hitop-form 31, and all 8 hitop checks after one 9-minute timeout and resume.
- Hygiene: no lesson added, because LESSONS is at its byte budget and the cache finding lives in `fastly2026limits.md`.
