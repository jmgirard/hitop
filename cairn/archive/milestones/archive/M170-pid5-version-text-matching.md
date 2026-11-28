# M170: PID-5 version refusals and item-text matching

**Status:** done (2026-10-07, PR #178 https://github.com/jmgirard/hitop/pull/178)

**Goal:** Researchers get a clear, classed refusal for a bad PID-5 `version` or an ambiguous item text, and `rename_pid5_items()` matches item text copied with common typographic differences.

**Outcome:**
- `resolve_pid5_version()` (R/util.R) replaces `match.arg()` in `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()`, `label_pid5()`, `validity_pid5()`, `norm_pid5()` and `plot_pid5()`. Full names only, any case, a factor read as its labels. An omitted `version` means FULL. Anything else, prefixes such as "S", "F", "B" and "BFP" included, raises `hitop_unknown_version` with the function as its call. NEWS lists it under breaking changes.
- `rename_pid5_items(method = "text")` matches through `normalize_item_text()`. Typographic quotes count as straight ones, and runs of periods, ellipses and whitespace (no-break space included) at either end are ignored. Two columns matching one item raise `hitop_duplicate_item_match` for every form, one message line per item.
- `score_pid5()` help and SOURCES.md say a prorated half rounds away from zero. A Manipulativeness test pins -0.6 and 0.6. No score changed.
- The internal `write_instrument_json()` reads a spec-named `text_col` (default `Text`) and refuses a missing column. No shipped JSON file changed.
- The six child generators joined the hand-kept generator test lists, and stale counts in test comments were fixed.
- New test file `test-pid5-version.R`. Eight older tests now assert the class.

**Decisions:** D-094 (full names only, Jeff's pre-1.0 waiver, two public classes), D-095 (half away from zero, annotates D-090), D-096 (reverses M031's `version` choice, keeps factors, withdraws a D-094 sentence).

**Review:**
- A criteria audit and one re-audit at plan time. A claim audit read 54 claims and corrected 4.
- One amendment return: AC2's "No function body in R/ changes" was narrowed to the bodies of `round_half_up()` and `apa_mean()`, then re-audited.
- The three-lens fan-out gave 23 findings: 17 fixed, 5 rejected (2 style, 2 planned, 1 false) and 1 follow-up. The follow-up is `rename_hitopsr_items()` duplicate names, part (b) of the "Helpers behind newer conventions" row.
- CI: ubuntu oldrel-1 hung 3.5 hours in `setup-r` and passed on re-run.
- Hygiene: no lesson added (LESSONS at its byte budget).
