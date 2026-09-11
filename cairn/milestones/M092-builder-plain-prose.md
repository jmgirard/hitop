# M092: The HiTOP-SR Module Builder's prose reads as plain human English

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — the prose a builder user reads on the page, in the bundle README, and in the repo README
- **Branch/PR:** `m092-builder-plain-prose` (this repo, tracking), and `m092-plain-prose` in jmgirard/hitop-builder (code)

## Goal

Every sentence a user reads on the builder page, in the bundle's README.txt, and in the builder repo's README meets the plain-English house rules and reads as written by a person.

## Scope

**In:** In the sibling repo `jmgirard/hitop-builder`: a rewrite of the page's static text, every runtime message, the bundle README template, and `README.md` under the house rules. The rules are the plain-English rules the session hook states: sentences of at most 25 words, active voice, simple tenses, no semicolons, no em dashes, no decorative triplets, no bold lead-ins, one word per meaning. An extraction script in the builder's `tests/` that enumerates the user-visible strings. This repo: tracking only, as M090 did.

**Out:** Naming M091's `layout = "printed"` in the builder text → candidate row (lands after M091 ships). The hitop package's own documentation prose → not in scope. Any change to element ids, `data-*` attributes, the `Ready.` status strings the smoke test pins, or page behavior → refused (AC3). The scale names and definitions the page renders from the package → hitop's content (D-038, IP1), untouched.

## Acceptance criteria

- [ ] AC1: Every user-visible prose string meets the house rules. The domain has four parts. (a) The text nodes of `<body>` outside `<script>` and `<style>`. (b) Every string that reaches the page as user-visible text: the arguments of `status()` and `log()` and the runtime notice writers, `FORMATS[].label` and `.button`, `selectionSentence()`, `settingsSummary()`, `refreshSelectAllLabel()`, the `<script nomodule>` message, and the `placeholder` and `aria-label` attributes. (c) The `bundleReadme()` output for each of the three formats. (d) `README.md`. Excluded: the scale names and definitions rendered from the package (`s.Scale`, `s.Brief`), and the pinned `Ready.` and `Ready. <label> chosen.` strings. An extraction script in the builder's `tests/` enumerates (a) to (c), and asserts its writer count against a grep for `textContent`, `innerHTML`, and `setAttribute` writers. The linter at `~/.claude/plugins/marketplaces/local-desktop-app-uploads/simple-english/evals/ste_lint.py`, run with `--type descriptive` on the extraction (code tokens in backticks, URLs bare) and on `README.md`, reports 0 in every rule. Baseline at that type on 2026-09-11: 18 hits over the static text, 122 over `README.md`.
- [ ] AC2: Every fact survives. A second extraction pass collects, per passage, the URLs, code tokens, element ids, `data-*` values, and numbers as an ordered multiset. Run from the branch against a merge-base checkout, the two multisets match passage by passage. Any deliberate removal is listed in the work log with its reason.
- [ ] AC3: Page behavior is unchanged. `npm run smoke` is green locally and `tests/smoke.spec.js` is green in the builder PR's CI. For each of the eight default builds, the bundle's entry names and the decompressed bytes of every entry except `README.txt` match a capture taken from the merge base in the same session. `README.txt` differs only in its rewritten paragraphs, quoted in the review. Every plant whose anchor string the rewrite touched is re-pointed, and the plant matrix is re-run for those plants plus one unplanted control run.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T4
- AC3 → T4

## Tasks

- [x] T1: The extraction script (`tests/prose.mjs`): the four-part domain of AC1, the writer-count assertion, the per-passage fact multiset of AC2, and a `--baseline` mode that writes both to JSON. Run it on the merge base and commit nothing from that run.
- [x] T2: Rewrite the page (`index.html`): static text, runtime messages, and the bundle README template. Keep every id, `data-*` value, and pinned status string. Lint to zero.
- [x] T3: Rewrite `README.md`. Keep its tables and dated verification records. Lint to zero.
- [ ] T4: Evidence. Smoke locally, the eight-build entry-wise comparison against the merge-base capture, the plant re-run for touched anchors, then the builder PR and this repo's tracking PR.

## Work log

- 2026-09-11: created by /milestone-plan, from Jeff's remark at the M091 plan gate that the builder's visuals improved but the prose reads as AI-written. Not a hotfix: a rewrite is not a bug, and not a direct commit: the page is a runtime surface.
- 2026-09-11: criteria audit ran in full mode (fresh Opus reader) and returned nine findings, all fixed. The linter clause named `--type descriptive` (the draft's carve-out was self-contradicting). The enumeration names every user-visible writer (the draft covered `status()` and `log()` only). Package-rendered scale text and the pinned `Ready.` strings are excluded. The bundle comparison is entry-wise against a same-session merge-base capture (no stored baseline exists, and zip bytes are not stable). The CI command is the smoke spec CI runs. The fact check is a per-passage multiset. The plant requirement narrows to touched anchors. The draft's fourth criterion, a human read, moved into the review gate.
- 2026-09-11: plan gate chose a milestone over `/hotfix` because a prose rewrite is not a user-visible bug and needs judgment at review; falsified by nothing, the tier is a routing rule.
- 2026-09-11: implement gate. The linter runs from the plugin path named in AC1 and is not copied into the builder (md5 `d20daa68bc83384d4652a53eb7a59b17` for `ste_lint.py`, `5811fd8013baf9423af23c3e245fc93b` for `slop.tsv`). Log lines that echo an R call (`> generate_…`, `> zip::zip(…)`, `> library(hitop)`, `> installPackages(…)`) stay verbatim and the extraction wraps each whole line in backticks, so the linter reads one code token.
- 2026-09-11: T1 done, builder commit 5cd97b1. `tests/prose.mjs` takes `--ref` for a git ref, `--text` for the linted passages, `--json` for passages with facts, and `--compare` for the fact check (the plan's `--baseline` is these two flags). README.md passages are its sections, one per heading, because a paragraph-level rewrite reflows paragraphs. Facts compare as multisets, sorted on both sides. Merge-base run: 153 passages, 18 writer sites, the linter at 57 over parts (a) to (c) (the plan's 18 counted the static body text alone) and 122 over `README.md`.
- 2026-09-11: T2 done, builder commit c11e95e. Page extraction lints 0 in every rule and every passage keeps its facts. The three `<strong>` lead-ins in the notices are removed (a house rule the linter cannot see, and no CSS targets them). Local smoke 1/1 and runtime probes 2/2 green on the rewritten page. No plant anchor is touched (the plants anchor code lines only).
- 2026-09-11: T3 done, builder commit 80a650e. `README.md` lints 0 in every rule, longest sentence 25 words. Two deliberate fact changes: the quoted `README.txt` block follows the rewritten template (its last sentence), and the layout table gains the `tests/prose.mjs` row. Bold in the README became italic or plain.

## Decisions

## Review
