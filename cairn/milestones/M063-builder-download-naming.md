# M063: Every file the builder writes says which form it belongs to

- **Status:** review
- **Priority:** normal
- **Depends on:** M062
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m063-builder-download-naming` (hitop); hitop PR #70 https://github.com/jmgirard/hitop/pull/70; builder PR #9 https://github.com/jmgirard/hitop-builder/pull/9 (merged as `afb1535`)

## Goal

Name every download for the format that built it, whether it covers the whole
questionnaire, and whether a Word form was shuffled, so two builds in one session
cannot be confused or shadow each other's descriptor.

## Scope

Surface tier: **user-facing** — the deployed page's visitors are external
consumers of this deliverable.

**In:** the download stem in `download()` and the descriptor's stem, in
`jmgirard/hitop-builder`'s `index.html`, plus the filenames named in that repo's
`README.md` and page copy.

**Out:** recovering the scale selection, paper size, item numbering or the
Qualtrics/REDCap naming values from a filename — those stay readable only from
the descriptor travelling beside the file, so two different scale selections in
the same format still share a filename. A settings digest in the stem was weighed
and declined at this plan gate (work log). The descriptor's JSON format, and
which fields it carries, are untouched. The handover mechanism → M062.

## Acceptance criteria

- [x] AC1 Over the eight builds the driven run constructs — the page's own three
      `FORMATS` keys crossed with a whole-scale and a proper-subset selection,
      and for the Word format only crossed again with the shuffle control — the
      sixteen filenames the page requests are pairwise distinct. Verified from
      the recorded `download` attributes.
- [x] AC2 Each of those sixteen filenames equals the name its build's format,
      wholeness and shuffle settings determine, checked against the table of
      expected stems written in T1 from those settings and never read off the
      page.
- [x] AC3 In each of those eight recorded pairs, the descriptor and the
      questionnaire file share a stem and differ only in extension.
- [x] AC4 Every string matching `[A-Za-z0-9._-]+\.(docx|txt|zip|json)` in
      `index.html` and `README.md` is triaged, and each hit that names a file the
      page writes names one this scheme produces.
- [x] AC5 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [x] AC6 The `hitop` package is untouched — `git diff --name-only` against the
      merge base lists only paths under `cairn/` — and `devtools::test()` is
      clean.

## Coverage

- AC1 → T2, T4
- AC2 → T1, T4
- AC3 → T2, T4
- AC4 → T3, T5
- AC5 → T6
- AC6 → T2, T3, T6

## Tasks

- [x] T1 Write the naming table: for each format x wholeness x shuffle
      combination, the expected stem, derived from the settings alone.
- [x] T2 Build the stem in `download()` from the format key, `wholeInstrument()`
      and the shuffle state, and give the descriptor the same stem
      (`hitop-builder/index.html:1008-1019`).
- [x] T3 Update `README.md`'s per-format download descriptions and any page copy
      naming a written file.
- [x] T4 Drive the eight builds in the browser pane, actuating the page's own
      controls, and record the sixteen requested filenames.
- [x] T5 Run the AC4 grep over both files and triage every hit.
- [x] T6 Open the builder pull request; after merge, fetch the deployed page and
      compare bytes; write the URL into the header.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 3 findings on this milestone — a promise quantifying over naming axes the enumeration omitted, a universal over "the same build" naming no procedure, and a filename regex excluding digits and underscores so `hitopsr-a4.docx` escaped it — all fixed before the criteria were written.
- 2026-08-28: plan gate chose three recoverable axes in the name over a short digest of the full settings (rejected: uglier names carrying a code that means nothing to a reader, for a collision only two same-format builds with different scale sets can hit) and over naming the format alone (rejected: leaves a shuffled Word form and an unshuffled one indistinguishable, which is the confusion this milestone exists to close); falsified by a report of two same-format builds with different scale selections being confused on disk.
- 2026-08-29: implement gate chose the format words `word`/`qualtrics`/`redcap` over the page's `docx`/`txt`/`zip` keys, and kept `-module` with the whole instrument unmarked over an explicit `-full`; naming table written (M063-D1).
- 2026-08-29: T2 — `downloadStem(format, whole, shuffle)` composes the stem from `FORMATS[].name`, wholeness and shuffle; `download()` calls it for both files (builder `e660cee`). Page served from the branch boots clean: webR installed, 76 scales, status Ready.
- 2026-08-29: T4 — eight builds driven on the branch page (localhost copy): three formats x whole/two-scale selection, the Word pair crossed with the shuffle box. Sixteen saves recorded off a patched `HTMLAnchorElement.click`, eight `build` and eight `visitor`, all sixteen names distinct; each pair shares a stem and differs only in extension; all eight match the M063-D1 table typed independently, and planting one wrong expectation put that build in the mismatch list, so the comparison can fail. The eight logged R calls confirm the settings apart from the names: four with no `module`, four with `module = <2 scales>`, `randomize = TRUE` on exactly the two shuffled builds.
- 2026-08-29: T4's method refined from the plan's `read_page` refs to actuating the page's own controls from the console — the recorded lesson that a ref click can land in a viewport dead band and report a coordinate anyway, where the page's handlers are exact.
- 2026-08-29: T3 — README's three filename passages rewritten to the new stems, a *What the downloads are named* section added tabulating the eight builds' pairs, and the page's two-files notice gained a sentence on what a name carries; the notice re-read off the served branch copy. Written from T4's recorded names, not composed (builder `a03db3b`).
- 2026-08-29: T5 — the AC4 grep returns 17 distinct hits across both files. Sixteen name downloads and each is one of the sixteen names T4 recorded; the seventeenth, `module.json` at `index.html:1088`, is the descriptor's path inside webR's virtual filesystem and never a name the browser is asked to save. No recorded name goes unmentioned.
- 2026-08-29: T6 first half — builder PR #9 opened from `m063-download-naming`. Its second half (fetch the deployed page after merge and compare bytes) can only run past the merge, so it and AC5 land at the review gate; T6 stays unticked until then.
- 2026-08-29: implement closes with `devtools::test()` FAIL 0 / WARN 0 / SKIP 4 / PASS 15504 and `git diff --name-only origin/main...HEAD` listing only `cairn/ROADMAP.md` and this file. Every task but T6 is checked; T6's remaining half and AC5 with it cannot run before PR #9 merges.
- 2026-08-29: review — AC1-AC4 and AC6 verified with fresh evidence (eight driven builds, sixteen distinct names matching independently composed stems, the AC4 grep set-equal to them, `check()` 0/0/0, `test()` PASS 15504); AC5 waits on builder PR #9 merging. Three-lens fan-out returned 8 findings, all in prose; none falsifies a criterion.
- 2026-08-29: gate triage — Jeff chose fix-wording-then-ship. Four prose findings fixed in builder `aef66b8` (two false never-collide claims, the self-contradicting `downloadStem` comment, one over-long line), three deferred, none rejected. Builder PR #9 merged as `afb1535`; the deployed page is byte-identical to it, so AC5 and T6 close.

## Decisions

### M063-D1 (2026-08-29): A download's stem is the instrument, the format's word, `-module`, `-shuffled`

The stem every build writes is `hitopsr`, then the format's own word, then
`-module` when the build covers a selection of scales rather than the whole
instrument, then `-shuffled` when a Word form's printed item order was
shuffled. The questionnaire takes its format's extension and the scoring file
takes `.json` on that same stem. Chosen at the implement gate over the page's
three format keys (`docx`/`txt`/`zip`), which name the file type twice on the
questionnaire, and over marking the whole instrument with a word of its own,
which would rename files visitors already hold.

Shuffle is a Word-only control, so the eight builds are three formats crossed
with two selections, the Word pair crossed again with the shuffle box:

| Format | Selection | Shuffled | Stem |
|---|---|---|---|
| Word | whole instrument | no | `hitopsr-word` |
| Word | whole instrument | yes | `hitopsr-word-shuffled` |
| Word | some scales | no | `hitopsr-word-module` |
| Word | some scales | yes | `hitopsr-word-module-shuffled` |
| Qualtrics | whole instrument | — | `hitopsr-qualtrics` |
| Qualtrics | some scales | — | `hitopsr-qualtrics-module` |
| REDCap | whole instrument | — | `hitopsr-redcap` |
| REDCap | some scales | — | `hitopsr-redcap-module` |

## Review

Reviewed 2026-08-29. Branch merged up to date with `origin/main` (`d43d8fc`); no
merge was needed. hitop PR #70 (draft, tracking-only)
https://github.com/jmgirard/hitop/pull/70; builder PR #9
https://github.com/jmgirard/hitop-builder/pull/9.

**Evidence per criterion.** All browser evidence is a fresh review-time run
against the branch page served from `../hitop-builder` on `localhost:8788`, the
eight builds driven end to end from the page's own controls, with
`HTMLAnchorElement.prototype.click` patched to record every `download`
attribute and suppress the save.

- AC1 — the eight-build run recorded 16 requested filenames; `new Set(names).size`
  is 16 over 16, so they are pairwise distinct.
- AC2 — each build's pair was compared against a stem composed at review time
  from the build's own format, wholeness and shuffle settings, never read off
  the page: 8 of 8 matched, 0 mismatches. Discrimination: re-running the same
  comparison against a planted expectation that drops `-shuffled` reddens
  exactly the two shuffled Word builds, so the comparison can fail. The eight
  stems also equal the M063-D1 table row for row, and the run's own settings are
  independently confirmed by the page tally (76 of 76 scales on the four whole
  builds, 2 of 76 on the four subset builds) and by the shuffle box's state.
- AC3 — in all eight pairs the two names share a stem and differ only in
  extension, the questionnaire taking its format's extension and the second file
  `.json`; asserted per pair, 8 of 8.
- AC4 — the AC4 regex over `index.html` and `README.md` returns 17 distinct
  hits. Sixteen are exactly the 16 names the run recorded, set-equal in both
  directions: no hit outside the recorded set but one, and no recorded name
  unmentioned. The one remaining hit, `module.json` at `index.html:1088`, is
  `descPath` — a path inside webR's virtual filesystem, never a name the browser
  is asked to save.
- AC5 — builder PR #9 merged as `afb1535`, its URL in this file's header. The
  page fetched from `https://jmgirard.github.io/hitop-builder/index.html` after
  the Pages deploy is 57,732 bytes with SHA-256 `e6605ea0…49ce7`, equal to
  `git show afb1535:index.html`; `cmp` reports no difference. Discrimination:
  the same `cmp` against the pre-merge commit `9f8b615` differs, so the
  comparison can fail.
- AC6 — `git diff --name-only origin/main...HEAD` lists `cairn/ROADMAP.md` and
  this file only; `devtools::test()` gave FAIL 0 / WARN 0 / SKIP 4 / PASS 15504.

**Consistency gate.** `cairn_validate.py` exit 0, all 16 checks PASS, 21 advisory
warnings (20 dangling `D-00x` tokens predating this milestone, 1 references
staleness), none a gate failure. No `DESIGN.md` principle changed, so
`cairn_impact.py` was skipped. Toolchain slot: `devtools::document()` and
`devtools::build_readme()` each left the tree clean, `pkgdown::check_pkgdown()`
found no problems, `devtools::check()` was 0 errors / 0 warnings / 0 notes, and
NEWS.md takes no entry because the R package's behavior is unchanged. Byte
budgets measured by hand: `ROADMAP.md` 39,318 bytes and `LESSONS.md` 25,286
bytes both exceed their budgets — pre-existing, remedied at the hygiene pass.

**No Driving RR**, so no projection-vs-outcome pairs.

**Independent review.** Three fresh-context lenses, distinct evidence bases.
[S] prior-PR-comments: no findings — both GitHub inline-comment probes came back
empty, and the archived Review sections on these files (M056, M062) show this
milestone closing M056's cross-format collision finding without disturbing
M062's handover logic. [S] blame-history: one finding, the stale M062
disable-guard rationale, which the [O] lens raised as its finding 6. [O]
diff-bug: `downloadStem` itself correct over every reachable combination, with
seven findings in prose.

**Triage.** All eight findings were surfaced at the gate; Jeff chose to fix the
wording and ship. Four fixed now, on the builder branch, in `aef66b8` before the
merge:

- Fixed — `README.md:150-152`, *"Two builds made in one session therefore never
  arrive under the same name, so neither can overwrite the other's scoring file
  in your downloads folder."* False, and contradicted by the same section twenty
  lines down.
- Fixed — `index.html:526-531`, the same claim in visitor-facing copy: *"so a
  second build never lands on top of the first."*
- Fixed — `index.html:641-648`, `downloadStem`'s own comment asserting both
  *"two builds made in one session never arrive under one name, and no build's
  descriptor can shadow another's"* and *"two different scale selections in one
  format do still share a filename."*
- Fixed — `README.md:76`, a 99-character line in a file wrapped at ~76,
  introduced by the T3 edit. The whole walkthrough paragraph was rewrapped.

The three corrected passages are now written against an observed run rather than
composed: two Word builds driven back to back on the branch page, the first over
`agoraphobia` + `antisocialBehavior` and the second over `appearanceFocus` +
`appetiteLoss`, both requested `hitopsr-word-module.docx` and
`hitopsr-word-module.json` — the collision the old prose denied.

Deferred to follow-up, no change on this branch:

- `index.html:539-542`, `index.html:1073-1077`, `README.md:79-80` — the
  handover-disable guard's rationale, *"under the name the new one is about to
  take"*, was written when every build shared one of two stems and now overstates
  the hazard. Behavior correct and still needed.
- `README.md:148` / `index.html:528` — *"`-module` unless you ticked every
  scale"* omits `wholeInstrument()`'s `tilesExactly` gate. True of HiTOP-SR as
  shipped; `README.md:243` carries the same shape and predates this diff.
- M063-D1 rejects marking the whole instrument because it *"would rename files
  visitors already hold"*, but the format word renames those same files anyway
  (`hitopsr.docx` → `hitopsr-word.docx`). The rationale is wrong; the choice
  stands, and the entry is history, so it is not edited (IP4). Nothing tells a
  returning visitor the names changed.

None was rejected. No finding demonstrated an acceptance criterion failing, so
the return floor did not fire; the first two are false statements to users, which
is why they were fixed before the merge rather than after.
