# M063: Every file the builder writes says which form it belongs to

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M062
- **Driving RR:** —
- **Principles touched:** GP3
- **Branch/PR:** `m063-builder-download-naming` (hitop); `m063-download-naming` (hitop-builder)

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

- [ ] AC1 Over the eight builds the driven run constructs — the page's own three
      `FORMATS` keys crossed with a whole-scale and a proper-subset selection,
      and for the Word format only crossed again with the shuffle control — the
      sixteen filenames the page requests are pairwise distinct. Verified from
      the recorded `download` attributes.
- [ ] AC2 Each of those sixteen filenames equals the name its build's format,
      wholeness and shuffle settings determine, checked against the table of
      expected stems written in T1 from those settings and never read off the
      page.
- [ ] AC3 In each of those eight recorded pairs, the descriptor and the
      questionnaire file share a stem and differ only in extension.
- [ ] AC4 Every string matching `[A-Za-z0-9._-]+\.(docx|txt|zip|json)` in
      `index.html` and `README.md` is triaged, and each hit that names a file the
      page writes names one this scheme produces.
- [ ] AC5 The change ships: a merged pull request in `jmgirard/hitop-builder`
      whose URL is in this file's header, and the page served at
      `https://jmgirard.github.io/hitop-builder/` matches that commit's
      `index.html` byte for byte.
- [ ] AC6 The `hitop` package is untouched — `git diff --name-only` against the
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
- [ ] T3 Update `README.md`'s per-format download descriptions and any page copy
      naming a written file.
- [ ] T4 Drive the eight builds in the browser pane through `read_page` refs and
      `form_input`, recording the sixteen requested filenames.
- [ ] T5 Run the AC4 grep over both files and triage every hit.
- [ ] T6 Open the builder pull request; after merge, fetch the deployed page and
      compare bytes; write the URL into the header.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 3 findings on this milestone — a promise quantifying over naming axes the enumeration omitted, a universal over "the same build" naming no procedure, and a filename regex excluding digits and underscores so `hitopsr-a4.docx` escaped it — all fixed before the criteria were written.
- 2026-08-28: plan gate chose three recoverable axes in the name over a short digest of the full settings (rejected: uglier names carrying a code that means nothing to a reader, for a collision only two same-format builds with different scale sets can hit) and over naming the format alone (rejected: leaves a shuffled Word form and an unshuffled one indistinguishable, which is the confusion this milestone exists to close); falsified by a report of two same-format builds with different scale selections being confused on disk.
- 2026-08-29: implement gate chose the format words `word`/`qualtrics`/`redcap` over the page's `docx`/`txt`/`zip` keys, and kept `-module` with the whole instrument unmarked over an explicit `-full`; naming table written (M063-D1).
- 2026-08-29: T2 — `downloadStem(format, whole, shuffle)` composes the stem from `FORMATS[].name`, wholeness and shuffle; `download()` calls it for both files (builder `e660cee`). Page served from the branch boots clean: webR installed, 76 scales, status Ready.

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
