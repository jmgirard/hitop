# M056: The browser builder offers the module descriptor as a download

- **Status:** review
- **Priority:** normal
- **Depends on:** M055
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Branch/PR:** `m056-builder-descriptor-download` (hitop) · `m056-descriptor-download` (hitop-builder)

## Goal

Give a visitor to the browser module builder the descriptor file that scores
the data their generated form collects, including a shuffled form's printed
order.

## Scope

Surface tier: **user-facing** — a control and a download on a public page.

**In:** a descriptor download on the builder page
([jmgirard/hitop-builder](https://github.com/jmgirard/hitop-builder)) beside
whichever instrument file the visitor builds, written by M055's `descriptor`
argument; the page's copy telling the visitor what the file is for and to keep
it with their data; the shuffled-form notice rewritten now that the page has a
record to hand over; the DESIGN known-issue-8 remainder closed.

**Out:** package-side changes of any kind → M054 and M055. Any control
exposing a generator argument not already signed off → the gate, per D-038.
Distinguishing shuffled from unshuffled download filenames → the standing
candidate row from the M048 implementation gate.

## Acceptance criteria

- [ ] AC1 For each of the three formats the page offers, driving the page to
      build a module form yields a descriptor download whose bytes, captured
      in-page by the `URL.createObjectURL` patch the M050 lesson records, parse
      as M054's format and name exactly the scales ticked in the run.
- [ ] AC2 A run with every scale ticked and shuffling on yields a descriptor
      whose `itemOrder` equals the item sequence parsed out of the downloaded
      DOCX itself, read from the form's own rows in the parse-and-compare style
      D-010 uses — never merely its length or its set.
- [ ] AC3 The page states, in copy a `read_page` capture shows, what the
      descriptor is for and that it must be kept with the collected data; the
      shuffled-form notice no longer tells the visitor to keep their own record
      of an order the page never gave them.
- [ ] AC4 The deployed page loads and builds all three formats after the
      change, evidenced by a driven run in the review.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T3

## Tasks

- [x] T1 Wire the descriptor into the app's build step, calling M055's
      `descriptor` argument and offering the result as a download.
- [x] T2 Page copy: the descriptor's purpose, and the shuffled-form notice
      rewrite.
- [x] T3 Drive the page and capture the three descriptors and the shuffled run,
      using the blob-capture and `read_page`/`form_input` techniques the M045,
      M050, and M052 lessons record.
- [x] T4 Close DESIGN known issue 8 and record the disposition of the candidate
      row it settles; the row itself graduates at the post-merge hygiene pass.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan chose a separate download over bundling the descriptor into the REDCap zip or the Word file because the descriptor must accompany all three formats and only one of them is an archive; falsified by visitors losing the second file often enough that bundling would help.
- 2026-08-24: implementation gate chose, on recommendation, one click saving both files over a second button or an opt-out tick-box; the descriptor taking the questionnaire's own stem (`hitopsr-module.json` beside `hitopsr-module.docx`); and a standing notice above the download button over a sentence in the small print under it.
- 2026-08-24: T1+T2 in `jmgirard/hitop-builder` commit `80d0c5e` — all three generator calls pass `descriptor = desc_path`, `saveFile()` hands over both files with both read before either is saved, the step-three notice and the download hint state the pair, the shuffled whole-instrument sentence points at the descriptor instead of the visitor's own record, and README gains a *The scoring file* section with an example built from a two-scale shuffled module.
- 2026-08-24: r-universe's WebAssembly build of `hitop` still served commit `80c3601`, one behind M055's merge, so T3's driven capture waits on the rebuild — observed 2026-08-24 20:15 CDT.
- 2026-08-24: driven smoke run against the branch page served locally, with the M050 blob-capture patch installed: the Word build reached R as `generate_docx_hitopsr(file = out_path, descriptor = desc_path, module = .m, papersize = paper_size, renumber = TRUE, randomize = FALSE)` and failed `unused argument (descriptor = desc_path)` against the served pre-M055 build, with zero blobs captured — so a failed build hands over neither file. The step-three notice and the rewritten download hint were read back from the live page.
- 2026-08-24: blocked on r-universe. Its `Update universe` sync last ran 2026-08-24 23:55Z against an hourly cadence and had not run again by 01:42Z, so the WebAssembly binary still serves commit `80c3601`, one behind M055's merge, and no `descriptor` argument exists in the browser. T3's in-page capture and T4, which reads T3's evidence, both wait on the rebuild; resume once `https://jmgirard.r-universe.dev/api/packages/hitop` reports `RemoteSha` at `a3bddd1` or later — observed 2026-08-24.
- 2026-08-24: fresh-context criteria audit ran in FULL mode (user-facing tier) and returned findings on two criteria, both repaired before commit. The shuffled-run criterion asserted only the length and the set of `itemOrder`, which a permutation unrelated to what the form actually prints satisfies — the very failure known issue 8 names — so it now compares against the sequence parsed out of the downloaded DOCX. A criterion binding the removal of known issue 8 and two ROADMAP rows bound tracking records rather than the deliverable and was dropped; T4 carries that work and the review's consistency gate checks it. The remaining criteria returned nothing.

- 2026-08-24: r-universe's rebuild landed — `https://jmgirard.r-universe.dev/api/packages/hitop` reports `RemoteSha` `a3bddd1`, M055's merge — so the blocker cleared and the milestone returned to in-progress.
- 2026-08-24: T3 drove the branch page served at `localhost:8788` with the M050 blob-capture patch. Word, Qualtrics and REDCap runs over the same two ticked scales (Agoraphobia, Binge Eating) each handed over two files (`hitopsr-module.<ext>` plus `hitopsr-module.json`); the three captured descriptors are byte-identical, name exactly the two ticked scales, and the captured bytes parse with the package's own `read_module()` as `<hitop_module> hitopsr: 8 items from 2 scales`. A fourth run with all 76 scales ticked and shuffling on handed over `hitopsr.docx` + `hitopsr.json`, the descriptor carrying `itemOrder` of length 405.
- 2026-08-24: T3's order check parsed the downloaded DOCX in-page (central-directory walk, `DecompressionStream('deflate-raw')`, item rows read out of `word/document.xml`), mapped each printed item's own text back to its HiTOP-SR number through a map generated from `hitopsr_items`, and compared position by position: all 405 printed rows matched a known item, printed numbers ran 1..405, and the recovered sequence equals the descriptor's `itemOrder` exactly, a real permutation rather than the identity. The comparison was shown able to fail — swapping one adjacent pair, rotating the sequence by one, and altering one item's text each break it while the control passes.

- 2026-08-24: T3 also captured the page copy on the shuffled Word screen: the standing notice reads "Two files are saved, and both matter. Beside the questionnaire, this page saves a small .json file naming the scales you chose — and, on a shuffled Word form, the order the items were printed in." followed by "Keep it with the responses you collect."; the shuffled-form notice now reads "Nothing on the paper records the order it came out in; the .json file saved with the form does, and is the only copy of it." — no sentence asks the visitor to keep a record of their own. `read_page` truncates a long text node, so the copy was read back with `get_page_text` over the same driven page.

- 2026-08-24: T4 removed DESIGN known issue 8, whose whole remainder was the app change this branch makes: the page now passes `descriptor =` on all three formats, hands the file over, and its notice points at that file rather than at a record the visitor was never given. Nothing else in DESIGN referenced it and the list needed no renumbering.
- 2026-08-24: T4 disposition of the candidate row the issue settles. The row planning merged from two (the shuffled form's lost order, and downloads too alike to tell apart) is settled in its order half only: M056 gives the visitor the printed order in the descriptor. Its naming half stands — every module download is still `hitopsr-module.<ext>`, and a whole-instrument one `hitopsr.<ext>` whether shuffled or not, so a shuffled Word form and an unshuffled one remain indistinguishable on disk. The row is left in place for the post-merge hygiene pass to narrow to that half, per the candidates-graduate-at-completion rule; T4's task text was refined to say so.

- 2026-08-24: all tasks checked; `devtools::test()` clean (0 failures, 0 warnings, 1 skip, 14419 passing). Status to review.

## Decisions

## Review
