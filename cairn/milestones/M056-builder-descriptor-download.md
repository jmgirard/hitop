# M056: The browser builder offers the module descriptor as a download

- **Status:** planned
- **Priority:** normal
- **Depends on:** M055
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Branch/PR:** —

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

- [ ] T1 Wire the descriptor into the app's build step, calling M055's
      `descriptor` argument and offering the result as a download.
- [ ] T2 Page copy: the descriptor's purpose, and the shuffled-form notice
      rewrite.
- [ ] T3 Drive the page and capture the three descriptors and the shuffled run,
      using the blob-capture and `read_page`/`form_input` techniques the M045,
      M050, and M052 lessons record.
- [ ] T4 Close DESIGN known issue 8 and the two candidate rows it settles,
      recording each disposition.

## Work log

- 2026-08-24: created by /milestone-plan.
- 2026-08-24: plan chose a separate download over bundling the descriptor into the REDCap zip or the Word file because the descriptor must accompany all three formats and only one of them is an archive; falsified by visitors losing the second file often enough that bundling would help.
- 2026-08-24: fresh-context criteria audit ran in FULL mode (user-facing tier) and returned findings on two criteria, both repaired before commit. The shuffled-run criterion asserted only the length and the set of `itemOrder`, which a permutation unrelated to what the form actually prints satisfies — the very failure known issue 8 names — so it now compares against the sequence parsed out of the downloaded DOCX. A criterion binding the removal of known issue 8 and two ROADMAP rows bound tracking records rather than the deliverable and was dropped; T4 carries that work and the review's consistency gate checks it. The remaining criteria returned nothing.

## Decisions

## Review
