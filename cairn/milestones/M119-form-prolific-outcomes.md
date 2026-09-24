# M119: The Prolific route of hitop-form gives each outcome its own end: a second completion address for a saved file, a sent screen before the redirect, and a doubled parameter read as its filled value

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page, its link builder and the package's article
- **Branch/PR:** —

## Goal

Three follow-ons close the Prolific route M118 opened: a link's `completeSaved` field names the address the saved-file screens link to, a confirmed send draws the sent screen before it goes to `complete`, and a Prolific parameter present twice in the address reads as its first filled value.

## Scope

**In:** In hitop-form: `parseLink()` reads `completeSaved`. `showSaved()` links to it. `finish()` draws the sent screen before `location.assign()`. `readProlific()` skips placeholders and blanks among repeated values. The builder gains a "Completion URL after a saved file" field. README, hint and tests. In hitop: the online-collection article's Prolific route, the source note's two added sentences, NEWS. A decision entry annotates D-071(d).

**Out:** Another recruiter's own parameters (SONA, CloudResearch) → the candidate row that remains. What the browser shows on Back after the redirect → nowhere, because the browser decides whether it restores or reloads the page. A "Save the file" button on the unconfirmed screen → its own candidate row. The deployed-page workflow run after the merge → T6, no criterion.

## Acceptance criteria

- [ ] AC1: `parseLink()` accepts `completeSaved` only as an `https://` address with no user name and no password, and only beside a `complete`. It runs the `checkCompleteUrl()` that `complete` and `link.html` use. It refuses any other value, and a `completeSaved` with no `complete`, with a message that names `completeSaved` and shows the value. Tests fire the refusal for `http://localhost`, a string that is no URL, an address with a user name only, an address with a password, a number, and the lone field, each asserting the message names `completeSaved`. One test accepts `https://app.prolific.com/submissions/complete?cc=SAVED123` beside a `complete`.
- [ ] AC2: With `completeSaved` in the link, the two saved screens (no store, unconfirmed send) show a link to that address after the file name. The link's text is the address's host. The screens issue no request to it within five seconds. A confirmed send goes to `complete` and issues no request to `completeSaved`. With `complete` alone, the saved screens link to `complete`, as under M118. Tests: a no-store save and an unconfirmed send with both fields assert that the link's `href` is `completeSaved`. A confirmed send with both fields asserts that the one navigation request's address is `complete` and that no request reaches `completeSaved`. The M118 tests S16 and T16 stay as they are and pass.
- [ ] AC3: With `complete` in the link, a confirmed send replaces the form with the sent screen before it issues the one navigation request. That screen holds the "Thank you" heading, the paragraph "Your responses were sent to the study team.", and a paragraph "Continue to <host>." whose link is the completion address, in place of "You can close this page." When the request is seen, no `nav button` is in the document. Without `complete`, the sent screen is as under M118. Tests: the confirmed walk's route handler for the completion address holds the request open while the test asserts the heading, the paragraph, the link's `href` and the absence of `nav button`, then fulfils it. N7 and T15 today assert that the heading is not drawn before the request. Their titles and assertions are rewritten to assert that it is.
- [ ] AC4: `readProlific()` reads each of `PROLIFIC_PID`, `STUDY_ID` and `SESSION_ID` as the first value of that name in the address that is neither blank nor a `{{%…%}}` placeholder. An address that carries a parameter twice, once filled and once a placeholder or blank, gives the filled value in either order. One that carries it twice filled gives the first. A lone placeholder or blank still reads as absent. Tests: walks under `prolific: true` whose address holds `PROLIFIC_PID` as placeholder-then-filled and as filled-then-placeholder each show no identifier field and save the filled ID as the participant. Walks with `STUDY_ID` doubled the same two ways each write the filled value to `prolific_study`. A unit test of `readProlific()` covers, for each of the three names, blank-then-filled, filled-then-blank and two filled values.
- [ ] AC5: The hitop-form README ("Make a study link", "What the participant sees", the test table), the field's hint in `link.html`, and hitop's online-collection article each tell the researcher two things. First, the new field takes the study's completion code for a saved file, because Prolific lets a study hold one code per outcome, each with its own `?cc=` address. Second, Prolific's "I'll use URL parameters" option appends the three parameters to the study URL itself, so a pasted link with the builder's placeholders carries each parameter once unless that option adds them again, and the page then reads the filled value. The README and the article cite article 445170 for the first and article 445178 for the second, with the API reference's `completion_codes` field for the address shape. `cairn/references/prolific2026help.md` gains article 445170 in its Provenance and the two sentences cited in its Extracted values. hitop's NEWS carries the entry. The article is purled and run against the installed package.
- [ ] AC6: `npx playwright test` is green on the hitop-form checkout. In hitop, `devtools::test()` is clean and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T4
- AC2 → T2, T4
- AC3 → T2, T4
- AC4 → T1, T4
- AC5 → T3, T5
- AC6 → T6

## Tasks

- [ ] T1: In `form.js`, `parseLink()` reads `completeSaved` through `checkCompleteUrl()` with a `bad` message that names the field, and refuses it with no `complete`. The header comment names it. `readProlific()` reads `params.getAll(name)` and takes the first value that is neither blank nor a placeholder.
- [ ] T2: `finish()` draws the sent screen with the "Continue to <host>." paragraph before `location.assign(config.complete)`. `showSaved()` links to `config.completeSaved ?? config.complete`.
- [ ] T3: `link.html` gains the "Completion URL after a saved file" field with its hint. The field is refused through `checkCompleteUrl()`, named in the alert, and refused when the completion field is empty. The link carries `completeSaved`.
- [ ] T4: Tests. Guard for AC1. Save and send for AC2 and AC4, with the `readProlific()` unit test. Send and network for AC3, with N7 and T15 rewritten. Link for the field, which puts `completeSaved` in the link and refuses `http://localhost` and an empty completion field. Plant-check each by reverting the feature it covers once. `git add` the fix before planting. Create any download promise before the walk with its own timeout.
- [ ] T5: Docs: the hitop-form README and hint, the Prolific route in hitop's `online-collection.Rmd`, the source note's page and two sentences, and `NEWS.md`. Purl and run the article.
- [ ] T6: `npx playwright test`, `devtools::test()`, `pkgdown::check_pkgdown()`. Jeff merges the hitop-form PR from his terminal, because the merge guard reads the session's repo. Dispatch the deployed-page run after the merge.

## Work log

- 2026-09-24: created by /milestone-plan, from the ROADMAP's Prolific candidate row (lineage M118 plan gate, review F6 and F9). D-072 annotates D-071(d). Prolific's custom completion codes article and the "automatically append" sentence were read that day and go into the source note at T5.
- 2026-09-24: criteria audit ran in full mode ([O] fresh reader, agent a057a7e9): 13 findings. Eleven fixed before the gate: a user-name-only probe; the refusal names the field; the "unmodified tests" clause became a behaviour with two named tests; the sent screen's paragraphs spelled out; AC3 worded as the document's state at the request, with the two test titles rewritten; the doubled-parameter unit test covers all three names and the blank case; AC5 promises content, the quotations as citations; the 445178 sentence added to the source-note task; AC6's "every new test present" dropped. Reachability: neither IP1 nor D-071 bars the sent-screen link, recorded in D-072. Two findings went to the gate as questions.
- 2026-09-24: plan gate chose one milestone over three because the three items share `finish()`, `showSaved()`, `readProlific()` and one docs pass; falsified by a review return that lands on one item while the others wait.
- 2026-09-24: plan gate chose the first filled value over the last, and over treating differing values as absent, because it matches the page's single-value read today and no Prolific page says where its appending lands; falsified by a Prolific page stating that its parameters follow the pasted ones, or a study whose recorded ID is a hand-typed stale value.
- 2026-09-24: plan gate chose `completeSaved` over `completeAfterSave` and `completeFile` because it pairs with `complete`; falsified by a researcher reading it as a yes-or-no flag.
- 2026-09-24: chose to draw the sent screen before the redirect over a `pageshow` handler that draws it on Back, because a handler depends on the browser's cache choice and a failed navigation leaves nothing; falsified by a participant report of a visible flash before the redirect.

## Decisions

## Review
