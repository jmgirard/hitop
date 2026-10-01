# M150: Study Link Builder: hint facts, names and tutorials

- **Status:** in-progress
- **Priority:** high
- **Depends on:** M149
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — the researcher page's hints and the package tutorials that describe it
- **Branch/PR:** m150-link-builder-hint-facts, companion: /Users/jmgirard/github/hitop-form m150-link-builder-hint-facts

## Goal

The Study Link Builder's hints give back the facts that M146 cut, and the package pages name its parts as the page shows them.

## Scope

**In:** In hitop-form `link.html`, six facts return to named hints within the word limits. The builder stops calling itself "this page", and the `form.js` messages it shows name the online form. In hitop, the tutorials that walk through the builder say which closed section holds each field they name, and `modules-hitopsr.Rmd` lists all five forms. D-083's names reach `pid5_scoring.Rmd`, `overview.Rmd` and the download pages' strip (D-084). NEWS.md gets an entry.

**Out:**
- M149 takes the page's behavior. M151 takes the test reach.
- The hitop-form README keeps its detail sections. Hints link to them as they do now.
- `form.js:671` (`answerFault()`) says "this page" on the online form only, and M148 owns that text.
- The R help pages keep their names (D-083).
- The clash of "module" with the HiTOP-HSUM's name stays in the "User-facing names" candidate row.

## Acceptance criteria

- [ ] AC1: Each fact below shows in the named `link.html` hint. A Playwright test asserts each one by a phrase.
  - The SONA hint says that the survey code becomes the participant identifier, and it names the "Completion URL" field.
  - The "Completion URL after a decline" hint says that `{participant}` goes after the address's `?` or `#`.
  - The "Completion URL after a saved file" hint names the two cases that save a file: a link with no place for responses, and a send that is not confirmed.
  - The "Completion URL" hint says that for Prolific it is the completion URL shown on the study's page.
  - The module hint says that with several instruments the module applies to the HiTOP-SR among them.
  - The "Publishable key" hint says that an opened study link puts the key in GitHub Pages' logs. This holds on the Study Link Builder and on the online form.
  The hints keep each fact that the S9 test asserts at hitop-form `a2d74e3`, and the test's phrases for them can be reworded. Each `.hint` and `.site-hint` element holds at most 40 words. This covers the markup of `link.html` and the hints that a question group of each type adds. The intro, from the `<h1>` to the form, holds at most 60.
- [ ] AC2: A case-insensitive grep for `this page` runs over `link.html`, and over the bodies of the `form.js` functions that `link.html` imports (`link.html:262-265`). In `link.html`, each remaining hit is a code comment, or quotes the online form's own screen text. In the imported functions, each hit now says "the online form". At `a2d74e3` these hits are `form.js:101`, `:881`, `:1050` and `:1135`. A Playwright test fires each reworded message on the builder and asserts the new text. For `:881` it first adds an option to the destination menu.
- [ ] AC3: Three tutorials are searched: `vignettes/articles/online-collection.Rmd`, `vignettes/articles/modules-hitopsr.Rmd` and `vignettes/pid5_scoring.Rmd`. Each is searched with its line breaks joined into spaces, case-insensitively, for each field label inside `details.optional` in the markup of `link.html`. For the "Participant" label, the search terms are `participant field` and `"Participant"`. Each hit that tells the reader to fill in, choose or tick that field on the Study Link Builder names the section that holds it. `online-collection.Rmd` says that the optional parts sit in closed sections, and that the finished link shows under "Your study link". The sentence at `modules-hitopsr.Rmd:405-407` names PID-5, PID-5-SF, PID-5-BF, HiTOP-SR and HiTOP-BR.
- [ ] AC4: A case-insensitive PCRE search (`rg -i -P`) runs for each D-083(b) pattern over `vignettes/pid5_scoring.Rmd` and `vignettes/articles/overview.Rmd`. Each remaining hit sits in R code, a comment, a code identifier or a URL. A link's visible text is not a URL. The five download pages that call `online_strip()` are each `vignettes/articles/download-*.Rmd` except `download-hitophsum.Rmd`. On each, the strip's button text contains "Study Link Builder". The strip's text contains "online form" and names the participant page no other way. The same search over the rendered strip finds only the `?c=` in the button's `href`. A test in `tests/testthat/test-download-pages.R` asserts these three facts on each of the five pages.
- [ ] AC5: The hitop-form Playwright suite passes locally and on its PR's CI. In hitop, NEWS.md has an entry for the hints and the tutorials. `devtools::check()` gives 0 errors and 0 warnings, `pkgdown::check_pkgdown()` passes, and each changed article builds with `pkgdown::build_article()`.

## Coverage

- AC1 → T1, T5
- AC2 → T2, T5
- AC3 → T3, T6
- AC4 → T4, T6
- AC5 → T5, T6

## Tasks

- [x] T1: Rewrite the six hints with the facts, each within 40 words. They are at `link.html:168`, `206`, `217`, `213` and `180`, and the key hint at `:142`. The prior wording at hitop-form `236d2f1` and README sections 130-152, 102-104 and 219-236 give the facts. Extend the S9 facts test, and reword its old phrases where a hint changes.
- [x] T2: Reword the intro (`link.html:106`), the table-name hint (`:146`) and the prefill refusal (`:893`). The intro has 2 words of room: by the test's count it is at 58 of 60. Reword the four `form.js` messages. Update the existing specs that assert the old text.
- [x] T3: Run the AC3 search, and record each hit and its class in the work log for review to re-run. Name the section for each field a hit sends the reader to, and describe the closed sections and "Your study link". Fix the form list at `modules-hitopsr.Rmd:405-407`. Build the articles.
- [x] T4: Clear every hit that is not exempt. Start from `pid5_scoring.Rmd:198`, `:200` (the link text) and `:202`, `overview.Rmd:69`, and the strip strings at `_download-helpers.R:179-189`. Add the strip assertions to `tests/testthat/test-download-pages.R`, which today checks only the `href`, the article link and the JSON button (`:88-118`). Record the AC4 searches and each hit's class in the work log.
- [ ] T5: Name the S9 hint-fact tests and the stale-build tests in the README row for `tests/link-sections.spec.js` (M146 review). Run the full suite locally and on the PR.
- [ ] T6: Add the NEWS.md entry. Run `devtools::check()` and `pkgdown::check_pkgdown()`, and build the changed articles.

## Work log

- 2026-09-30: created by /milestone-plan from the "Study Link Builder follow-ups" row, with M149 and M151.
- 2026-09-30: plan put the Supabase-key fact in the key hint rather than the intro. The intro has 2 words of room, and says already that an opened link reaches the host's logs. Falsified by a researcher who misses the key fact because they never open the Supabase fields.
- 2026-09-30: the criteria audit ran in full mode with a fresh Opus reader. It returned 10 findings, and each was repaired as suggested. AC1 states the word limit over the markup, not over a test's walk, and lets the S9 phrases change. AC2 greps the imported `form.js` functions: `:1135` joins, and `:671` leaves for M148. AC3 joins line breaks and searches without quotes. The intro count is 58, not 47, and two task line numbers were corrected.
- 2026-09-30: plan gate split the row into M149 to M151 and took the old names in three files into M150 (D-084). AC4 is new, and it went back to a fresh Opus reader in full mode. It returned 10 findings, and each was repaired as suggested. The search is PCRE, and it reads the strip as rendered on its five pages. Link text is not a URL, and a test asserts the strip.
- 2026-09-30: plan gate kept the 40-word hint limit over a 50-word limit, because short hints were the aim of M146. Falsified by a hint that cannot hold its fact in 40 words without losing a second fact.
- 2026-09-30: implement started on branch `m150-link-builder-hint-facts` in hitop and in hitop-form, both cut from their pushed `main`. The question gate was skipped: the plan left no choice open.
- 2026-09-30: T1 done. The six hints hold their facts within 40 words, and S9 asserts each by a phrase. The SONA, decline, saved-file, completion and module hints changed their old S9 and L-test phrases, which were reworded in `link-sections.spec.js` and `link.spec.js`. The two specs pass, 204 of 204.
- 2026-09-30: T2 done. The intro, the table-name hint and the prefill refusal say "the Study Link Builder". The four `form.js` messages, now at `:105`, `:891`, `:1061` and `:1146`, say "the online form". New test L35 in `link.spec.js` fires all four on the builder and asserts each whole message. It went red against the old `form.js`. Six specs that asserted the old text were updated, and they pass.
- 2026-09-30: T3 done. The AC3 search script read 12 labels from the five closed sections, and searched the three tutorials with line breaks joined. `pid5_scoring.Rmd` has 0 hits. Offsets below are into the joined text after the edits.
- 2026-09-30: T3 `online-collection.Rmd`, instruction hits, each now naming its section. Module file @4114/@4125. Prolific site and participant field @15665/@15715/@15751. SONA or Connect site and participant field @19542/@19592/@19628. Prolific "Completion URL" @17253/@17580. Saved-file URL @18679/@18731. SONA "Completion URL" @20947. Connect "Completion URL" @22866. "Consent text" @23446. Decline URL @24781. Questions file @27603.
- 2026-09-30: T3 `online-collection.Rmd`, other hits: R code @14169 and @19251. Descriptive @4544, which is the section name itself. Descriptive @7893, which also names the section now. Descriptive with no instruction: @10519, @11130, @11231, @18849, @18904, @19362, @20595, @21257, @21364, @21822, @22230, @23254, @23288, @24296, @24502, @24969, @25120, @25186, @28604 and @29233.
- 2026-09-30: T3 `modules-hitopsr.Rmd`: instruction hits @15479/@15692 (module file) name "Item order and HiTOP-SR module". The random-order box @15991/@16101 is descriptive and says "in the same section". The other 10 hits are about R and the module file as a file. The intro of "Make the study link" now says the optional parts sit in five closed sections, and the link shows under "Your study link". The form list names all five forms. Both articles build with `pkgdown::build_article()`.
- 2026-09-30: T4 AC4 search, `rg -n -i -P` per D-083(b) pattern over `pid5_scoring.Rmd` and `overview.Rmd`. Before: `\bstores?\b` at `overview.Rmd:69` and `pid5_scoring.Rmd:198`, `:202`, and `(?<!study )\blink builder\b` at `pid5_scoring.Rmd:200`. All four were prose and are cleared. After: 0 hits for every pattern, so no exempt hit remains.
- 2026-09-30: T4 done. The strip says "online form" and "Study Link Builder", and its button reads "Open the Study Link Builder". `online-collection.Rmd:85` names the new button, which moves every T3 offset after @4300 by 12. A new test in `test-download-pages.R` checks the five strips: the button text, "online form" with no other page name, and only the `?c=` of the button's `href` among the retired terms. Its four assertions each failed against the old strip. The four changed pages and `download-pid5` build.
