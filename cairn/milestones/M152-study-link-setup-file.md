# M152: Hosted setup files for study links

- **Status:** in-progress
- **Priority:** high
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** user-facing — a new study-link form the online form and the Study Link Builder read
- **Branch/PR:** m152-study-link-setup-file; companion: /Users/jmgirard/github/hitop-form m152-study-link-setup-file

## Goal

A researcher can keep a study's setup in a JSON file they host, and a short study link that names the file's address and fingerprint opens the online form with that setup, while a link that carries the whole setup stays the default.

## Scope

**In:** hitop-form (`/Users/jmgirard/github/hitop-form`) companion work. In `form.js`, a link with `setup` (the file's address) and `sha256` (its fingerprint) fetches, checks and runs the setup. In `link.html`, a "Where the setup is kept" choice, a setup-file download, an address field, a fetch check before the link is made, and the fill from a `setup` link, with an offer to load an edited file. README sections, the hitop article section, NEWS, and D-086 (written with this plan).

**Out:**
- The question-count limit and the link-length warning go to M153.
- "HiTOP-SR module" as an instrument choice goes to M154.
- Private hosts, sign-in, and a setup file that changes without a new link stay out (D-086). A request to relax that is a new plan.
- An R writer for setup files goes to the hitop-form researcher-content candidate row.

## Acceptance criteria

- [x] AC1: In hitop-form, a link with `setup` and `sha256` and neither `c` nor `z` runs the online form from a hosted setup file. `parseLink()` fetches the address in `setup` with `cache: 'no-store'`, `credentials: 'omit'` and `referrerPolicy: 'no-referrer'`. It reads at most 100,000 bytes, decodes them as UTF-8 with `fatal: true`, and parses them as JSON. The fingerprint is the SHA-256 of the UTF-8 bytes of `JSON.stringify(parsedValue)`, written base64url without padding (43 characters). When it equals `sha256`, the setup goes through the checks a `c` or `z` setup goes through. Playwright tests serve setup files through routes, with `sha256` computed in Node by `node:crypto`. One, holding consent text and questions, walks to a saved file. One whose fingerprint matches but whose instrument is unknown is refused with the message a `c` link of that setup gets.
- [x] AC2: `parseLink()` refuses each of these faults by name, and one Playwright test per fault shows the error screen with the fault in "Details for the study team": `setup` beside `c`; `setup` beside `z`; `setup` without `sha256`; `sha256` without `setup`; a `sha256` that is not 43 base64url characters; a `setup` that is not an absolute `https:` address; a `setup` holding a user name or password; a fetch that throws (kind `connection`); a status outside 200 to 299; a body over 100,000 bytes; bytes that are not UTF-8; text that is not JSON; JSON that is not an object; a fingerprint that differs from `sha256`; and a browser without `crypto.subtle` (kind `browser`). `checkParticipantParam()` refuses `setup` and `sha256` as it refuses `c` and `z`.
- [x] AC3: On a hosted-file walk, the online form requests only its own files, the exports and the setup file, and it requests the setup file once. `tests/network.spec.js` shows this with a count per address.
- [x] AC4: link.html has a "Where the setup is kept" fieldset in the form, before "Make the link", offering "In the study link" (the default) and "In a file I host". The second shows "Download the setup file" and an "Address of the setup file" field. The download runs the checks "Make the link" runs on the form, then saves `setup.json` holding `JSON.stringify(config, null, 2)` and a final newline. When those bytes exceed 100,000, it refuses instead and names the size. With the second choice, "Make the link" refuses an empty address, one that is not absolute `https:`, and one holding a user name or password. It then fetches the address and refuses, naming the address, each fault AC2 lists for a fetched file. For a fetch that throws, the message says the host must let other sites read the file. It also refuses a file whose fingerprint differs from the fingerprint of the form's current setup: "The file at <address> does not match this setup. Download the setup file again and replace the hosted copy." A field changed during the fetch gives the builder's existing interrupted-build message. When the fingerprints are equal, the link is the online form's address with `setup` and `sha256` written by `URLSearchParams`, plus any recruiting-site suffix. Playwright tests cover each refusal, the equal case, and an address holding `&`.
- [x] AC5: Opening link.html with `setup` and `sha256` refuses AC2's link-shape faults in the builder's refusal pattern, then fetches the file. When the fingerprints are equal, it fills the form as a `z` link fills it, with "In a file I host" chosen and the address filled and listed in the notice of filled addresses. On a mismatch, it says the file changed since the link was made and offers "Fill in the form from the current file". Pressing it fills the form the same way, and a new link must then be made. A fetch fault is refused in the builder's refusal pattern. Playwright tests cover the fill, each link-shape fault, the mismatch offer and its button, a thrown fetch, a status fault and a size fault.
- [x] AC6: hitop-form's README "Your study link" gains a section on hosted setup files. It gives the `setup` and `sha256` parameters, the fingerprint rule and a GitHub raw-file address as an example. It says the repository must be public, the file must be at most 100,000 bytes as saved, a changed file needs a new link, and the host must let other sites read the file. It also says GitHub can serve the old copy of an edited file for the time its cache header states, and a source note records that header. "What the page's host sees" says the setup file's host sees each participant's request for it. hitop's `vignettes/articles/online-collection.Rmd` gains a short section on the same choice. NEWS.md names it and says `setup` and `sha256` can no longer be a `participantParam`. D-083(b)'s retired-term patterns find nothing in the new README and article text, in the new `form.js` messages, or in link.html's text in the "In a file I host" state, the mismatch offer and each new refusal.
- [x] AC7: hitop-form's Playwright suite passes. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4, T6
- AC5 → T5, T6
- AC6 → T7, T8, T9
- AC7 → T2, T3, T6, T8, T10, T11

## Tasks

- [x] T1: In `form.js`, add `setupFingerprint()` (`crypto.subtle.digest`) and `fetchSetup()`, the `setup`/`sha256` branch of `decodeLink()` with AC2's refusals, and `setup` and `sha256` in `checkParticipantParam()`. Read a GitHub raw file's `Access-Control-Allow-Origin` and `Cache-Control` headers with `curl -I` and write a hitop source note on them.
- [x] T2: Add `tests/setupfile.spec.js` with a Node fingerprint helper in `tests/helpers.mjs`: AC1's walk and refusal, and one test per AC2 fault.
- [x] T3: Add the hosted-file walk to `tests/network.spec.js`, counting requests per address.
- [x] T4: In `link.html`, add the fieldset, the download (`saveFile()` gains a type argument), the address checks, the fetch check and the link written by `URLSearchParams`, with the interrupted-build message across the fetch.
- [x] T5: In `link.html`, fill from a `setup` link, with the mismatch offer and its button.
- [x] T6: Builder tests for AC4 and AC5 in a new `tests/link-setupfile.spec.js`. Extend S8's retired-term walk in `link-sections.spec.js` to the new states, and S12 to the new refusals.
- [x] T7: README section and "What the page's host sees". Scan the new README text and `form.js` messages with D-083(b)'s patterns.
- [x] T8: hitop article section and NEWS line. Run `pkgdown::build_article("articles/online-collection")` and `devtools::check()`. Open the companion PR with the hitop PR.
- [x] T9: Review pass 1 text fixes (findings 1, 6, 7, 9, 10, 14, 18, 19). Write "Ingested" in the source note's provenance. Warn that a public setup file shows where responses go. Correct the white-space claim. Say to wait out the cache before sending a new link. Name the builder's fetch of an opened link. Add the read-by-other-sites hint to the prefill refusal. Update the article's "Who holds the data" and the `form.js` header. Tidy NEWS and "the page".
- [x] T10: Review finding 2. The setup fetch stops after a time limit, in the online form and the builder, with a refusal and tests.
- [ ] T11: Review findings 3 and 4. "Make the link" hides the changed-file offer. A file too deeply nested to fingerprint gets a named refusal. Tests for both.
- [ ] T12: A candidate row "Hosted setup file gaps" for findings 5, 8, 11 to 13 and 15 to 17. Finding 20 is rejected as intended (D-086(c)).

## Work log

- 2026-10-01: created by /milestone-plan, with M153 and M154. Jeff asked for hosted setup files for advanced use, with the link-carried setup kept as the default.
- 2026-10-01: criteria audit (full mode, fresh [O] reader) returned 22 findings across M152 to M154; 20 clear fixes applied and 2 went to the gate. Jeff chose to offer loading an edited file, and to put M152 to M154 before M144 and M145 at high priority. A second full audit on the post-gate wording returned 17 findings, applied (D-086 written, one test per fault, the match defined, the property form of AC3, the retired-term walk widened, the cache claim sourced).
- 2026-10-01: plan chose a fingerprint pinned in the link over a link that follows the file's latest version, because a silent edit would change the questions partway through a study and mix two setups in one data set. Falsified by researchers needing mid-study edits often enough that a new link each time is a burden.
- 2026-10-01: plan chose the fingerprint of `JSON.stringify` of the parsed file over the file's raw bytes, so a whitespace or line-ending change from an editor or git does not break the link. Falsified by a browser whose `JSON.stringify` output differs from Node's for the same value.
- 2026-10-01: plan chose `setup` and `sha256` as parameter names over `f` and `h`, because a one-letter name is likelier to collide with a recruiting site's own parameter. Falsified by a recruiting site that appends a `setup` parameter.
- 2026-10-01: plan chose a pretty-printed `setup.json`, readable when edited on GitHub, over a compact file, with the 100,000-byte limit applied to the file as saved. Falsified by a researcher whose setup fits a `z` link but not the file.
- 2026-10-01: implement started on `m152-study-link-setup-file` in hitop and hitop-form. Gate: "Download the setup file" runs the form checks only and fetches no instrument files (Jeff).
- 2026-10-01: T1 done. `form.js` gains `fetchSetup()`, `setupFingerprint()`, `checkSetupAddress()`, `canFingerprint()` and the `setup`/`sha256` branch of `decodeLink()`. `checkParticipantParam()` refuses both names, and `saveFile()` takes a type. Source note `references/github2026rawheaders.md`: raw files send `access-control-allow-origin: *` and `cache-control: max-age=300`.
- 2026-10-01: T2 done. `tests/setupfile.spec.js` holds 23 tests (SF1 to SF4), with `setupFingerprint()`, `serveSetup()` and `setupQuery()` in `tests/helpers.mjs`. Four plants went red: a pretty-printed fingerprint, a size check at 100,000, the referrer option dropped, and the browser check dropped. Full hitop-form suite: 902 passed.
- 2026-10-01: T3 done. N10 in `tests/network.spec.js` counts requests per address on a setup-file walk. A planted second fetch of the file turned it red (count 2).
- 2026-10-01: checkpoint, T4 and T5 code written in `link.html`, not yet ticked. `build()` now reads the form through `readForm()`, which the download shares. Three `link-sections.spec.js` tests stay red until T6 (S12 must fire the new refusals) and T7 (the README heading the new hint links to). Added to T6: extend S12 to the new refusals.
- 2026-10-01: T4, T5 and T6 done. `tests/link-setupfile.spec.js` holds 29 tests (LF1 to LF8). S8 now reads the "In a file I host" state and the offer, and S12 fires 8 new refusals. `RETIRED` and `retiredIn()` moved to `tests/helpers.mjs`. Two plants went red: the match check dropped, and the address written unencoded. The offer's text is written at the moment the offer shows, which keeps the intro at 60 words. The fingerprint keeps key order. If a hand-written file has its keys in another order, it does not match the builder's form.
- 2026-10-01: T7 done. README gains "Keep the setup in a file you host" and a paragraph each in "Edit a study link" and "What the page's host sees". It also names `setup` and `sha256` as refused parameter names and gains test-table rows. The retired-term scan read 80 added README lines and 50 new `form.js` strings. Its two hits are the `'no-store'` fetch option, a code literal D-083(c) exempts. Full hitop-form suite: 940 passed.
- 2026-10-01: T8 done. `online-collection.Rmd` gains "Keep the setup in a file you host", and NEWS gains one entry. The retired-term scan of the article's 22 added lines found nothing. `pkgdown::build_article("articles/online-collection")` failed against the installed hitop 0.2.0 in the "Several instruments" chunk, which the installed copy predates. Against the branch installed to a scratch library, it rendered without error. `devtools::check()`: 0 errors, 0 warnings, 0 notes. The two PRs open at `/milestone-review`'s merge step, after approval.
- 2026-10-01: claim audit: 150 claims read, 6 corrected — hitop-form tests/link-setupfile.spec.js (two LF6 cases added), README.md, link.html, and hitop NEWS.md, vignettes/articles/online-collection.Rmd
- 2026-10-01: the audit's re-read found one correction still overstated (the download's checks). The README now says a missing module item shows at "Make the link" only under a Supabase table. Builder specs: 139 passed. The article renders against the branch package.
- 2026-10-01: implement complete. Status set to review.
- 2026-10-01: review returned, defect return 1. The consistency gate failed: `cairn_validate.py` FAIL `references index<->disk`, because `cairn/references/github2026rawheaders.md` names no ingested date. AC1 to AC7 passed on fresh evidence. 20 reviewer findings are in the Review section, untriaged. Status set to in-progress.
- 2026-10-01: implement resumed. Gate (Jeff): fix the text findings, a setup-fetch time limit, and the builder state findings now, as T9 to T11. The rest goes to a new candidate row (T12). Tasks added, Coverage extended for AC6 and AC7.
- 2026-10-01: T9 done. The source note says "Ingested", and `cairn_validate` has no FAIL. README, article, NEWS and the `form.js` header gain the public-file warning, the white-space correction, the wait before sending a new link and the builder's own request. The builder's refusal of an opened link whose fetch throws gains the read-by-other-sites hint, and LF8 asserts it. Retired-term scan of the added text: 0 hits. Builder specs: 139 passed.
- 2026-10-01: T10 done. `fetchSetup()` aborts after `SETUP_TIMEOUT_MS` (30,000, the send limit), with fault `timeout`. The online form shows it as kind `connection`. Three tests advance the page's clock: one in the online form, and two in the builder, for an opened link and for "Make the link". A planted 60-second limit turned all three red at the refusal. README lists the fault. Setup-file, network and builder specs: 175 passed.

## Decisions

## Review

Pass 1, 2026-10-01. Both repos were in step with `origin/main`, so no merge was needed. Full hitop-form Playwright suite: 942 passed. hitop `devtools::check()`: 0 errors, 0 warnings, 0 notes.

- AC1: `tests/setupfile.spec.js` SF1 walks a pretty-printed CRLF setup file holding consent text and a question to a saved CSV. It asserts the fetch options `no-store`, `omit` and `no-referrer`, and one GET. SF2 refuses an unknown instrument with the `c` link's message. The fingerprint comes from Node's `createHash` in `tests/helpers.mjs`. `fetchSetup()` stops reading past 100,000 streamed bytes, decodes with `fatal: true` and parses JSON. Passed in the 942.
- AC2: `tests/setupfile.spec.js` SF3 holds one test per fault, 18 in all, each reading the fault in "Details for the study team". The 9 link faults include two bad `sha256` shapes and both a relative and an `http:` address. The 7 file faults and the mismatch follow, then the missing `crypto.subtle`. The thrown fetch shows the `connection` sentence and the missing `crypto.subtle` shows the `browser` sentence. SF4 refuses `participantParam` `setup` and `sha256`. Passed in the 942.
- AC3: `tests/network.spec.js` N10 walks a setup-file link through save and counts requests per address. It asserts exactly the page, `form.js`, the HiTOP-BR export and the setup file, once each. Passed in the 942.
- AC4: `tests/link-setupfile.spec.js` LF1 to LF4 cover the fieldset and its place, the download and its 100,000-byte bound, the form refusal and each address refusal. They also cover each fetched-file fault, the mismatch message and the equal case. The `&` address and Prolific's ending are covered too. `link-sections.spec.js` S12 covers the interrupted-build message across the setup fetch. Passed in the 942.
- AC5: `tests/link-setupfile.spec.js` LF5 fills the form from a matching file with "In a file I host" chosen and the address listed. LF6 refuses each link-shape fault with nothing fetched. LF7 covers the mismatch offer and its button, with the next link carrying the new fingerprint. LF8 covers a thrown fetch, a 404 and an over-size file. Passed in the 942.
- AC6: Read hitop-form `README.md` lines 201 to 261. They give both parameters, the fingerprint rule and a raw-file example. They also state the public repository, the 100,000 bytes as saved, a new link per change and `Access-Control-Allow-Origin`. The `max-age=300` cache note cites `cairn/references/github2026rawheaders.md`. "What the page's host sees" gains the setup host's paragraph. The article section and the NEWS entry, with its `participantParam` sentence, are present. A scan of added lines used `tests/helpers.mjs` `RETIRED` (12 patterns), and a planted line went red. The README gave 1 hit in 83 lines and `form.js` gave 1 in 114 non-comment lines, both the code literal `'no-store'` that D-083(c) exempts. The article and NEWS gave 0 hits in 36 lines. S8 and every LF refusal check link.html's new text with `retiredIn()`, and they passed in the 942.
- AC7: hitop-form Playwright: 942 passed. hitop `devtools::check()`: 0 errors, 0 warnings, 0 notes. `pkgdown::build_article("articles/online-collection")` rendered without error against the branch installed to a scratch library through `R_LIBS`. The output holds the new section.

Consistency gate: `devtools::document()` left no diff, and `pkgdown::check_pkgdown()` found no problems. NEWS has the entry, and the branch adds no top-level file. `cairn_validate.py` exited 1 with one FAIL, `references index<->disk`. `cairn/references/github2026rawheaders.md` names no ingested date: its provenance says "made 2026-10-01" where the check reads "Ingested". Review stopped at this gate (step 4), and status went back to `in-progress`.

Reviewer findings, untriaged. The three lenses ran in parallel with the gate, and the gate stopped the review before triage. The next review's merge gate triages them, and implement can fix any of them first. Merged across lenses, most severe first:

1. A public `setup.json` exposes where responses go, the webhook address or the Supabase address, key and table. Anyone can read it, and GitHub code search finds it. A `c` or `z` link reaches only participants. The README gives no warning (`README.md` near 239).
2. The setup fetch has no timeout (`form.js` near 301). The builder awaits the prefill before it attaches its submit and download handlers, so a host that never answers leaves the builder inert and silent. The online form stays on "Loading".
3. The prefill overwrites fields typed during its fetch. The mismatch offer stays on screen after the researcher makes a link, and its button then resets the form without asking.
4. A deeply nested file parses but makes `setupFingerprint()` throw `RangeError` outside any `try` (`form.js` near 381). The participant sees the engine's raw message, and the builder shows a generic or raw one.
5. "Fill in the form from the current file" fails for a hand-edited file. The form fixes key order, trims, drops `shuffle: false` and changes CRLF, so "Make the link" then refuses a mismatch. The README and the offer imply the path works.
6. "A change of spaces or line endings keeps the link working" is false for spaces or CRLF inside string values such as consent text. The README, the article and NEWS each say it.
7. The README says only the builder can meet GitHub's 5-minute cache. A participant behind another cache edge can get the old copy after the new link goes out, and that participant is refused.
8. A host that blocks other sites, a 503 and a captive portal give the participant "check your connection and reload" or the generic sentence. The `NEXT_STEP` comment still says `connection` means an export fetch.
9. Opening a `link.html?setup=` link now fetches a host named by whoever sent the link, which shows that host the researcher's IP. The builder's intro says it "keeps and sends nothing you type". The README covers only the participant's request.
10. The article's "Who holds the data" and the `form.js` header comment still describe only link-carried setups.
11. The new `resetForm()` lines (`setupFileFields`, the offer) and the `fillFromCurrent` catch have no test, and S11 does not cover them.
12. No test covers an opened link whose fetched setup `fill()` refuses, from either the prefill or the offer's button.
13. No test covers a streamed body without or with a wrong Content-Length, a `#` in the address, or a redirect. SF1's `referer` check can pass vacuously, but the fetch-options assertion covers the same property.
14. The builder prefill's connection refusal lacks the hint that the host must let other sites read the file.
15. Redirects are followed. A redirect to `http:` is blocked as mixed content and reported as a connection fault.
16. SF1 and N10 fetch the live export, which adds network-dependent specs of the kind the M151 candidate row lists as flaky.
17. The README's opening paragraph and S1 still say the closed sections are followed by "Make the link", with the new fieldset now between them.
18. NEWS lines are wrapped unevenly ("fills the form / from the file. A / link's"). The `participantParam` change sits under New features, and the entry calls the link "short".
19. The README's new host paragraph says "the page's address" for the online form, the wording the M150 candidate row flags.
20. A `participantParam` of `setup` or `sha256`, once valid, is now refused, and `setup` beside `c` gets the setup-file message. D-086(c) and NEWS state this, so it is intended.
