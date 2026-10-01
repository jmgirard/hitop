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

- [ ] AC1: In hitop-form, a link with `setup` and `sha256` and neither `c` nor `z` runs the online form from a hosted setup file. `parseLink()` fetches the address in `setup` with `cache: 'no-store'`, `credentials: 'omit'` and `referrerPolicy: 'no-referrer'`. It reads at most 100,000 bytes, decodes them as UTF-8 with `fatal: true`, and parses them as JSON. The fingerprint is the SHA-256 of the UTF-8 bytes of `JSON.stringify(parsedValue)`, written base64url without padding (43 characters). When it equals `sha256`, the setup goes through the checks a `c` or `z` setup goes through. Playwright tests serve setup files through routes, with `sha256` computed in Node by `node:crypto`. One, holding consent text and questions, walks to a saved file. One whose fingerprint matches but whose instrument is unknown is refused with the message a `c` link of that setup gets.
- [ ] AC2: `parseLink()` refuses each of these faults by name, and one Playwright test per fault shows the error screen with the fault in "Details for the study team": `setup` beside `c`; `setup` beside `z`; `setup` without `sha256`; `sha256` without `setup`; a `sha256` that is not 43 base64url characters; a `setup` that is not an absolute `https:` address; a `setup` holding a user name or password; a fetch that throws (kind `connection`); a status outside 200 to 299; a body over 100,000 bytes; bytes that are not UTF-8; text that is not JSON; JSON that is not an object; a fingerprint that differs from `sha256`; and a browser without `crypto.subtle` (kind `browser`). `checkParticipantParam()` refuses `setup` and `sha256` as it refuses `c` and `z`.
- [ ] AC3: On a hosted-file walk, the online form requests only its own files, the exports and the setup file, and it requests the setup file once. `tests/network.spec.js` shows this with a count per address.
- [ ] AC4: link.html has a "Where the setup is kept" fieldset in the form, before "Make the link", offering "In the study link" (the default) and "In a file I host". The second shows "Download the setup file" and an "Address of the setup file" field. The download runs the checks "Make the link" runs on the form, then saves `setup.json` holding `JSON.stringify(config, null, 2)` and a final newline. When those bytes exceed 100,000, it refuses instead and names the size. With the second choice, "Make the link" refuses an empty address, one that is not absolute `https:`, and one holding a user name or password. It then fetches the address and refuses, naming the address, each fault AC2 lists for a fetched file. For a fetch that throws, the message says the host must let other sites read the file. It also refuses a file whose fingerprint differs from the fingerprint of the form's current setup: "The file at <address> does not match this setup. Download the setup file again and replace the hosted copy." A field changed during the fetch gives the builder's existing interrupted-build message. When the fingerprints are equal, the link is the online form's address with `setup` and `sha256` written by `URLSearchParams`, plus any recruiting-site suffix. Playwright tests cover each refusal, the equal case, and an address holding `&`.
- [ ] AC5: Opening link.html with `setup` and `sha256` refuses AC2's link-shape faults in the builder's refusal pattern, then fetches the file. When the fingerprints are equal, it fills the form as a `z` link fills it, with "In a file I host" chosen and the address filled and listed in the notice of filled addresses. On a mismatch, it says the file changed since the link was made and offers "Fill in the form from the current file". Pressing it fills the form the same way, and a new link must then be made. A fetch fault is refused in the builder's refusal pattern. Playwright tests cover the fill, each link-shape fault, the mismatch offer and its button, a thrown fetch, a status fault and a size fault.
- [ ] AC6: hitop-form's README "Your study link" gains a section on hosted setup files. It gives the `setup` and `sha256` parameters, the fingerprint rule and a GitHub raw-file address as an example. It says the repository must be public, the file must be at most 100,000 bytes as saved, a changed file needs a new link, and the host must let other sites read the file. It also says GitHub can serve the old copy of an edited file for the time its cache header states, and a source note records that header. "What the page's host sees" says the setup file's host sees each participant's request for it. hitop's `vignettes/articles/online-collection.Rmd` gains a short section on the same choice. NEWS.md names it and says `setup` and `sha256` can no longer be a `participantParam`. D-083(b)'s retired-term patterns find nothing in the new README and article text, in the new `form.js` messages, or in link.html's text in the "In a file I host" state, the mismatch offer and each new refusal.
- [ ] AC7: hitop-form's Playwright suite passes. In hitop, `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::build_article("articles/online-collection")` renders without error.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T3
- AC4 → T4, T6
- AC5 → T5, T6
- AC6 → T7, T8
- AC7 → T2, T3, T6, T8

## Tasks

- [x] T1: In `form.js`, add `setupFingerprint()` (`crypto.subtle.digest`) and `fetchSetup()`, the `setup`/`sha256` branch of `decodeLink()` with AC2's refusals, and `setup` and `sha256` in `checkParticipantParam()`. Read a GitHub raw file's `Access-Control-Allow-Origin` and `Cache-Control` headers with `curl -I` and write a hitop source note on them.
- [x] T2: Add `tests/setupfile.spec.js` with a Node fingerprint helper in `tests/helpers.mjs`: AC1's walk and refusal, and one test per AC2 fault.
- [x] T3: Add the hosted-file walk to `tests/network.spec.js`, counting requests per address.
- [ ] T4: In `link.html`, add the fieldset, the download (`saveFile()` gains a type argument), the address checks, the fetch check and the link written by `URLSearchParams`, with the interrupted-build message across the fetch.
- [ ] T5: In `link.html`, fill from a `setup` link, with the mismatch offer and its button.
- [ ] T6: Builder tests for AC4 and AC5 in a new `tests/link-setupfile.spec.js`. Extend S8's retired-term walk in `link-sections.spec.js` to the new states.
- [ ] T7: README section and "What the page's host sees". Scan the new README text and `form.js` messages with D-083(b)'s patterns.
- [ ] T8: hitop article section and NEWS line. Run `pkgdown::build_article("articles/online-collection")` and `devtools::check()`. Open the companion PR with the hitop PR.

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

## Decisions

## Review
