# M135: A study link can carry the researcher's consent text, which hitop-form shows before the form with an agree and a decline choice

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, GP3
- **Resolves:** —
- **Surface tier:** user-facing — the deployed form page, its link builder and the package's article
- **Branch/PR:** `m135-form-consent-screen`; companion: /Users/jmgirard/github/hitop-form m135-form-consent-screen

## Goal

A study link's `consent` field holds the researcher's consent text. The page shows it on a screen of its own before the start screen. "I agree" continues to the form, and "I do not agree" ends the session with nothing sent or saved.

## Scope

**In:** In hitop-form, the work covers these parts. The `consent` link field (`text`, optional `declined`) in `parseLink()`, with its refusals. The consent screen and the declined screen. The `completeDeclined` link field, an address that the declined screen sends the participant to, such as the Prolific no-consent code. The compressed link parameter `z`, which the builder writes for a link that carries `consent` and which the page reads beside `c`. `participantParam` refusing `z`. Two multi-line boxes and an address field in link.html, with their prefill. A README section and Playwright tests. In hitop, the work covers the consent section of the online-collection article and NEWS. D-079 governs. This plan records it.

**Out:** The researcher's own questions and their columns go to M136 (reader) and M137 (page). Markup, links or formatting inside the consent text go to a candidate row. Instrument content (IP1) is untouched. No row or file column changes, so `read_form_responses()` is unchanged.

## Acceptance criteria

- [ ] AC1: Under a link whose `consent.text` is set, the page shows a consent screen before the start screen. The page first turns each CR LF and each lone CR into LF. It then splits the text into paragraphs at each run of blank lines, where a blank line is empty or holds only white space. It keeps a single line break inside a paragraph as a line break. It writes each paragraph as text, so no tag or entity in it is read as markup. Below the text are an "I agree" and an "I do not agree" button. "I agree" shows the start screen as a link without `consent` does. A link without `consent` shows no consent screen. Tests walk a text that holds two paragraphs, a single line break, a line of spaces, `<b>x</b>` and `&amp;`. They use a link from the builder and a hand-made link with CR LF. They assert the `innerText` of each paragraph, in which the single line break is `\n`, and that no `b` element exists.
- [ ] AC2: "I do not agree" shows a screen with the link's `consent.declined` text, split and written as AC1 states. When `declined` is absent, the screen shows the fixed sentence "You chose not to take part. You can close this page." After that press, the page sends no request to the link's store and starts no download. The screen has no control that returns to the form and no link other than to `completeDeclined`. Without `completeDeclined`, the page keeps its address. Tests assert both texts, with `<b>x</b>` and `&amp;` in the declined text. Tests run with no store, under a webhook store and under a Supabase store, each with `complete` set. For 2 seconds after the press, they assert three facts. The recording server logs no request, no download event fires, and the address does not change.
- [ ] AC3: A link can carry `completeDeclined`, an address checked as `complete` is and accepted only beside `consent`. Each `{participant}` in it is filled as in `complete`. If the page holds no identifier at the press, each `{participant}` becomes the empty string. After "I do not agree", the page draws the declined screen with a link to that address, then goes to it. Without `declined` text, the fixed sentence ends at "You chose not to take part." and the link follows. The store and download clauses of AC2 still hold. Tests assert the exact address reached for a Prolific-shaped address with a `cc` code. They also assert it for a SONA-shaped address with the `{participant}` token, with an identifier from the address and without one. Tests repeat the store and download assertions of AC2 under `completeDeclined`. Tests also assert the refusal of `completeDeclined` without `consent` and of an `http:` address, each naming `completeDeclined`.
- [ ] AC4: `parseLink()` refuses each of these faults and names the `consent` field and the fault. The faults are a `consent` that is not a plain object, and a key other than `text` and `declined`. They include a `text` that is absent, is not a string, is blank after trimming white space, or has more than 20,000 characters. They include a `declined` that is not a string, is blank after trimming, or has more than 2,000 characters. The last fault is a `text` or `declined` that holds a lone surrogate. Here a character is one UTF-16 code unit, as the string length in JavaScript counts it. Before it builds a link, link.html refuses the faults its controls can produce. These are a Consent box or a Declined box that holds only white space. They include a Declined box or a decline address filled while the Consent box is empty. They also include the two length limits and a lone surrogate. An empty Consent box with an empty Declined box builds a link without `consent`. Tests assert the message of each refusal on each side where it can occur.
- [ ] AC5: For a link that carries `consent`, link.html writes the configuration as `?z=`. The value is the UTF-8 JSON, compressed with the browser's `CompressionStream("deflate-raw")` and written as base64url with no padding. For a link without `consent`, link.html writes `?c=` as on main. The page reads either parameter. It refuses by name a link with both parameters. It also refuses a `z` that is not base64url or does not inflate. It refuses a `z` that inflates to more than 100,000 bytes, or to text that is not UTF-8, not JSON, or not a JSON object. It reads UTF-8 through `TextDecoder` with `fatal: true`. In a browser without `DecompressionStream`, a `z` link shows a refusal that names the browser as the cause. `checkParticipantParam()` refuses `z` as it refuses `c`. One test makes a link in the builder with consent text that holds CR LF, blank lines, a tab and non-ASCII text. The page reads the same text. Further tests cover a prefill round trip through `?z=` and each refusal. They include a truncated stream, bytes after the end of the stream, and the page with `DecompressionStream` deleted.
- [ ] AC6: Take a link with `consent` and the same link without it. The row, the saved file and the Supabase SQL from the builder have the same columns in the same order under both links. This holds with shuffle off and with shuffle on. Tests compare each pair.
- [ ] AC7: The README section and the consent section of the article each state six facts. The text shows before the form. A decline sends no answer and saves no file. `completeDeclined` sends a decliner to the address a recruiting site gives for that outcome. The page reads the text as plain text. The consent text belongs to the researcher and their review board. The text limit is 20,000 characters. NEWS names `consent` and `completeDeclined`, and says that `participantParam` now refuses `z`. The hitop-form suite passes in its CI. In hitop, `devtools::check()` gives 0 errors, 0 warnings and 0 notes, and `cairn_validate` exits 0.

## Coverage

- AC1 → T2
- AC2 → T2
- AC3 → T1, T2, T3
- AC4 → T1, T3
- AC5 → T1, T3
- AC6 → T4
- AC7 → T5, T6

## Tasks

- [ ] T1: In form.js, add `checkConsent()` to `parseLink()` with the refusals of AC4, and `completeDeclined` through `checkCompleteUrl()`. Add `decodeLink()`, which reads `c` or `z` with the refusals of AC5. It reads `z` through `DecompressionStream("deflate-raw")` and stops at 100,000 bytes, so `boot()` awaits it. Add `encodeLink()` for the builder. Add `z` to the names that `checkParticipantParam()` refuses.
- [ ] T2: In `runForm()`, add the consent screen and the declined screen before `start()`. Build both with the `el()` helper, so every string goes through `textContent`. Move focus to the screen heading, as the start screen does. Draw the declined screen before the page goes to `completeDeclined`, as D-072(b) orders the sent screen. Write the walk and network tests of AC1 to AC3.
- [ ] T3: In link.html, add a "Consent text" and a "Declined text" `<textarea>` and a decline address field. A one-line input strips line breaks (the M128 lesson). Add the submit checks, the `z` output, the link length as shown today, and `prefill()` from `?z=`. Write the builder tests.
- [ ] T4: Write the row, file and SQL comparisons with and without `consent`, with shuffle off and on.
- [ ] T5: Write the README section "Show consent text before the form", the consent section of the article, and the NEWS entry.
- [ ] T6: Get hitop-form CI green on its PR. Run hitop `check()` and `cairn_validate`.

## Work log

- 2026-09-28: created by /milestone-plan. Absorbs the "consent text before the instrument" part of the online-form candidate row, which is narrowed in the same commit.
- 2026-09-28: criteria audit ran in full mode (user-facing tier) on two fresh [O] readers, the second over the criteria the gate changed. The first returned 8 findings and the second 7, all fixed before the commit. The fixes cover line-break rules, `innerText` probes, a no-store decline probe, no completion link on the declined screen, the `z` refusals, the inflate cap, old browsers, `participantParam` and `z`, an empty identifier in `completeDeclined`, and the NEWS wording.
- 2026-09-28: plan gate chose a compressed `z` link over a plain `c` link with a length cap and over a hosted setup file, because consent text needs room and a hosted file can change under a running study. Falsified by a recruiting site or mail system that breaks `z` links.
- 2026-09-28: plan gate chose agree or decline with a `completeDeclined` address over recording declines and over required tick boxes, because Prolific and other sites need a decline code and a decline record stores data about people who refused. Falsified by a review board that requires a record of declines.

## Decisions

## Review
