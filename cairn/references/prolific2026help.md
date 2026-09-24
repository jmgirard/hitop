# prolific2026help — Prolific's URL parameters, placeholders and completion URL, as its help center and API reference state them

**Provenance.** Ingested 2026-09-23 at the M117/M118 plan from four public web pages fetched by Claude that day, with no PDF on the shelf: `https://researcher-help.prolific.com/en/articles/445178-what-survey-experimental-software-is-compatible-with-prolific`, `https://researcher-help.prolific.com/en/articles/445127-data-collection`, `https://researcher-help.prolific.com/en/articles/445131-previewing-your-study`, and `https://docs.prolific.com/api-reference/studies/the-study-object`.
Pagination: —.
Extraction: read directly from the four pages on 2026-09-23, quoted below as fetched, and not re-read since — observed 2026-09-23.

**Citation.** Prolific (2026). *What survey / experimental software is compatible with Prolific?*, *Data collection*, and *Previewing your study*, Prolific Researcher Help Center; and *The study object*, Prolific API reference. Web pages, undated, read 2026-09-23. The help-center articles carry numeric ids in their URLs and no author or date line.

**Role.** This page settles what a Prolific study passes to an external page and how a participant is returned to Prolific. The hitop-form link fields `prolific` and `complete` (M118), the row and file columns `prolific_study` and `prolific_session` (D-071, M117, M118), and the online-collection article's Prolific section depend on it. The column names are this package's own and trace to nothing here.

## Extracted values

- The three URL parameters — "The default parameter for the participant ID is: `PROLIFIC_PID`", and "Prolific also provides two additional optional parameters: `STUDY_ID` – identifies your Prolific study. `SESSION_ID` – identifies an individual participant's submission." Article 445178, section "Recording Participants' Prolific IDs".
- The researcher must save them — "Make sure your survey or experimental software is configured to save these URL parameters in your dataset." Article 445178, same section.
- An example participant URL — "`https://example.com?PROLIFIC_PID=5a9d64f5f6dfdd0001eaa73d`". Article 445178, same section.
- The placeholder syntax — "You can add query parameters with the following placeholders": `{{%PROLIFIC_PID%}}` for the participant id, `{{%STUDY_ID%}}` for the study id, `{{%SESSION_ID%}}` for the session id, with the example `https://eggs-experriment.com?participant={{%PROLIFIC_PID%}}` (the misspelling is the page's). API reference, field `external_study_url`.
- The completion URL — "Simply use the Completion URL provided on your Prolific study setup page as the redirect URL. It will look similar to: `https://app.prolific.com/submissions/complete?cc=CHHXQERF`" and "The completion code (`CHHXQERF` in this example) is unique to your study." Article 445178, section "Returning Participants to Prolific". The API reference states the same shape as `https://app.prolific.com/submissions/complete?cc={code}` under `completion_codes`.
- The completion code — "A completion code is a required field consisting of a random string of letters and numbers that participants receive when they complete your study." API reference, field `completion_codes`.
- Redirecting is the recommended path — "If you choose to redirect participants using a URL, you'll need to set up your survey tool to redirect participants back to Prolific. This is our recommended option." Article 445127, section on completion paths.
- Why the ID is recorded — "If you do not record participant IDs, you will not know who has submitted which individual survey response." Article 445127, section "Recording Prolific IDs".
- The preview's value — "The PROLIFIC_PID parameter will be replaced with a 24-character ID in your preview Study URL." Article 445131.

## Traces to

- `cairn/DECISIONS.md` D-071 — the three parameter names, the placeholder mechanism and the completion URL shape.
- `cairn/milestones/M118-form-prolific.md` AC2, AC3 and AC5 — the parameter names, the placeholder suffix the builder prints, the placeholder value a page may receive, and the 24-character preview ID the article states.
- Once M118 ships: `vignettes/articles/online-collection.Rmd`, the Prolific section, and the hitop-form README's "Make a study link" section — the same facts, stated for the researcher.

## Open questions

- Whether Prolific's own "URL parameters" toggle appends its parameters with `&` to a study URL that already carries `?c=…`, or with a second `?`. None of the four pages says. The builder therefore prints the placeholders itself, so nothing depends on the answer — observed 2026-09-23.
- The character set and length of a live `PROLIFIC_PID`, `STUDY_ID` and `SESSION_ID`. The example ID is 24 hexadecimal characters and the preview article says 24 characters, and no page states a format. Nothing in the repo checks the values — observed 2026-09-23.
