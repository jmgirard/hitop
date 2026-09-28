# sona2026help — SONA's survey code, its client-side completion URL and that URL's security note, as SONA's researcher documentation states them

**Provenance.** Ingested 2026-09-27 at M133 from five public web pages read by Claude in the browser pane that day, with no PDF on the shelf: `https://www.sona-systems.com/researcher/using-the-survey-code-feature/`, `https://www.sona-systems.com/researcher/external-study-credit-granting/`, `https://www.sona-systems.com/researcher/security-considerations/`, `https://www.sona-systems.com/help/qualtrics/`, and `https://www.sona-systems.com/help/soscisurvey/`.
Pagination: —.
Extraction: read directly from the five pages on 2026-09-27, quoted below as read, and not re-read since — observed 2026-09-27.

**Citation.** Sona Systems (2026). *Using the SURVEY CODE Feature*, *External Study Credit Granting* and *Security Considerations*, Sona Systems Researcher Documentation; *Qualtrics Help Page* and *SoSci Survey Help Page*, Sona Systems help. Web pages, undated, read 2026-09-27. No page carries an author or date line.

**Role.** This page settles what a SONA study passes to an external page and how that page returns the participant for credit. The hitop-form `participantParam` field and the `{participant}` completion token (D-077, M133), the builder's SONA choice with its `id` name and `%SURVEY_CODE%` suffix, and the SONA paragraphs of the hitop-form README and the online-collection article depend on it.

## Extracted values

- The placeholder and its value — "if the text %SURVEY_CODE% is placed anywhere within the Study URL field, the system will automatically replace this text with a unique number for the participant." and "The number can be anywhere from 2-7 characters in length, and will not contain numbers leading with zeroes (1234 is possible, but 01234 is not)." *Using the SURVEY CODE Feature*.
- The example study URL — "If the Study URL is entered in the system as: https://www.myschool.edu/mysurvey.html?id=%SURVEY_CODE%" it becomes "https://www.myschool.edu/mysurvey.html?id=30039". *Using the SURVEY CODE Feature*.
- The researcher's own view — "If a participant is not viewing the URL (for example, the researcher is viewing the URL), this special survey code text will simply be removed." and the text "must be in all capital letters, and surrounded by percent signs." *Using the SURVEY CODE Feature*.
- `id` in the Qualtrics guide — "change the Study URL so the URL ends with ?id=%SURVEY_CODE%", with the note "(Note: “id” must be in lower-case)". *Qualtrics Help Page*, Step 1. The SoSci Survey page uses `?r=%SURVEY_CODE%` instead, so the name is the researcher's choice.
- The client-side completion URL — "The client-side completion URL will look like this: https://yourschool.sona-systems.com/webstudy_credit.aspx?experiment_id=123&credit_token=9185d436e5f94b1581b0918162f6d7e8&survey_code=XXXX" and "the XXXX at the end is to show where the survey code number should be placed (in place of XXXX) by the external study website." *External Study Credit Granting*.
- What the external page must do — "You will need to replace XXXX with the survey code number and pass that to the system in the completion URL." and "If this URL is loaded, the participant will receive credit. Typically, the participant clicking on this link in their browser, or the participant being redirected to this link after completing the study, would load this URL." *External Study Credit Granting*.
- The server-side URL — "This URL would typically be loaded by the external study (not clicked on by the end-user participant) and is a server-to-server communication" and "It also provides more control and security than the client-side method." *External Study Credit Granting*.
- The client-side risk — "Because the URL is typically accessed directly by the participant (their browser is redirected to it), they also have access to view the parameters in the URL. The completion URL contains a key specific to your study, as well as an ID (the survey code number)" then "The risk is that a participant could use this URL and start trying other ID (survey code numbers) to grant other participants credit." and "If this is of concern the best option is to use the Server-Side Completion URL". *Security Considerations*.
- Testing — "The entire credit granting process is not possible to test as a researcher, as researchers cannot sign up for studies." *External Study Credit Granting*.

## Traces to

- `cairn/DECISIONS.md` D-077 — the parameter, the per-participant completion code and the reason for the token.
- `cairn/milestones/M133-form-recruiter-param.md` AC2, AC4, AC5 and AC6 — the placeholder a page may receive, the SONA-shaped completion URL in the tests, the builder's `id` and suffix, and the cited statements.
- Once M133 ships: `vignettes/articles/online-collection.Rmd`, the SONA and Connect section, and the hitop-form README's SONA section.

## Open questions

- Whether SONA leaves `%SURVEY_CODE%` in the address in any view. The documentation says it is removed when the researcher views the URL, so the page reads a blank value there. The page also reads a `%…%` value as absent, so nothing depends on the answer — observed 2026-09-27.
