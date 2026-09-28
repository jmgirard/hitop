# cloudresearch2026help — CloudResearch Connect's participant ID parameter and completion paths, as its researcher help center states them

**Provenance.** Ingested 2026-09-27 at M133 from two public web pages read by Claude in the browser pane that day (a plain fetch was refused with HTTP 403), with no PDF on the shelf: `https://connect-researcher-help.cloudresearch.com/hc/en-us/articles/21181529476500-How-to-Integrate-your-Survey-with-Connect` and `https://connect-researcher-help.cloudresearch.com/hc/en-us/articles/4416245469332-Project-Link`.
Pagination: —.
Extraction: read directly from the two pages on 2026-09-27, quoted below as read, and not re-read since — observed 2026-09-27.

**Citation.** CloudResearch (2026). *How to Integrate your Survey with Connect* and *Project Link*, Connect Researcher Knowledgebase. Web pages, the first marked "1 year ago Updated" and the second "4 years ago Updated" on 2026-09-27, both by "CloudResearch Account".

**Role.** This page settles what CloudResearch Connect passes to an external page and how that page returns the participant. The builder's CloudResearch Connect choice with the name `participantId` (D-077, M133) and the Connect paragraphs of the hitop-form README and the online-collection article depend on it.

## Extracted values

- The parameter name — "Enter participantId into the field (case sensitive –notice the “I” is capitalized)." How to Integrate, Qualtrics steps; for Alchemer, "In the 'Populate with the following field', replace the xxx with 'participantId'." after choosing "URL Variable".
- Why the ID is recorded — "Connect IDs are needed in order to match up responses and approve/reject/bonus participants." How to Integrate.
- Capture after launch — "If you have forgotten to collect Connect IDs before launching the study, you can get them retroactively by adding the \"participantId\" capture to your survey platform (if supported)." How to Integrate.
- The two further IDs — "The assignmentId and projectId collection is not required. AssignmentId is a unique ID we give each participant as soon as they enter the survey. It is not the same as their Connect ID. Project ID is the ID that your project is under on Connect." How to Integrate.
- The fallback — "If your survey platform does not have the ability to automatically collect Connect IDs through something like embedded data for the variable “participantId”, then you will need to ask an in-survey question that allows participants to input their ID within your survey." How to Integrate.
- The completion paths — "you need to make sure you are collecting Connect IDs and following one of our completion methods: fixed completion code or completion redirect." and "Select the Redirect to a URL option and paste the completion redirect URL we give you on Connect when setting up the study." How to Integrate.
- The project URL — "The project URL is where we will send participants who accept your project. The URL can be any external survey platform or website." Project Link.

## Traces to

- `cairn/DECISIONS.md` D-077 and D-078 — the parameter name and the completion redirect URL Connect gives the study (corrected at the M133 review: this line said "one completion address per study", which the open question below leaves unsettled).
- `cairn/milestones/M133-form-recruiter-param.md` AC5 and AC6 — the builder's Connect choice and the cited statements.
- Once M133 ships: `vignettes/articles/online-collection.Rmd`, the SONA and Connect section, and the hitop-form README's Connect section.

## Open questions

- Whether Connect adds `participantId` to the project URL itself. Neither page says so in those words. How to Integrate tells the researcher to capture `participantId` as a URL variable, and says the capture can be added after launch, which only works if the address already carries it (corrected at the M133 review: an earlier draft also said both pages forbid a placeholder in the project URL, which no quote above supports). The page asks for the identifier on its start screen when the address carries none — observed 2026-09-27.
- Whether the completion redirect address differs per participant. The pages describe one address "we give you on Connect when setting up the study" — observed 2026-09-27.
