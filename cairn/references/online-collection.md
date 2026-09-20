# Online response collection as an alternative to Qualtrics and REDCap

**Provenance.** Ingested 2026-09-20 by M093 from a read of this package's own surfaces (`R/module_file.R`, `R/util.R`, `R/generate_qualtrics.R`, `R/generate_redcap.R`, `R/sysdata.rda`, `hitop_artifacts`, `hitophsum_items`) at commit `6d2be446`, a read-only read of the builder repository `jmgirard/hitop-builder` (`README.md`, `index.html`) at its 2026-09-20 checkout, and the vendor and regulator pages listed in the Sources table below.
Pagination: —.
Extraction: a 2026-09-20 snapshot, the vendor figures read directly from the pages the Sources table names on that date, and every price and limit moves independently of this note — observed 2026-09-20.

**Scope.** This page compares four ways to collect questionnaire responses online without Qualtrics or REDCap, and costs each under two operators. It is not a summary of any one source, and it builds nothing: it recommends one architecture and decides nothing. This is a reference, not an authority. Status lives in `ROADMAP.md`, decisions in `DECISIONS.md`, architecture in `DESIGN.md`. A synthesis note that starts asserting status is a second tracking system.

**Evidence snapshot.**

- The module descriptor format, version `1.0`, with its required fields `format`, `instrument` and `scales`, and its `itemOrder` slot — `R/module_file.R` at `6d2be446` — observed 2026-09-20.
- The item-column naming helper, which writes a lowercase stem, an underscore, and a number zero-padded to the widest item — `R/util.R:697` at `6d2be446` — observed 2026-09-20.
- The four instruction objects, each a start text and (for the three scored instruments) a four-option response table with integer values — `R/sysdata.rda` at `6d2be446` — observed 2026-09-20.
- The HSUM item table: 650 rows, 638 of them gated on another field's value, four field types (radio, text, checkbox, dropdown), nine choice sets — `hitophsum_items` and `hitophsum_choices` at `6d2be446` — observed 2026-09-20.
- The builder's no-backend posture and its webR stack — `jmgirard/hitop-builder` `README.md`, sections "What it does, and what it does not" and "How it works" — observed 2026-09-20.
- The artifact manifest, 39 rows over 24 files, build dates from 2026-07-16 to 2026-08-30 — `hitop_artifacts` at `6d2be446` — observed 2026-09-20.

## What the package offers today

The package generates blank forms for three delivery routes and scores the responses afterwards. The routes are a Word document for paper, a Qualtrics import file, and a REDCap data dictionary. The researcher fields the form through their own Qualtrics or REDCap account, exports the responses, and scores them in R. Qualtrics publishes no price and directs to a quote [S13]. REDCap is free to non-profit institutions that join its consortium and host their own instance [S14]. The package never touches a response in transit. The browser builder at `jmgirard.github.io/hitop-builder/` follows the same posture: it builds the blank form in the browser tab and transmits nothing.

## Requirements

An online form that replaces Qualtrics or REDCap must reproduce what those routes give the researcher today. Each requirement below names the package surface it derives from.

| # | Requirement | Package surface it derives from |
|---|---|---|
| R1 | A participant link the researcher can hand out, and a per-study configuration behind it: which instrument, which module, which item order, and a study label. | The builder's two-step flow chooses instrument scales and settings once and writes them into the bundle. The link must carry the same choices. |
| R2 | Response storage that survives the browser tab: the responses of every participant in a study, in one place the researcher can reach. | Qualtrics and REDCap hold responses server-side today. A device-only save (architecture A) changes this contract, and the comparison table says so. |
| R3 | An export in the package's item-column names: `hsr_001`..`hsr_405`, `hbr_01`..`hbr_45`, `pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100`, `pid5bf_01`..`pid5bf_25`. | `item_names()` at `R/util.R:697` builds every name (D-052, D-055). The `rename_*` helpers exist for data that arrives under other names, but an export the package designs must not need them. |
| R4 | Module support: a study can field a HiTOP-SR module, and its export scores through the module's descriptor. | `read_module()` rebuilds a module from a JSON descriptor whose `scales` field is required and whose `itemOrder` records a shuffled form's printed order (D-039). The form must be built from a descriptor, and the export must ship one. |
| R5 | Instrument coverage: PID-5 in three forms, HiTOP-SR, HiTOP-BR, and the HSUM. | The `*_items` tables and the four `*_instructions` objects hold every item, option and instruction. The HSUM adds display logic: 638 of its 650 fields show only when a gate field holds a gate value, across four field types and nine choice sets. Qualtrics's flat import cannot carry that logic (DESIGN, the Generators family line), so the HSUM's Qualtrics artifact is built through the API instead. |
| R6 | Content fidelity under IP1: item text, response options and instructions render exactly as the tables hold them, and a served form carries a build date and a version the reader can check. | IP1 makes item text and instructions sacrosanct. D-016 identifies every distributed artifact by build date and locks it by checksum in `hitop_artifacts`. A served form is a distributed artifact in the same sense. |
| R7 | A scoring hand-off: the researcher opens the export in R and scores it with no renaming and no retyping of scale names. | `score_hitopsr(data, items = seq_along(data), module = read_module(f), layout = "printed")` is the hand-off today for a module form. The online export must be that call's input. |
| R8 | No dependency the package's users must install for a route they do not use. | GP4: internals are base R, a family earns an Import, a per-function need uses Suggests. Anything server-side belongs outside the package. |

Two facts bound the design space. First, the package holds every item, option and instruction as data, so a form renderer needs only a JSON export of those tables. Second, the builder already runs R in the browser through webR and installs the package from r-universe at page load, about twenty seconds on a first visit. A collection form must load in the time a participant will wait, so it cannot inherit the builder's stack. That trade is worked through in the build breakdown below.

## The four architectures

- **A. Device-only.** A static page renders the form. At the end, the participant saves a response file to their own device and sends it to the researcher by a route the study arranges. Nothing is transmitted by the page.
- **B. Static page, researcher-owned storage.** The same static page posts each submission to storage the researcher already owns or opens for the study: a Supabase or Firebase project, or a Google Apps Script endpoint that appends to a sheet. The plan named the institution's REDCap as a fourth store, and the architecture-fit section below shows why a static page cannot reach it. The page holds no data and the operator of the page holds no data.
- **C. Hosted service.** The operator runs a service with researcher accounts, a database and a CSV export. Researchers create studies and hand out links. The operator holds every response.
- **D. Self-hosted kit.** The operator publishes a container image or a Shiny/plumber application. An institution deploys it on its own infrastructure and holds its own responses. The operator holds nothing.

Two operators are costed throughout: Jeff as a solo maintainer, and an institutional operator, either the HiTOP Society or a university unit.

## Compliance

This section states who holds which role under each architecture and each operator. The statements of law carry no vendor citation. The vendor facts cite the Sources table.

### Data controller and processor roles

Under the GDPR, the controller decides why and how personal data is processed, and the processor processes it on the controller's behalf [L3]. A processor acts under a written contract with the controller, the data processing agreement (DPA) [L4]. The research institution is the controller in every architecture, because it decides the study. What changes is who the processor is.

| Architecture | Processor for the responses | Who signs the DPA with the institution |
|---|---|---|
| A | None for the responses. The page host processes only the page request logs. | Nobody for the responses. |
| B | The storage vendor the researcher chose (Supabase, Firebase, or Google). | The institution, with its own vendor. The page operator is not a party. |
| C | The service operator, and beneath them the cloud provider as a sub-processor. | The service operator, with every institution that fields a study, and the cloud provider with the operator. |
| D | The institution itself, on its own infrastructure. | Nobody outside the institution. |

### When HIPAA applies, and who needs a BAA

HIPAA binds covered entities, which are health plans, health-care clearinghouses, and providers who transmit health information electronically, and it binds their business associates [L1]. A questionnaire study run by a university psychology lab with community participants is not, by itself, a covered entity's activity. It becomes one when the researcher's institution is a covered entity or a hybrid entity's covered component, for example an academic medical center, and the responses are protected health information in its hands [L2]. Then anyone who stores the responses on the institution's behalf is a business associate and must sign a business associate agreement (BAA).

| Architecture | Who is the business associate when HIPAA applies | Solo maintainer | Society or university unit |
|---|---|---|---|
| A | Nobody. No one but the participant and the researcher holds the data. | No BAA. | No BAA. |
| B | The researcher's own storage vendor. Supabase offers a BAA only on its Team plan and above [S9]. Firestore is on the Google Cloud BAA's covered-products list, and no Firebase-branded service is [S7]. Apps Script and Sheets are covered only under a Google Workspace account with the Workspace BAA in place, not a personal account [S16]. | No BAA. The page operator never holds data. | No BAA. |
| C | The service operator, for every covered-entity customer, and the cloud provider as a subcontractor. AWS [S6], Google Cloud [S7] and Azure [S8] each offer a BAA at no extra charge, but only over their HIPAA-eligible services. | A private individual signing BAAs with hospitals, carrying breach-notification duties and liability personally. Not tenable. | Tenable for a university unit, whose institution already holds cloud BAAs and a security office. Tenable for the Society only after it procures those agreements and the insurance behind them. |
| D | The institution, deploying inside its own covered environment. | No BAA. The maintainer ships software. | No BAA. |

### GDPR: DPA and EU residency

The GDPR applies when participants are in the EU or EEA, or when the institution is established there. The controller needs a DPA with every processor [L4], and many EU institutions require the data to stay in an EU region. Under A there is no processor for the responses. Under B the institution signs with its own vendor. Supabase offers a Frankfurt region [S9], and the Google Cloud BAA covers every region [S7]. Under C the operator signs a DPA with each EU institution and must offer an EU region. Under D the institution chooses its own region.

### The questions an IRB asks

IRBs ask the same data-security questions of every route. Where is the data stored, and in which country. Who can read it. Is it encrypted in transit and at rest. How long is it kept, and how is it destroyed. Which identifiers are collected, and how are they separated from the responses. What happens on a breach. Each architecture answers them differently.

| Architecture | The IRB story |
|---|---|
| A | The responses never leave the participant's device until the participant sends the file. That last step is the weak point: a file sent by email is an identified transmission the study did not design. Workable for in-lab sessions where the researcher collects the file on the spot. Hard to describe for remote studies. |
| B | The researcher describes a vendor store (Supabase, Firebase or Google) the IRB does not know. Many IRBs keep an approved-vendor list, and a vendor outside it adds a security review. The REDCap story the IRB already accepts stays with the existing data-dictionary route, not with B. |
| C | The researcher describes a third-party service run by a private individual or a society, with no institutional agreement behind it. This is the hardest story of the four. The IRB will ask for a data use agreement, a security review, and the operator's breach plan. |
| D | The institution's IT reviews the container once. After that the IRB treats it as institutional infrastructure, the same story as REDCap. |

### Consent capture and participant identifiers

Consent text is the IRB's, not the package's. IP1 makes instrument text and instructions sacrosanct, and consent is neither, so a form shows the researcher's approved consent text as configuration, before the instrument. The form collects no direct identifier by default. The participant link carries a study identifier and an optional participant code the researcher assigned, so the researcher links the responses to their own roster and the store holds no name. Under A the file name carries that code. Under B the posted row carries it as its first column, next to the item columns. Under C and D the same column is the export's first.

## Architecture fit with the package

**Rendering from the tables, under IP1.** The `*_items` tables, the `*_instructions` objects and `hitophsum_choices` hold every word a form shows. A renderer that reads a JSON export of those tables shows the content as the package holds it, and a parse-and-compare test locks the export to the tables, the pattern the Qualtrics and REDCap generators already follow. The export is a distributed artifact under D-016: it carries a build date and a package version, and `hitop_artifacts` locks its checksum. A form that reads it at load shows both to the participant's browser and to the researcher.

**The builder's stack against plain JavaScript.** The builder runs R in the browser through webR and installs the package from r-universe on every first visit, about twenty seconds. That is acceptable for a researcher building a form once. It is not acceptable for a participant opening a link. M045 established two facts that carry over: R under WebAssembly cannot shell out, and plain GitHub Pages serves webR with no special headers. Neither is needed here. A collection form is plain JavaScript over the JSON export, loads in under a second, and shares no code with the builder beyond the tables it reads. Every architecture below renders this way, so the renderer is built once and reused four times.

**Dependencies under GP4.** The renderer and anything server-side live outside the package. The package's only new surface is the JSON export script under `data-raw/` and, for the scoring hand-off, documentation and a test. No new Import. Architecture C and D each need a server runtime, a database driver and an authentication library, all of them in their own repository, none in `DESCRIPTION`.

**Versioning a served form under D-016.** A form served from a URL changes underneath a running study when the export is regenerated. The export therefore carries its build date, the form records that date in every submission, and the researcher's export shows which build each participant saw. This is the same discipline D-016 applies to the Word forms, extended to a form nobody downloads.

**Column names at retrieval under D-052 and D-055.** The renderer names each response field with `item_names()`'s pattern, read from the JSON export rather than rebuilt in JavaScript, so the export's first row is the package's column names and no `rename_*` call is needed.

**Scoring hand-off through descriptors.** A study is configured from a module descriptor, the same JSON the builder writes today. The form reads it to know which items to show and in which order. The export ships it back beside the responses. The researcher's scoring call is the one the modules article documents today: `score_hitopsr(responses, items = seq_along(responses), module = read_module("study.json"), layout = "printed")`.

**One constraint on architecture B.** A REDCap API token is issued by a REDCap administrator and grants import rights on the project it belongs to [S12], and a static page has nowhere to hide one. A token embedded in the page is readable by every participant. The REDCap-API variant of B therefore needs a relay that holds the token, and a relay is a server, which is architecture C or D. B's viable stores are the ones designed for anonymous client-side writes under a policy: Supabase with an insert-only row-level policy, Firestore with insert-only security rules, and an Apps Script web app deployed as an anonymous endpoint that appends to a sheet the researcher owns.

## Build breakdown

Effort is in working sessions, one session being a sitting of two to four hours of the maintainer's time. Each row names where the component lives. The renderer is shared, so its row is estimated once and repeated.

### A. Device-only

| Component | Sessions | Where it lives |
|---|---|---|
| Form rendering | 5 | A new static page in a new repository, `hitop-form`, a sibling of `hitop-builder`, served from GitHub Pages. Two sessions for the PID-5, HiTOP-SR and HiTOP-BR renderer over the JSON export (item list, four-option grid, page breaks, required-item check). One session for the study link and descriptor read. Two sessions for the HSUM's gating over 638 fields and four field types. In the package: one `data-raw/` script that writes the JSON export and its `hitop_artifacts` row, with a parse-and-compare test. |
| Scoring hand-off | 1 | In the package: a documented read of the saved response files into one data frame, an article section, and a test that a rendered export round-trips through `score_hitopsr()` with `read_module()`. |

Total: 6 sessions.

### B. Static page, researcher-owned storage

| Component | Sessions | Where it lives |
|---|---|---|
| Form rendering | 5 | As in A: the same page in `hitop-form`. |
| Submission transport | 3 | In `hitop-form`: one adapter per store, each a small function posting one row. Supabase (insert under an anonymous key and a row-level policy), Firestore (insert under security rules), Apps Script (POST to a web-app URL). The study configuration names the store and its endpoint. One session each. |
| Storage | 1 | Nothing built. One session of setup documentation per store, with the policy or rules text the researcher pastes in, lives in the `hitop-form` README. |
| Researcher retrieval | 1 | Nothing built. The three stores each export CSV natively. One session documents the export click and the column names it yields, in the `hitop-form` README. |
| Scoring hand-off | 1 | As in A: in the package, the documented read, the article section and the round-trip test. |

Total: 11 sessions.

### C. Hosted service

| Component | Sessions | Where it lives |
|---|---|---|
| Form rendering | 5 | As in A, served by the service instead of Pages. |
| Submission transport | 2 | In a new `hitop-server` repository: an HTTP endpoint that validates a submission against the study's descriptor and writes it. |
| Storage | 3 | In `hitop-server`: a Postgres schema for studies, participants and responses, multi-tenant by researcher account, with backups and a retention rule. |
| Researcher retrieval | 3 | In `hitop-server`: researcher accounts (sign-up, login, password reset), a study page, a CSV export endpoint. Authentication is most of this row. |
| Scoring hand-off | 1 | As in A: in the package, the documented read, the article section and the round-trip test, plus the `hitop-server` export endpoint attaching the descriptor. |

Total: 14 sessions, before operations. Operations are costed in the comparison table and dominate.

### D. Self-hosted kit

| Component | Sessions | Where it lives |
|---|---|---|
| Form rendering | 5 | As in A, served by the kit. |
| Submission transport | 2 | As in C: the same endpoint in `hitop-server`, packaged into the kit's image. |
| Storage | 2 | As in C, single-tenant: one institution, one database, packaged as a container image with a compose file. Backups are the institution's. |
| Researcher retrieval | 2 | As in C, without multi-tenancy: a shared institutional login or a single admin credential, and the CSV export. |
| Scoring hand-off | 1 | As in A: in the package, the documented read, the article section and the round-trip test. |

Total: 12 sessions, plus one session per release to rebuild and publish the image with security patches.

## Comparison

One row per architecture, one column per dimension. The hosting-cost column summarizes the cost table below it, which holds the twelve costed cells. Where the two operators differ, the cell says so. "Solo" is Jeff as a solo maintainer. "Institution" is the HiTOP Society or a university unit.

| Architecture | Who holds the data | Transport and at-rest protection | HIPAA and GDPR obligations | IRB story | Hosting cost per month (pilot / 1,000 a year / 10,000 a year) | Build effort | Ongoing operations burden |
|---|---|---|---|---|---|---|---|
| A. Device-only | The participant, then the researcher. The operator never holds a response. | The page transmits nothing. The hand-off from participant to researcher is whatever the study arranges, and email is the likely default. At rest: the participant's device, then the researcher's machine. | Solo: no BAA and no DPA, because no one but the participant and the researcher processes the responses. Institution: the same. | Weak for remote studies, because the participant sends an identified file over a route the study did not design. Fine for in-lab sessions. | Solo $0, institution $0, at every scale. | 6 sessions | Solo: page updates only, a session per package release. Institution: the same. |
| B. Static page, researcher-owned storage | The researcher, in a store they own: Supabase, Firestore, or a Google sheet. The operator never holds a response. | HTTPS from the page to the store. At rest: the store's own encryption, under the researcher's account. Supabase and Firestore write under an insert-only policy the researcher sets, so the page can add a row and read none. | Solo: none, because the operator is not a processor. The researcher's institution signs its own DPA and, when HIPAA applies, its own BAA: Supabase offers a BAA on its Team plan [S9], Firestore sits on the Google Cloud BAA's list [S7], and a Google sheet is covered only under a Workspace account with the Workspace BAA [S16]. Institution: the same. | A vendor store the IRB does not know, so an approved-vendor check or a security review. | Solo $0, institution $0, at every scale. The researcher pays $0 to $25 at every scale, and $599 plus an add-on of unshown price where a BAA is needed and Supabase is the store. | 11 sessions | Solo: page updates, plus keeping three adapters working as the vendors change their APIs. Institution: the same. The researcher runs their own store. |
| C. Hosted service | The operator, for every study. | HTTPS to the service. At rest: the database's encryption and backups, configured and paid for by the operator. | Solo: the operator is a processor for every study and a business associate for every covered-entity study, signing DPAs and BAAs as a private individual and carrying breach duties personally. Not tenable. Institution: a university unit signs under agreements its institution already holds with AWS [S6], Google Cloud [S7] or Azure [S8]. The Society must first procure those agreements. | The hardest story: a third-party service with no institutional agreement behind it. The IRB asks for a data use agreement, a security review and a breach plan. | Solo: $2 to $13 at every scale. Institution: the same hosting, on a BAA-eligible tier where a study needs one. | 14 sessions | Solo: accounts, backups, patching, uptime, breach notification, and the contracts above, standing and personal. Institution: the same work inside a security office that already does it. |
| D. Self-hosted kit | The institution that deploys the kit. The operator never holds a response. | HTTPS terminated by the institution. At rest: the institution's disk and backup policy. | Solo: none, because the operator ships software. The institution is controller and processor at once and signs nothing outside itself. Institution: the same. | Reviewed once by the institution's IT, then treated as institutional infrastructure, the same story as REDCap. | Solo $0, institution $0, at every scale, as the operator. The deploying institution pays $4 to $13 at every scale, or runs an in-house VM. | 12 sessions, plus 1 per release | Solo: rebuilding and publishing the image with security patches, a session per release, and answering deployment questions. Institution: running the container, its backups and its patching. |

### Hosting cost per month

Twelve cells, each citing the Sources table. The responses are small: a 405-item HiTOP-SR row is about two kilobytes as a CSV row and about ten kilobytes as a Firestore document that stores every field name, so 10,000 responses a year is 20 to 100 megabytes, under every free quota read. The cost therefore does not move with scale inside the range costed. It moves with the compliance tier.

| Architecture | Pilot (50 responses) | 1,000 responses a year | 10,000 responses a year |
|---|---|---|---|
| A | Solo $0, institution $0: a public GitHub Pages site is free, with a soft limit of 100 GB of bandwidth a month [S1]. | Solo $0, institution $0 [S1]. | Solo $0, institution $0. Ten thousand page loads at 200 KB each is 2 GB, under the 100 GB soft limit [S1]. |
| B | Solo $0, institution $0 [S1]. Researcher $0 with Firestore's free plan (1 GiB stored, 20,000 writes a day) [S10] or with Apps Script and a sheet [S15]. Supabase's free project pauses after a week idle, so a live study needs Pro at $25 [S9]. A study needing a BAA on Supabase needs the Team plan at $599 plus a HIPAA add-on whose price the page does not show [S9]. On Firestore, the Google Cloud BAA adds no charge [S7]. | The same figures: solo $0, institution $0 [S1], and researcher $0 [S10, S15], $25 [S9], or $599 plus the add-on with a Supabase BAA [S9]. | The same figures: solo $0, institution $0 [S1], and researcher $0 [S10, S15], $25 [S9], or $599 plus the add-on with a Supabase BAA [S9]. A busy day of a few hundred submissions is under Firestore's 20,000 writes a day [S10], and a year's 100 MB is under Supabase's 500 MB [S9]. |
| C | Solo $2 to $13, institution $2 to $13: a Fly.io shared-cpu-1x machine at $1.94 [S2] with Neon's free 0.5 GB Postgres [S11], or Render's $7 web service with its $6 Postgres [S3], or Railway's $5 Hobby plan [S4]. Where a BAA is needed: AWS [S6], Google Cloud [S7] and Azure [S8] each sign one at no charge, but only over their eligible services, whose prices this note did not read. | The same figures [S2, S11, S3, S4]. | The same figures [S2, S11, S3, S4]. A 20 MB database a year sits under Neon's 0.5 GB [S11] and Render's 1 GB included storage [S3]. |
| D | Solo $0, institution $0 as the operator: a container image on a public registry costs nothing to publish. The deploying institution pays $4 to $13: a DigitalOcean droplet at $4 or $6 [S5], or Render's $7 service and $6 Postgres [S3], or an in-house VM at no marginal cost. | The same figures [S5, S3]. | The same figures [S5, S3]. |

## Recommendation

**B, a static page with researcher-owned storage, built in two steps.** The first step is the renderer with a device save, which is architecture A and is six of B's eleven sessions. It serves in-lab sessions on its own and is the base every other architecture needs. The second step adds the Firestore, Apps Script and Supabase adapters, three sessions, and the setup pages, two sessions. Under B the operator holds no response, signs no agreement, and runs no server, whether the operator is Jeff or the Society. The researcher gets a free route where no Qualtrics or REDCap licence exists, and keeps the data in an account their institution can review.

What B does not give is a REDCap route. A static page cannot hold an API token, so REDCap stays what it is today: the researcher imports the package's data dictionary and fields the form in REDCap itself.

The conditions under which each other architecture becomes the better choice:

- **A becomes better** if the studies asking for an online form are in-lab sessions where the researcher collects the file on the spot, or if IRBs reject every vendor store outside the institution. Then the transport adapters buy nothing, and A is B with five sessions removed.
- **C becomes better** if an institutional operator with a security office, a BAA program and staff funds the service, or if a grant requires a turnkey service that researchers only log in to. Under a solo maintainer it is never better, because the operations burden and the contracts fall on one person.
- **D becomes better** if a partner institution without REDCap asks to host its own collection and has IT to run a container, or if the REDCap-API relay is wanted, since the relay is a server the institution must run inside its own REDCap's trust boundary.
- **None becomes better** if no researcher without Qualtrics or REDCap access asks for an online form before the disposition is taken. The package's existing routes already serve every researcher who has either licence, and eleven sessions buys nothing for them.

## Sources

One row per figure the note uses. Every URL was read directly on the date shown. Vendor pages are S rows. Regulator and legal-text pages are L rows, and the statements of law above cite L rows only.

### Vendor pages

| # | Vendor and page | URL | Date read | Figure read |
|---|---|---|---|---|
| S1 | GitHub Pages, usage limits | https://docs.github.com/en/pages/getting-started-with-github-pages/github-pages-limits | 2026-09-20 | Free for a public repository. Soft limit of 100 GB of bandwidth a month, 1 GB published site size, 10 builds an hour. |
| S2 | Fly.io, resource pricing | https://fly.io/docs/about/pricing/ | 2026-09-20 | A shared-cpu-1x Machine with 256 MB is $1.94 a month. |
| S3 | Render, pricing | https://render.com/pricing | 2026-09-20 | Free web service $0 (spins down after 15 minutes idle, per https://render.com/docs/free). Smallest paid web service, 0.5 CPU and 512 MB, $7 a month. Free Postgres $0. Smallest paid Postgres, 256 MB, $6 a month with 1 GB storage included. A HIPAA BAA is offered on the Enterprise plan only. A GDPR DPA is offered on every plan. |
| S4 | Railway, pricing | https://railway.com/pricing | 2026-09-20 | Hobby plan $5 a month including $5 of usage. Free plan $0 with $1 of usage credit. |
| S5 | DigitalOcean, Droplet pricing | https://www.digitalocean.com/pricing/droplets | 2026-09-20 | $4 a month for 512 MiB and 1 vCPU. $6 a month for 1 GiB and 1 vCPU. |
| S6 | AWS, HIPAA eligible services | https://aws.amazon.com/compliance/hipaa-eligible-services-reference/ | 2026-09-20 | A BAA is required before PHI is placed in an eligible service. The BAA is accepted self-service in AWS Artifact at no charge (https://aws.amazon.com/blogs/security/introducing-the-self-service-business-associate-addendum/). Several EU regions exist. |
| S7 | Google Cloud, HIPAA | https://cloud.google.com/security/compliance/hipaa and the covered-products list at https://cloud.google.com/security/compliance/hipaa-compliance | 2026-09-20 | The BAA covers all regions and the listed services, and is not subject to modification. The covered list names Firestore, Cloud Run, Cloud SQL and Identity Platform. No Firebase-branded service is on it. |
| S8 | Microsoft Azure, HIPAA and HITECH | https://learn.microsoft.com/en-us/compliance/regulatory/offering-hipaa-hitech | 2026-09-20 | A BAA is offered to covered-entity and business-associate customers through the Microsoft Product Terms and the Data Protection Addendum. |
| S9 | Supabase, pricing and regions | https://supabase.com/pricing and https://supabase.com/docs/guides/platform/regions | 2026-09-20 | Free: 500 MB database, 5 GB egress, and a project is paused after one week of inactivity. Pro from $25 a month. A HIPAA add-on is available on the Team plan, $599 a month, its own price not shown. Regions include Central EU (Frankfurt). |
| S10 | Firebase, pricing | https://firebase.google.com/pricing | 2026-09-20 | Spark plan, free: 1 GiB of Firestore storage, 50,000 reads and 20,000 writes a day. Blaze plan: the same free quota, then pay as you go. |
| S11 | Neon, pricing | https://neon.com/pricing | 2026-09-20 | Free: 0.5 GB of storage and 100 compute-hours a project. Launch plan, usage based: $0.106 a compute-hour and $0.35 a GB-month, no monthly minimum. |
| S12 | REDCap API access, a university help page | https://support.sc-ctsi.org/index.php?pg=kb.page&id=3 | 2026-09-20 | The API page appears only once a user-rights holder grants API Export and API Import/Update on the project. A token request goes to a REDCap administrator, who issues it. |
| S13 | Qualtrics, pricing | https://www.qualtrics.com/pricing/ | 2026-09-20 | No public figure. The page directs to a pricing request. |
| S14 | REDCap consortium, licensing | https://www.project-redcap.org/ | 2026-09-20 | REDCap is free to non-profit organizations that join the consortium. Each institution hosts its own instance. |
| S15 | Google Apps Script, quotas | https://developers.google.com/apps-script/guides/services/quotas | 2026-09-20 | Consumer account: 20,000 URL Fetch calls and 90 minutes of trigger runtime a day. Workspace account: 100,000 calls and 6 hours. Web apps run at no charge on either account. |
| S16 | Google Workspace, HIPAA | https://knowledge.workspace.google.com/admin/compliance/hipaa-compliance-with-google-workspace-and-cloud-identity | 2026-09-20 | Workspace and Cloud Identity customers subject to HIPAA must accept a BAA before using PHI in the services on the included-functionality list. A personal Google account is not a Workspace customer. |

### Regulator and legal-text pages

| # | Page | URL | Date read | What it states |
|---|---|---|---|---|
| L1 | HHS, covered entities and business associates | https://www.hhs.gov/hipaa/for-professionals/covered-entities/index.html | 2026-09-20 | A covered entity is a health-care provider that transmits information electronically for a standard transaction, a health plan, or a clearinghouse. An entity that is neither a covered entity nor a business associate does not have to comply with the HIPAA Rules. |
| L2 | HHS, research under the Privacy Rule | https://www.hhs.gov/hipaa/for-professionals/special-topics/research/index.html | 2026-09-20 | The Privacy Rule governs how covered entities use and disclose protected health information for research. Most human-subjects research runs under the Common Rule, whose protections are separate from the Privacy Rule's. |
| L3 | GDPR Article 4, definitions | https://gdpr-info.eu/art-4-gdpr/ | 2026-09-20 | A controller determines the purposes and means of the processing. A processor processes personal data on behalf of the controller. |
| L4 | GDPR Article 28, processor | https://gdpr-info.eu/art-28-gdpr/ | 2026-09-20 | Processing by a processor is governed by a contract or other legal act that binds the processor to the controller. |

Pages tried and not read, so that no figure rests on them: Hetzner Cloud's price list renders only after a script this session did not run. Fly.io's Managed Postgres price page returned 404. The Google Workspace HIPAA included-functionality list returned 404, so which Workspace services the BAA covers is not stated here.

## Disposition

The maintainer's disposition on building any architecture lives in `ROADMAP.md` (a candidate row) or in `DECISIONS.md` (a rejection with its rationale), never here.

## Open questions

- Whether any researcher without a Qualtrics or REDCap licence ever asked for an online form. No such request is recorded in the repository's issues or tracking files — observed 2026-09-20.
- Which Google Workspace services the Workspace BAA covers, and so whether a sheet behind an Apps Script endpoint is a BAA-covered store. The included-functionality page did not load — observed 2026-09-20.
- The price of a BAA-eligible compute and database tier on AWS, Google Cloud or Azure for architecture C. Not read — observed 2026-09-20.
- The price of Supabase's HIPAA add-on on the Team plan, which the pricing page did not show — observed 2026-09-20.
