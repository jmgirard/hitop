# Online response collection as an alternative to Qualtrics and REDCap (M093)

**Provenance.** Ingested 2026-09-20 by M093 from a read of this package's own surfaces (`R/module_file.R`, `R/util.R`, `R/generate_qualtrics.R`, `R/generate_redcap.R`, `R/sysdata.rda`, `hitop_artifacts`, `hitophsum_items`) at commit `6d2be446`, a read-only read of the builder repository `jmgirard/hitop-builder` (`README.md`, `index.html`) at its 2026-09-20 checkout, and the vendor and regulator pages listed in the Sources table below.
Pagination: —.
Extraction: a 2026-09-20 snapshot; the vendor figures were read directly from the pages the Sources table names on that date, and every price and limit moves independently of this note — observed 2026-09-20.

**Scope.** This page compares four ways to collect questionnaire responses online without Qualtrics or REDCap, and costs each under two operators. It is not a summary of any one source, and it builds nothing: no architecture is chosen here. This is a reference, not an authority. Status lives in `ROADMAP.md`, decisions in `DECISIONS.md`, architecture in `DESIGN.md`. A synthesis note that starts asserting status is a second tracking system.

**Evidence snapshot.**

- The module descriptor format, version `1.0`, with its required fields `format`, `instrument` and `scales`, and its `itemOrder` slot — `R/module_file.R` at `6d2be446` — observed 2026-09-20.
- The item-column naming helper, which writes a lowercase stem, an underscore, and a number zero-padded to the widest item — `R/util.R:697` at `6d2be446` — observed 2026-09-20.
- The four instruction objects, each a start text and (for the three scored instruments) a four-option response table with integer values — `R/sysdata.rda` at `6d2be446` — observed 2026-09-20.
- The HSUM item table: 650 rows, 638 of them gated on another field's value, four field types (radio, text, checkbox, dropdown), nine choice sets — `hitophsum_items` and `hitophsum_choices` at `6d2be446` — observed 2026-09-20.
- The builder's no-backend posture and its webR stack — `jmgirard/hitop-builder` `README.md`, sections "What it does, and what it does not" and "How it works" — observed 2026-09-20.
- The artifact manifest, 39 rows over 20 files, build dates from 2026-07-16 to 2026-08-30 — `hitop_artifacts` at `6d2be446` — observed 2026-09-20.

## What the package offers today

The package generates blank forms for three delivery routes and scores the responses afterwards. The routes are a Word document for paper, a Qualtrics import file, and a REDCap data dictionary. The researcher fields the form through their own Qualtrics or REDCap account, exports the responses, and scores them in R. The package never touches a response in transit. The browser builder at `jmgirard.github.io/hitop-builder/` follows the same posture: it builds the blank form in the browser tab and transmits nothing.

## Requirements

An online form that replaces Qualtrics or REDCap must reproduce what those routes give the researcher today. Each requirement below names the package surface it derives from.

| # | Requirement | Package surface it derives from |
|---|---|---|
| R1 | A participant link the researcher can hand out, and a per-study configuration behind it: which instrument, which module, which item order, and a study label. | The builder's two-step flow chooses instrument scales and settings once and writes them into the bundle. The link must carry the same choices. |
| R2 | Response storage that survives the browser tab: the responses of every participant in a study, in one place the researcher can reach. | Qualtrics and REDCap hold responses server-side today. A device-only save (architecture A) changes this contract, and the comparison table says so. |
| R3 | An export in the package's item-column names: `hsr_001`..`hsr_405`, `hbr_01`..`hbr_45`, `pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100`, `pid5bf_01`..`pid5bf_25`. | `item_names()` at `R/util.R:697` builds every name (D-052, D-055). The `rename_*` helpers exist for data that arrives under other names, but an export the package designs must not need them. |
| R4 | Module support: a study can field a HiTOP-SR module, and its export scores through the module's descriptor. | `read_module()` rebuilds a module from a JSON descriptor whose `scales` field is required and whose `itemOrder` records a shuffled form's printed order (D-039). The form must be built from a descriptor, and the export must ship one. |
| R5 | Instrument coverage: PID-5 in three forms, HiTOP-SR, HiTOP-BR, and the HSUM. | The `*_items` tables and the four `*_instructions` objects hold every item, option and instruction. The HSUM adds display logic: 638 of its 650 fields show only when a gate field holds a gate value, across four field types and nine choice sets. Qualtrics's flat import cannot carry that logic (DESIGN Known issue #5), so the HSUM's Qualtrics artifact is built through the API instead. |
| R6 | Content fidelity under IP1: item text, response options and instructions render exactly as the tables hold them, and a served form carries a build date and a version the reader can check. | IP1 makes item text and instructions sacrosanct. D-016 identifies every distributed artifact by build date and locks it by checksum in `hitop_artifacts`. A served form is a distributed artifact in the same sense. |
| R7 | A scoring hand-off: the researcher opens the export in R and scores it with no renaming and no retyping of scale names. | `score_hitopsr(data, items = read_module(f), layout = "printed")` is the hand-off today for a module form. The online export must be that call's input. |
| R8 | No dependency the package's users must install for a route they do not use. | GP4: internals are base R, a family earns an Import, a per-function need uses Suggests. Anything server-side belongs outside the package. |

Two facts bound the design space. First, the package holds every item, option and instruction as data, so a form renderer needs only a JSON export of those tables. Second, the builder already runs R in the browser through webR and installs the package from r-universe at page load, about twenty seconds on a first visit. A collection form must load in the time a participant will wait, so it cannot inherit the builder's stack. That trade is worked through in the build breakdown below.
