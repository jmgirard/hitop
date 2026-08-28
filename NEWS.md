# hitop 0.2.0

This release makes several **breaking** API changes to stabilize the interface
before a CRAN submission.

* **Two HiTOP-SR scales are now named the way the instrument's introduction
  paper prints them.** The scale abbreviated `NSSI` is named in full,
  `Non-suicidal Self-injury`, spelled out the way the other 75 scales already
  were; and the scale called `Body Focus` is named `Appearance Focus`.
  *Breaking:* `score_hitopsr()` returns `hsr_nonSuicidalSelfInjury` and
  `hsr_nonSuicidalSelfInjury_se` where it returned `hsr_nssi` and `hsr_nssi_se`,
  and `hsr_appearanceFocus` and `hsr_appearanceFocus_se` where it returned
  `hsr_bodyFocus` and `hsr_bodyFocus_se`; code selecting the old names must be
  updated. Because the scale tables are sorted by name, both scales also move
  position in the returned tibble — the first from 448 to 451 and its standard
  error from 524 to 527, the second from 412 to 408 and its standard error from
  488 to 484 — and the columns lying between an old and a new position shift by
  one, so code selecting scored columns by position rather than by name must be
  updated too. Both scales are also addressed by name elsewhere:
  `hitop_module()` no longer accepts `"NSSI"` or `"Body Focus"`, and
  `read_module()` rejects a saved module descriptor that records either, so any
  descriptor written before this release must be rebuilt;
  `available_scales("hitopsr")` lists the new names. No score changes: every
  column, those two included, returns exactly the values it did before. The
  names also change on the scoring page of the two Word questionnaires; the
  Qualtrics and REDCap exports print no scale names and are unchanged.

* **A chosen set of an instrument's scales is called a *module*.** The
  entries below describe that feature under its final names. *For development-
  version users only:* these were briefly called `hitop_subset()` and `subset`
  before release. Both still work and warn, every function taking a `module`
  also accepts a descriptor built by `hitop_subset()`, and supplying both
  `module` and `subset` in one call is an error. One further consequence of
  the argument rename: in `score_hitopsr()`, the abbreviation `m = ` is now
  ambiguous between `module` and `missing` and errors — write `mo = ` and
  `mi = `, or spell the arguments out.

* **`write_module()` and `read_module()` save a module to a file and read it
  back.** Keep the file beside the forms you generate, and at scoring time
  hand `read_module()` to the `module` argument instead of retyping every
  scale name. The file is small, plain JSON you can read, edit, and send to a
  collaborator; it carries a format version so later releases can grow it.
  What it records is scale names, never a scoring key: the items and their
  reverse-keying flags are rebuilt from the package's own tables on read, and
  the item numbers the file records are checked against that rebuild, so a
  descriptor written against tables that have since changed stops with an
  error rather than scoring quietly. Each failure carries its own condition
  class, listed on `?read_module`; a descriptor that is malformed rather than
  merely wrong is refused by one of those same classes, never by a bare R
  coercion error. The order the item numbers are written in carries no
  meaning, so a hand-edited file may list them any way round.

* **The three HiTOP-SR generators can save a module descriptor beside the file
  they build.** Pass `descriptor = "module.json"` to
  `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()`, or
  `generate_redcap_hitopsr()` and one call produces both the form you field
  and the file that scores the data it comes back as. A call passing no
  `module` writes a descriptor naming every scale, so a full administration is
  described too. On a shuffled Word form (`randomize = TRUE`) the descriptor
  also records the printed order, returned on the read module's `item_order`
  attribute — the record a shuffled whole-instrument form leaves nowhere else,
  since no crosswalk is printed for one. The descriptor is written before the
  instrument file, so an unwritable path is reported before any form is
  produced, and it is removed again if the form itself cannot be written.
  `write_module()` now writes an `item_order` attribute as the file's
  `itemOrder` field, so a descriptor read and written again keeps the order it
  recorded.

* **The jsonlite package moved from Suggests to Imports**, so it is now
  installed with hitop rather than optionally. `write_module()` needs it, and
  the browser module builder runs in an environment where suggested packages
  are not installed.

* **`available_scales()` lists the scales you can build a module from**, with
  the name printed on the form, the camelCase stem that names the scored
  output column, the item count, and — in a new fourth column, `Brief` — the
  scale's brief clinician-facing definition, so you no longer need to know
  which dataset to open before choosing scales. The definition is matched to
  the scale on that camelCase stem, never on a printed name, and a stem with
  no definition behind it is an error rather than a blank.

* **`hitopsr_definitions` gains a `camelCase` column**, naming whatever each
  row defines: the subscale where there is one, otherwise the scale. It is the
  key `available_scales()` joins on, and it lines up with
  `hitopsr_scales$camelCase` and `hitopsr_subscales$camelCase`.

* **The [browser module
  builder](https://jmgirard.github.io/hitop-builder/) shows those definitions
  while you pick.** Pointing at a scale, or reaching its checkbox with the Tab
  key, brings up that scale's definition; Escape dismisses it. The page reads
  the text from the installed package rather than keeping a copy, so a version
  that does not supply it shows the list exactly as before.

* **A new article, [Building HiTOP-SR
  Modules](https://jmgirard.github.io/hitop/articles/modules-hitopsr.html),
  walks the whole module workflow**: choosing scales, describing the module,
  generating the paper, Qualtrics, and REDCap files, selecting the collected
  item columns, and scoring plus reliability. The HiTOP-SR scoring vignette now
  links to it rather than carrying its own shorter copy.

* **A new web app builds HiTOP-SR modules in your browser.** Tick the scales
  you want at
  [jmgirard.github.io/hitop-builder](https://jmgirard.github.io/hitop-builder/)
  and download a Word, Qualtrics, or REDCap instrument containing only those
  items. The page downloads R and this package into your browser and generates
  the files there, so nothing you select or produce is sent anywhere; it builds
  blank questionnaires and scores nothing. Linked from the Instruments menu on
  the package website. A *Word item order* box there shuffles the printed order
  of the Word form's items, with an on-page warning that the collected columns
  must be put back into the instrument's own order before scoring; the
  Qualtrics and REDCap downloads are unaffected by it. A *Word item numbering*
  group chooses between numbering the printed items 1 to n -- the default --
  and keeping the HiTOP-SR's own item numbers, which are the names the same
  page's Qualtrics and REDCap exports give the collected variables, so paper
  responses can be typed into a project built there without translating them;
  it too leaves those two downloads unchanged. Ticking every scale now builds
  the whole instrument rather than a module, so that Word form is headed
  `HiTOP-SR (v1.0)` and the three downloads are named for the instrument.

* **The Qualtrics and REDCap generators now check their arguments** (breaking).
  `block_name`, `id_prefix`, and `include_instructions` on every
  `generate_qualtrics_*()`, `form_name` and `required` on every
  `generate_redcap_*()`, and `breaks` on every generator that takes it,
  previously wrote whatever they were handed into the import file:
  `id_prefix = 1` wrote question IDs reading `1_001`, and `required = "yes"`
  left the dictionary's required column blank on every row instead of marking
  anything. Each now raises an error naming the argument, and no file is
  written. `breaks` still accepts `0` and `NULL` to turn pagination off. Files
  built from valid arguments are byte-for-byte unchanged.

* **The REDCap generators no longer need an external `zip` program.** They
  built the instrument archive by running the system's `zip` command, which
  silently failed wherever no such program was installed -- commonly on
  Windows. The archive is now written by the {zip} package, in R, with no
  outside program involved. The file REDCap receives is unchanged.

* The download buttons on the instrument pages now serve the files from the
  package website itself, so a browser saves each one under its own name. The
  Qualtrics survey files used to open as text in a new tab, because GitHub
  serves them as plain text; they now download ready to import. The GitHub
  links keep working for anyone who saved them.

* The HiTOP-HSUM Qualtrics survey file (`hitophsum_qualtrics.qsf`) now imports
  into Qualtrics. The previous build was exported through the Qualtrics API,
  which writes absent values as empty objects rather than as JSON `null`; the
  importer rejects that file with an internal error. The file now carries the
  same encoding as a survey exported from the Qualtrics interface. Its
  content — questions, response choices, and skip/display logic — is unchanged.

* The Qualtrics import instructions note that a browser may save the survey
  file with a `.txt` extension, and that renaming it back to `.qsf` is safe.

* `?hitop_module` now says that a module naming every scale holds exactly the
  instrument's own items but is still framed as a module by
  `generate_docx_hitopsr()` -- the `HiTOP-SR Module` header, and a crosswalk
  when the form is shuffled -- so a caller wanting the full instrument's
  framing passes no `module` at all.

* The `calc_se` help text on `score_pid5()`, `score_hitopsr()`, and
  `score_hitopbr()` now states what these standard errors are computed over and
  says plainly that they are not standard errors of measurement. Each is the SD
  of the items a respondent actually answered over the square root of how many
  they answered (for a PID-5 full- or short-form domain, over its three
  contributing facet scores), so no reliability estimate enters it: it describes
  how much a respondent's answers varied within a scale, not how precisely the
  scale measures. The reliability functions are named for that. The vignettes
  already said this; the help pages said only "the standard error of each scale
  score", which reads as a standard error of measurement.

* The scoring vignettes described the `calc_se` standard errors incorrectly and
  now describe what is actually computed. The divisor is the number of items a
  respondent answered, not the number of items on the scale. A PID-5 short-form
  domain score is a mean of three facet scores rather than of items, so its
  standard error is taken over those three facet scores. The vignettes also no
  longer suggest converting these standard errors into confidence intervals:
  they summarize how much a respondent's answers varied within a scale, not how
  precisely the scale measures the trait.

* **Norm-referenced profile plots.** New `plot_pid5()` draws one respondent's
  normed PID-5 scores as a profile against the published normative tables —
  the five domains (plus the brief form's total), or all 25 facets grouped by
  domain, on a T-score or percentile axis. It presents scores against norms and
  characterizes none of them: there are no severity bands, no elevation
  thresholds, and no annotation about what a score means. The score axis spans
  the range the tables actually print, so two profiles on the same version are
  directly comparable. Returns an ordinary ggplot object, which stays in
  Suggests — install {ggplot2} to use it. Worked profile examples for all three
  forms: `vignette("pid5_scoring")`, `vignette("pid5sf_scoring")`, and
  `vignette("pid5bf_scoring")`.

* `plot_pid5(labels = FALSE)` no longer reserves the extra room a value label
  would need on the score axis. That padding is the label's, and reserving it
  when no label is drawn spent width on empty margin — exactly the width
  `labels = FALSE` is asked for to save. Profiles drawn with labels are
  unchanged.

* `plot_pid5()` now reports a non-numeric normed column the way `norm_pid5()`
  already did: one bullet per offending column, each naming that column and its
  own class, rather than a single line listing the names with no types, and a
  closing line saying what to do about it. The two functions now share one
  guard, so the two messages cannot drift apart.

* `plot_pid5()` now places each value label to the right of its point rather
  than above it, and pads the score axis to hold it. Offsetting upward took the
  room out of the panel's height, where it ran out on a smaller figure and the
  top label in each panel was clipped. The labels fit on figures about 7 inches
  wide or more; see `labels` below for narrower ones.

* `plot_pid5()` gains a `labels` argument. The value labels need a figure about
  7 inches wide or more; set `labels = FALSE` for a narrower one and the points
  and profile line are drawn without them.

* **Scoring modules.** `score_hitopsr()` and `reliability_hitopsr()` gain a
  `module` argument taking the same description that `hitop_module()` builds and
  the `generate_*_hitopsr()` functions consume. Give it the item columns you
  actually collected and it scores only that module's scales, returning the same
  values a full 405-item administration would have produced for them. Without
  the argument both functions behave exactly as before. See
  `vignette("hitopsr_scoring")`.

* **Clearer errors for bad arguments.** Every argument check across the package
  now reports which argument was wrong, what was supplied, and which function
  was called, instead of printing the internal test that failed. This affects
  `data`, `prefix`, `name`, `append`, `calc_se`, `alpha`, `omega`, and `top`
  throughout the scoring, reliability, norming, labelling, and ranking
  functions. A bad
  `dir` in `rank_scales()` now lists the permitted values and suggests the
  closest match. No function accepts or rejects anything it did not before —
  only the messages changed.

* **PID-5 normative tables.** The new `pid_norms` dataset carries the published
  normative score distributions for the PID-5, PID-5-SF, and PID-5-BF: the raw
  score and percentile at each T score for the five domain scales, for all 25
  facet scales of the full and short forms, and for the brief form's total
  score; and the percentile at each raw score for the INC, INC-S, ORS, and PRD
  validity scales. Scale names match the columns `score_pid5()` and
  `validity_pid5()` return. Every value comes from Markon et al. (2024) and is
  verified cell by cell against that source. Note that most facet columns print
  raw scores above the 3.00 a mean of 0–3 items can reach, and 19 of them repeat
  a printed 4.00 across several T rows; those rows ship exactly as published and
  are simply unattainable.

* **PID-5 score conversion.** The new `norm_pid5()` converts scored PID-5,
  PID-5-SF, and PID-5-BF columns to normative T scores and percentiles from
  `pid_norms`, adding a `_t` column for every converted scale whose normative
  rows carry a T score and a `_ptl` column for every converted scale. Every
  returned value is a printed cell of Markon et al. (2024): the nearest printed
  row is selected and nothing is interpolated. Scores outside a printed range are
  capped to the nearest end with a warning rather than extrapolated, and scales
  the tables do not cover return `NA` with a warning naming them. Scores
  collected on any four-option response coding are accepted: a coding shifted
  off the official 0-3 range (1-4, say) is reconciled to it before lookup, per
  scale — item means by the coding's low value, `PRD` by that value times its
  item count, and `INC`, `INC-S`, and `ORS` left alone as coding-invariant — and
  a warning names which scales were adjusted and which were not. A coding
  implying some other number of response options has no mapping onto the
  four-option tables and returns `NA` in every conversion column with a warning.
  Note that `validity_pid5()`'s published cut scores are still *not* adapted to
  a shifted coding, so a reconciled percentile and an unreconciled validity flag
  can appear together; see `?norm_pid5`. Every report the function makes is a
  warning condition, so one `suppressWarnings()` call silences it entirely. All
  25 facets convert on the full and short forms as well as the five domains; on
  the brief form, and for `SD-TD` on any form, the tables carry nothing and the
  conversion columns come back `NA` with the warning above. The PID-5, PID-5-SF,
  and PID-5-BF vignettes each gain a section demonstrating the conversion.

* **PID-5-BF total score** (breaking). `score_pid5(version = "BF")` now returns a
  `total` column after its five domains, so the brief form's normed total score
  in `pid_norms` has something to convert. Following Markon et al. (2024, p. 23),
  it is the item-level mean over all 25 items rather than the mean of the five
  domain means; the two agree on complete data and differ only when items are
  missing. Because each scale applies the `missing` rule independently, a total
  can be reported alongside one or more `NA` domains — see `?score_pid5` for the
  exact bounds. Two consequences for existing code: `reliability_pid5(version =
  "BF")` now returns **six** rows rather than five, and the printed scoring table
  on the PID-5-BF Word forms gains a `Total` row listing all 25 items (both forms
  are rebuilt, with new `hitop_artifacts` entries). Code that counts the columns
  of `score_pid5(version = "BF")` or the rows of `reliability_pid5(version =
  "BF")` must be updated. The PID-5 and PID-5-SF are unaffected.

* **PID-5 Word forms print the response options on two lines.** The response
  scale printed above the items on the PID-5, PID-5-SF, and PID-5-BF Word forms
  now runs across two lines — `0` and `1` on the first, `2` and `3` on the
  second — so that no option phrase is broken partway through by the column
  width. The option values and wording are unchanged, and the HiTOP-SR and
  HiTOP-BR forms keep their single-line scale. All six PID Word files
  (US and A4) were regenerated, with new `hitop_artifacts` entries.

* **HiTOP-SR module Word forms are numbered `1` to `n`, and can be shuffled.**
  `generate_docx_hitopsr()` gains `renumber` (default `TRUE`), so a module's
  paper form no longer prints the full instrument's gapped numbers; pass
  `renumber = FALSE` for the previous behavior. It also gains `randomize`
  (default `FALSE`), which prints the items in a random order and appends a
  crosswalk from each printed number back to its original HiTOP-SR number, so
  a shuffled form is still scoreable from the paper alone. Use `set.seed()`
  to make an order reproducible. Every call's invisible return value now
  carries an `item_order` attribute holding the original item numbers in
  printed order. Data collected on a shuffled form must be reordered through
  that attribute before `score_hitopsr()`, which addresses a module's items in
  ascending original order; `?generate_docx_hitopsr` shows the idiom.
  `generate_qualtrics_hitopsr()` and `generate_redcap_hitopsr()` are
  deliberately unchanged: there an item number names a collected data column.
  No distributed form under `inst/extdata/` changed, since each is the full
  instrument and already numbered from one. The two new arguments sit between
  `module` and `subset` in the signature, so any call passing arguments
  positionally past `font_family` must be respelled by name.

* **A HiTOP-SR Word form built from a module says so in its header.** With no
  `title` of your own, `generate_docx_hitopsr()` now heads a module form
  `"HiTOP-SR Module (v1.0)"` and a full-instrument form `"HiTOP-SR (v1.0)"`, so
  a paper holding a handful of scales is no longer titled as the whole 405-item
  instrument. Passing `title` still prints exactly what you pass, including on
  a module form. The item text, response options, and administration
  instructions are untouched, and no distributed form under `inst/extdata/`
  changed, since each is the full instrument.

* **Generate a HiTOP-SR module from selected scales.** The new
  `hitop_module()` describes a chosen set of an instrument's scales, and
  `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()`, and
  `generate_redcap_hitopsr()` each take it as a `module` argument to emit a
  form containing only those scales' items. The Qualtrics and REDCap exports
  keep each item's original HiTOP-SR number, because there an item number
  names a collected data column; the Word form numbers its items `1` to `n`
  down the page (see the entry above).
  Scale names may be given as printed on the instrument (`"Antisocial
  Behavior"`) or as the camelCase stems used in scored output
  (`"antisocialBehavior"`), in any mixture and ignoring case. Subsetting is
  currently available for the HiTOP-SR only.

* `norm_pid5()` now checks its `scores` argument before converting anything.
  Naming the same score column twice is an error rather than a silently
  duplicated pair of output columns, and a factor or character score column is
  an error rather than being coerced — a factor's integer codes are not its
  scores, and a character column coerces to `NA`. Logical columns still
  convert. Every complaint about the argument names `scores`, not the `items`
  or `scales` of the shared validators behind it. That error now gives each
  offending column its own line with its full class (an ordered factor reads as
  `<ordered/factor>` rather than as `ordered`), and errors raised while
  reconciling a shifted response coding are attributed to `norm_pid5()` rather
  than to the internal helper that raised them.

* `rank_scales()`'s `prefix` argument is now matched **literally** (breaking).
  It was previously compiled as a regular expression anchored to the start of
  the column name, which meant a prefix containing `(` failed with a regex
  error and one containing `.` could strip a prefix that was never there. A
  column name that does not begin with exactly `prefix` is now carried through
  whole. Code relying on a regex `prefix` must pre-strip the names instead.
  `norm_pid5()` matches `prefix` the same way.

* Qualtrics question IDs are now zero-padded to the width of the largest item
  number rather than the number of items. Output for every full instrument is
  unchanged; the change keeps IDs uniform in a module file.

* **New instrument overview page.** A single "HiTOP Instruments" page presents
  the three self-report measures — HiTOP-SR, HiTOP-BR, and HiTOP-HSUM — as
  at-a-glance summary cards, each linking to its full download page. It is the
  first entry in the website's "Instruments" menu. Its HiTOP-BR card now
  describes the eight scales at their true hierarchy levels — six spectra plus
  the Externalizing superspectrum and a general p-factor — rather than calling
  all eight "spectra".

* **Redesigned instrument download pages.** Each download button on the
  website's instrument pages now shows its file's build date, and the
  version tables are replaced by a collapsible "Current builds & version
  history" panel rendered from the `hitop_artifacts` manifest. The
  manifest's change notes were reworded for a general audience (data
  unchanged otherwise).

* **Centralized import instructions.** A new "Importing into Qualtrics &
  REDCap" article gives step-by-step instructions for all three import
  formats — Qualtrics survey files (`.qsf`), Qualtrics questions files
  (`.txt`), and REDCap instrument ZIPs — and every instrument download page
  now links its Qualtrics and REDCap cards to it. The REDCap import steps
  previously embedded in each `generate_redcap_*()` help page now live in
  that article, which the functions point to via "See also".

* **Distribution artifacts are now versioned.** The new `hitop_artifacts`
  manifest dataset identifies every prebuilt file in `inst/extdata/` by
  build date and MD5 checksum (one row per build, history kept); the
  website's download pages show each instrument's current builds and a
  version history; and generated Word documents carry a build stamp in the
  footer ("Generated YYYY-MM-DD · hitop X.Y.Z"). A test suite locks the
  committed files to the manifest, so no distributed artifact can change
  again without a visible version bump. **Artifact filenames no longer
  carry the instrument version** (e.g., `pid5_1.0_A4.docx` is now
  `pid5_A4.docx`, so previously shared download URLs no longer resolve),
  and the `generate_docx_*` default `file` arguments dropped `_1.0`
  accordingly

* **HiTOP-HSUM aligned to its authoritative source** (the HiTOP Society's
  "revised SUD module-August 2024" development worksheet):
  `hitophsum_items` item text now matches the worksheet's substance-specific
  wording (alcohol items use drink-specific phrasing; nicotine and other-drug
  items corrected; obvious worksheet typos repaired and logged), the
  free-text nicotine quantity item now shows only for non-cigarette,
  non-cigar forms, and `hitophsum_choices` gains the alcohol/cigarette/cigar
  quantity choice sets. In the REDCap export, the cigar quantity item is now
  a valid dropdown (it previously imported with an empty choice list), and
  "Prefer not to say" frequency responses no longer satisfy any symptom
  gate. **New `other_drug_rule` argument** on `generate_redcap_hitophsum()`:
  the default (`"most_frequent"`) follows the worksheet's looping rule —
  symptom items appear only for the most frequently used other drug used at
  least monthly (ties show all tied drugs) — while `"per_drug"` reproduces
  the previous looser behavior of gating every other drug independently.
  The overview DOCX now says "Street opioids" (previously "Heroin/opiates")
  and "Goose bumps", and its item matrix matches the corrected wording; the
  prebuilt DOCX and REDCap files were regenerated
* **The HiTOP-HSUM Qualtrics import file was rebuilt** from the corrected
  item data. The previous file predated the source alignment above and also
  contained an empty cigar-quantity dropdown and a duplicate copy of every
  question. Note one platform difference: Qualtrics display logic cannot
  compare answers across questions, so the Qualtrics survey shows symptom
  items for *every* other drug used at least monthly (the source module's
  sanctioned loosening), whereas the REDCap export defaults to the
  most-frequently-used other drug only

* **New `reliability_pid5()`, `reliability_hitopsr()`, and `reliability_hitopbr()`
  functions** return a per-scale tibble (`scale`, `nItems`, `alpha`, `omega`).
  These replace the `alpha` and `omega` arguments of `score_pid5()`,
  `score_hitopsr()`, and `score_hitopbr()`, which only *printed* a reliability
  table as a side effect and have been **removed**
* **`score_pid5()`, `score_hitopsr()`, and `score_hitopbr()` now take a single
  `missing` argument** in place of the previous `na.rm` (and, for `score_pid5()`,
  `apa_scoring`) arguments. For `score_pid5()`, `missing = "apa"` (the default)
  applies the APA missing-data/proration rule, `"available"` averages the present
  items (the old `apa_scoring = FALSE, na.rm = TRUE`), and `"complete"` returns
  `NA` for any scale with a missing item (the old `na.rm = FALSE`).
  `score_hitopsr()`/`score_hitopbr()` offer `"available"` (default) and
  `"complete"`. Default behavior is unchanged
* **`rank_scales()` gains a `name` argument** (default `"top_scales"`) naming its
  output column, which was previously hard-coded as `"out"`. It also gains
  `reverse` and `srange` arguments: scales named in `reverse` are reflected via
  `sum(srange) - value` before ranking, so a reverse-directioned scale (e.g. a
  well-being scale, where higher = healthier) ranks on the same "higher = more
  elevated" metric as the other scales
* **The `tibble` argument has been removed** from `score_pid5()`,
  `score_hitopsr()`, `score_hitopbr()`, `validity_pid5()`, and `rank_scales()`;
  these functions now always return a tibble
* **Documentation accuracy and polish** across the scoring tutorials and pkgdown
  instrument pages: corrected stale column/dataset names in the HiTOP-SR tutorial
  (leftovers from an earlier "HiTOP-PRO" draft), updated the HiTOP-BR scale count
  (8, not 7) and the PID-5 appended-column count (now includes the 5 domains),
  finished the previously "work in progress" PID-5-BF tutorial, added the missing
  Scale Reliability sections to the HiTOP-BR and PID-5-BF tutorials, fixed a
  mis-targeted REDCap "Import Instructions" link on the PID-5 download page, and
  reconciled the instrument download pages so each describes only the resources it
  actually links
* **Standardized item-text punctuation** in `hitopsr_items` (7 items) and
  `hitopbr_items` (1 item): every item now ends in a period. The affected items
  (HSR 5, 27, 30, 284, 314, 332, 382 and HBR 41) lack the period in the source
  instrument itself, where 398 of 405 HiTOP-SR items have one; the omissions are
  treated as typographical oversights. The derived `*_scales`/`*_subscales`
  tables and the prebuilt DOCX/Qualtrics/REDCap artifacts in `inst/extdata/`
  were regenerated to match

# hitop 0.1.0

* Add initial HiTOP-HSUM functions
* Add data export functions
* Build out phase 1 website
* `score_pid5()` now returns the 5 personality-trait domain scores for the FULL
  and SF versions (APA scoring key Step 3), appended after the 25 facet scores
* Add the `pid_domains` dataset (the domain to primary-facet map used for FULL/SF
  domain scoring)
* `score_pid5()` gains an `apa_scoring` argument (default `TRUE`) that applies the
  published APA missing-data and proration rule: a facet (or BF domain) with more
  than 25% of its items unanswered is set to `NA`; otherwise the raw score is
  prorated to the full item count and rounded before averaging, and a FULL/SF
  domain is `NA` if any contributing facet is `NA`. **This changes the default
  scored output under missing data** (previously `rowMeans(na.rm = TRUE)` averaged
  whatever items were present). Pass `apa_scoring = FALSE` to restore the previous
  behavior. Under `apa_scoring = TRUE`, `na.rm` is ignored (with a warning if set
  to `FALSE`), and any standard error is `NA` wherever its scale score is `NA`
* Fix `validity_pid5()` erroring on single-row input for the FULL and SF forms
* Fix `score_pid5(calc_se = TRUE)` erroring on single-row input
* Add tests for the `generate_docx_*`, `generate_qualtrics_*`, and
  `generate_redcap_*` export families, verifying each generated file against the
  source instrument datasets (including the HiTOP-HSUM REDCap branching logic)
* `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, and `validity_pid5()` now
  guard against two ways a bad `items` mapping silently produces wrong scores:
  they error on duplicated `items` entries and warn when `items` column names
  share a common prefix and trailing number but those numbers are not in
  ascending (instrument) order
* `validity_pid5()` now warns when `srange` is not `c(0, 3)`, because the
  published PRD and SD-TD cut scores are raw sums against fixed thresholds that
  assume 0-3 item coding and do not adapt to other codings
* Add runnable `@examples` to every exported function
* Correct the dataset documentation: fix the column counts in the `pid_items`
  and `hitopbr_items` `@format` blocks, document the `pid_scales` format, and fix
  the `sim_hitopbr` item-column names (`hitopbr_1` to `hitopbr_45`)
* Improve the package Title and Description
* `score_hitopbr()` gains `alpha` and `omega` arguments (default `FALSE`) that
  print a per-scale reliability summary, matching `score_pid5()` and
  `score_hitopsr()`
* Internal refactor: `score_pid5()`, `score_hitopsr()`, and `score_hitopbr()` now
  share a single internal scoring engine instead of three hand-maintained copies
  of the same pipeline (no change to scored output)
* Clearer input errors: `items` of the wrong length now reports the expected
  count and what was supplied, and supplying `items` names or positions that are
  not columns of `data` now raises an actionable error (naming the offenders)
  instead of a cryptic base-R subscript error
* Input-validation errors from the scoring, validity, reliability, and
  `rank_scales()` functions are now attributed to the function you called rather
  than to an internal helper

# hitop 0.0.2

* Add initial HiTOP-SR and BR functions

# hitop 0.0.1

* Add initial PID-5 functions
