# hitop (development version)

## New features

* **`rename_pid5_items()` renames PID-5 item columns to the standard names.**
  Given a data frame and the form its items belong to, it renames the item
  columns to `pid5_001`-style names: `method = "number"` (the default) reads the
  item number out of a column already named `pid_1`, `pid_2` and so on, the
  spelling this package's own PID-5 datasets carried before they were renamed to
  match the exports, and `method = "text"` matches the literal item prompts in
  `pid_items$Text` for callers whose data came from elsewhere. Item text, and
  columns spelled like an item number but numbered outside the form, are left
  alone and named in a warning of class `hitop_unmatched_items`; a column that
  looks like no item at all is left alone without comment.

* **`interval_hitopbr()` puts a confidence interval around a HiTOP-BR scale
  score.** Given columns that `score_hitopbr()` produced, it returns an `_est`,
  `_lo` and `_hi` column for each: an estimate of the respondent's true score
  and the bounds of a confidence interval around it, by the regression-based
  approach with scale correction of Schmukle (2026). The reference mean,
  standard deviation and reliability come from the new `hitopbr_devstats`
  dataset.

* **`hitopbr_devstats` ships the HiTOP-BR development-sample statistics.** One
  row per scale, carrying the item count, Cronbach's alpha, mean and standard
  deviation printed for it in the HiTOP-SR introduction paper's Table 1. That
  reference group is the paper's Development Sample 2, N = 780 Prolific Academic
  participants stratified by sex and age to approximate a
  community-representative United States population. It is a development sample
  and not a community norm: no census weighting was applied and the paper
  publishes no raw-score to T-score table, so an interval says where a score
  sits relative to the sample the instrument was developed on, not what
  percentile it occupies in any population.

## Deprecations

* **`calc_se` is deprecated in `score_pid5()`, `score_hitopsr()` and
  `score_hitopbr()`.** Calling any of the three with `calc_se = TRUE` now warns.
  The argument, and the `_se` columns it adds, will be removed in a future
  release; the deprecation adds a warning and moves no value they hold. (The
  HiTOP-BR item-36 rekey under Breaking changes below does move
  `hbr_detachment_se` and `hbr_internalizing_se`.) The number was never
  a standard error of measurement — no reliability estimate entered it — so it
  never gave a confidence interval for a respondent's true score. Use
  `interval_hitopsr()` or `interval_hitopbr()` for that. The PID-5 has no
  interval function in this package; for measurement precision on it, see
  `reliability_pid5()`. The warning carries the condition class
  `hitop_deprecated_calc_se`, so a caller who wants the columns without the
  notice can silence it by class.

## Breaking changes

* **Every item number the package ships is an integer.** `pid_items`' `FULL`,
  `SF`, `BF`, `INC`, `INCS`, `ORS`, `ORSS`, `PRD`, `PRDS`, `SDTD` and `SDTDS`
  columns, `hitopsr_items$HSR`, `hitopbr_items$HBR` and `$HSR`,
  `hitophsum_items$Item`, the `itemNumbers` vectors and the item-number columns
  of the nested `itemdata` frames in `pid_scales`, `hitopsr_scales`,
  `hitopsr_subscales` and `hitopbr_scales`, and the `items` element of what
  `hitop_module()` returns, are all stored as integers where they were doubles.
  No value moved and no exported file changed: only the type differs, so
  subsetting, `==` and arithmetic on these numbers behave as before, integer
  overflow being unreachable at these magnitudes. What changes is
  `identical()`, which distinguishes an integer from a double: a test written as
  `identical(hitopsr_scales$itemNumbers[["agoraphobia"]], c(1, 2, 3))` now fails
  and wants `c(1L, 2L, 3L)` (or `expect_equal()`, which ignores the
  difference). `typeof()`, `is.integer()` and `str()` also report the new type.
  No deprecation period precedes this change.

* **The PID-5 example datasets now use the item column pattern the package's
  own REDCap export writes.** `sim_pid5`'s item columns are now `pid5_001` to
  `pid5_220`; `sim_pid5sf`'s, and the item columns of `ku_pid5sf`, are
  `pid5sf_001` to `pid5sf_100`; `sim_pid5bf`'s are `pid5bf_01` to `pid5bf_25`.
  Only the names changed: every value and column position is as it was, and
  `ku_pid5sf` keeps its `response_id` column. Each form carries its own stem
  because the three forms number their items independently -- short-form item 5
  is full-form item 16, and brief-form item 5 is full-form item 31 -- so one
  shared stem would give one name to different items. The Qualtrics export
  writes the same pattern with an uppercase stem: `PID5_001`, `PID5SF_001`,
  `PID5BF_01`. All three forms previously shared one unpadded stem, so code
  selecting the old names moves form by form: `paste0("pid_", 1:220)` becomes
  `sprintf("pid5_%03d", 1:220)`, `paste0("pid_", 1:100)` becomes
  `sprintf("pid5sf_%03d", 1:100)`, and `paste0("pid_", 1:25)` becomes
  `sprintf("pid5bf_%02d", 1:25)`; the PID-5 vignettes show the new idiom.
  Selecting items by position (`items = 1:220`) is unaffected. The scoring
  functions' `prefix` argument names the *output* score columns and still
  defaults to `"pid_"`, so on this instrument the item columns and the score
  columns carry different stems. No deprecation period precedes this change.

* **The per-scale tables the package ships or returns now join on one column
  shape.** `hitopsr_devstats` and `hitopbr_devstats` name their display-name
  column `Scale`, as `available_scales()`, the `reliability_*()` family and
  every keying table already spelled it; it was `scale`. And
  `reliability_pid5()`, `reliability_hitopsr()` and `reliability_hitopbr()`
  return a `camelCase` column, holding the stem that names the scale's column
  in the matching `score_*()` output, read from the keying table on the same
  row as `Scale`; it sits second, between `Scale` and `nItems`, so `nItems`,
  `alpha` and `omega` each move one position to the right. Code reading
  `hitopsr_devstats$scale` or selecting reliability columns by position must
  migrate; no reliability, interval or reference value moves. No deprecation
  period precedes either change.

* **The HiTOP example datasets and the item-naming helpers now use one item
  column pattern, the one the package's own REDCap export writes** (the
  Qualtrics export writes the same pattern with an uppercase stem, `HSR_001`
  and `HBR_01`, which `prefix = "HSR_"` / `"HBR_"` matches). The item columns of `ku_hitopsr` and `sim_hitopsr` are now
  `hsr_001` to `hsr_405`, and those of `ku_hitopbr` and `sim_hitopbr` are
  `hbr_01` to `hbr_45`; only the names changed, every value and column
  position is as it was. `rename_hitopsr_items()`, `label_hitopsr()` and
  `label_hitopbr()` now zero-pad the item number to the instrument's width
  (three digits for the HiTOP-SR, two for the HiTOP-BR) for every `prefix`,
  and their default `prefix` is `"hsr_"` / `"hbr_"` rather than `"HSR_"` /
  `"HBR_"`. So `rename_hitopsr_items()` writes `hsr_001` where it wrote
  `HSR_1`; `label_*()` with a custom `prefix` against unpadded columns such
  as `HSR_1` no longer labels them (only items where padding makes no
  difference, HiTOP-SR 100 and up and HiTOP-BR 10 and up, still match) and
  warns, under the condition class `hitop_unpadded_items`, naming the
  columns it skipped; and
  `label_hitopsr(x, target = "scales")` with no `prefix` now matches
  `score_hitopsr()`'s default output (likewise for the HiTOP-BR pair). Code
  selecting the old names — `sprintf("hsr%03d", 1:405)` and
  `paste0("hsr_", 1:405)` for the two HiTOP-SR datasets, `sprintf("hbr%02d",
  1:45)` and `paste0("hitopbr_", 1:45)` for the two HiTOP-BR datasets — must
  move to `sprintf("hsr_%03d", 1:405)` and `sprintf("hbr_%02d", 1:45)`; the
  vignettes show the new idiom. No
  deprecation period precedes this change.

* **One HiTOP-BR item moved to the scale its development workbook gives it.**
  Item 36 ("I had a hard time asserting myself to others.") was keyed to
  `Detachment`; the HiTOP Society development workbook this keying was built
  from puts it under `Internalizing`, in its item-to-scale sheet and again in
  its scoring syntax, and the instrument's introduction paper agrees in both its
  descriptive table and its factor table. It was the only HiTOP-BR item where
  the package and the workbook disagreed.

  `score_hitopbr()` and `reliability_hitopbr()` therefore return different
  values for `hbr_detachment` (now 5 items: 7, 12, 30, 31, 37) and
  `hbr_internalizing` (now 8 items: 8, 9, 18, 22, 23, 36, 42, 44). No other
  scale changes, and no item text, item number or response option changes.
  Scores computed with an earlier version are not comparable for those two
  scales. The scoring-key page of the two HiTOP-BR Word forms in `inst/extdata/`
  has been rebuilt with the corrected item lists.

## Improvements and fixes

* **`nItems` is an integer in every shipped per-scale table.** `pid_scales`
  (each of its three elements), `hitopsr_scales`, `hitopsr_subscales` and
  `hitopbr_scales` stored the item count as a double where `available_scales()`,
  `hitop_module()` and the reliability family returned an integer, so
  `identical()` across that boundary was `FALSE` on type alone. The tables are
  rebuilt with an integer count and nothing else in them changed;
  `identical(available_scales()$nItems, hitopsr_scales$nItems)` now holds.

* **Each REDCap export now stages its data dictionary in a directory of its
  own.** Every `generate_redcap_*()` call wrote that dictionary to the same path
  inside the session's temporary directory before packing it, so two exports in
  one session used one file, and an export that failed left the file behind for
  the next call to find. Each call now gets a directory created for it and
  removed whether or not the archive is written.

* **`zip` is now required at version 2.1.0 or later.** That is the first release
  whose `zip()` accepts the archive mode the REDCap exports pass it; an older
  version installed cleanly and then failed at the call.

## Documentation and website

* **Three scoring vignettes gained sections for functions they never
  demonstrated.** *Scoring the PID-5* now ranks each participant's five
  highest-scoring facets with `rank_scales()`, and the HiTOP-SR and HiTOP-BR
  scoring vignettes each show `label_hitopsr()` and `label_hitopbr()` attaching
  the item text to a raw item column and the printed scale name to a scored
  column.

* **The HiTOP-HSUM download page links its file generators.** It gained the
  "Custom File Generation" card the five sibling instrument pages already
  carried, pointing at `generate_docx_hitophsum()` and
  `generate_redcap_hitophsum()`. The Qualtrics survey file for this instrument
  is still a prebuilt download only; the package exports no generator for it.

* **The test suite now sweeps every export against the vignettes.** Each entry
  in `NAMESPACE` must be called in an evaluated chunk of a vignette or article,
  or link there to its own reference page, unless it is a deprecated function.
  A name that appears only in prose, in a comment, or inside an unevaluated
  chunk does not count.

# hitop 0.2.0

This release makes several **breaking** API changes to stabilize the interface
before a CRAN submission.

## Breaking changes

* **Reliability tables now print each scale's canonical name, in a column named
  `Scale`.** `reliability_pid5()`, `reliability_hitopsr()` and
  `reliability_hitopbr()` used to rebuild a display name from the camelCase stem
  that names the scored column, which got nine names wrong. They now read the
  name from the same keying table the questionnaires and `available_scales()`
  print, so the nine change as follows:

  | was | is |
  |---|---|
  | `Distress Dysphoria` | `Distress-Dysphoria` |
  | `Non Persistence` | `Non-persistence` |
  | `Non Planfulness` | `Non-planfulness` |
  | `Non Suicidal Self Injury` | `Non-suicidal Self-injury` |
  | `Sex Related Substance Use` | `Sex-Related Substance Use` |
  | `Well Being` | `Well-being` |
  | `P Factor` | `p-Factor` |
  | `Unusual Beliefs Experiences` | `Unusual Beliefs & Experiences` |
  | `Negative Affectivity` | `Negative affectivity` |

  The first six are HiTOP-SR scales, the seventh is HiTOP-BR, `Unusual Beliefs &
  Experiences` is a PID-5 facet on both the full and short forms, and `Negative
  affectivity` is a PID-5-BF domain. Every HiTOP-SR name `reliability_hitopsr()`
  returns is now a name `hitop_module()` accepts and hands back unchanged;
  `hitop_module()` rejected all six of the old HiTOP-SR spellings.

  The column carrying those names is renamed `scale` to `Scale`, matching
  `available_scales()`. Code selecting `rel$scale` must migrate to `rel$Scale`;
  there is no dual column and no deprecation shim, the same one-release migration
  this package uses for its other renamed output columns. Nothing else about
  these functions' output changed -- the same rows in the same order, and the
  same `nItems`, `alpha` and `omega` values.

  Relatedly, `available_scales()` returns `nItems` as an integer rather than a
  double, so it now matches the `nItems` of `hitop_module()` and of the
  reliability tables. The shipped `hitopsr_scales` and `hitopbr_scales` datasets
  still store `nItems` as a double, so `identical()` between one of those columns
  and `available_scales()$nItems` now returns `FALSE` where it used to return
  `TRUE`; `==` and `dplyr` joins are unaffected. The **snakecase** package is no longer an import; the
  regeneration scripts under `data-raw/` still use it, and say so.

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

* **Scoring and converting now refuse two argument shapes they used to let
  fall through.** Re-running `score_pid5()`, `score_hitopsr()`,
  `score_hitopbr()`, `validity_pid5()`, `norm_pid5()`, `rank_scales()` or
  `interval_hitopsr()` with `append = TRUE` over data that already holds the
  columns that call produces is now an error naming every colliding column,
  where before it reached tibble's duplicated-names complaint, which named
  neither the argument nor the function. Nothing is overwritten: a same-named
  column in your data need not have come from this package, so it is not
  destroyed on your behalf. Pass `append = FALSE` to get only the new columns,
  or drop the colliding columns first. The condition is classed
  `hitop_append_collision`.

  Separately, a `scores` argument to `norm_pid5()` or `interval_hitopsr()`, or
  a `scales` argument to `rank_scales()`, that names no columns is now an error
  rather than a base-R complaint about differing numbers of rows -- and, in
  `rank_scales()`, rather than a report that `top` was out of range "between 1
  and 0", which blamed a consequence of the empty selection for its cause. The
  empty selection is reported ahead of the other selection arguments, so the
  cause named is the empty selection itself; an invalid `data` is still
  reported first. The condition is classed `hitop_empty_selection`. Both
  classes are part of the package's public contract.

  Scoring and converting return exactly what they returned before for every
  call that still succeeds; no arithmetic changed. Two calls that used to
  succeed no longer do: `norm_pid5()` and `interval_hitopsr()` with
  `append = FALSE` and an empty `scores` returned an empty tibble, and now
  raise the empty-selection error along with every other shape of that call.

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

* `rank_scales()`'s `prefix` argument is now matched **literally** (breaking).
  It was previously compiled as a regular expression anchored to the start of
  the column name, which meant a prefix containing `(` failed with a regex
  error and one containing `.` could strip a prefix that was never there. A
  column name that does not begin with exactly `prefix` is now carried through
  whole. Code relying on a regex `prefix` must pre-strip the names instead.
  `norm_pid5()` matches `prefix` the same way.

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

* **The `tibble` argument has been removed** from `score_pid5()`,
  `score_hitopsr()`, `score_hitopbr()`, `validity_pid5()`, and `rank_scales()`;
  these functions now always return a tibble

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

## New features

* **`interval_hitopsr()` puts a confidence interval around a HiTOP-SR scale
  score.** Give it scored columns and it returns three per scale: `_est`, a
  regression-based estimate of the respondent's true score, and `_lo` and `_hi`,
  the bounds of a confidence interval around it at a `level` you choose
  (0.95 by default). The method is the regression approach with scale
  correction from Schmukle (2026, *Assessment*, 33(5), 817-825), Equations (10)
  to (12): the estimate pulls the observed score toward the reference mean,
  because with imperfect measurement a true score tends to sit nearer the mean
  than the observed score does, and the scale correction returns it to the
  metric the observed score is on. The reference mean, standard deviation and
  Cronbach's alpha come from a new exported dataset, `hitopsr_devstats`, which
  carries the statistics printed for each of the 93 HiTOP-SR primary scales and
  subscales in Table 1 of the instrument's introduction paper. **That reference
  group is the paper's Development Sample 2, N = 780: a development sample, and
  not a community norm** -- no census weighting was applied and no raw-score to
  T-score table exists -- so an interval says where a score sits relative to the sample the
  instrument was developed on, not what percentile it occupies in a population.
  Two limits are worth knowing: the interval is the same width for every
  respondent on a scale and is not clipped to the 1-4 response range, so on a
  strongly skewed scale a bound can fall outside it; and the coverage the
  method demonstrates holds across a population of respondents rather than for
  any one of them.

* **A chosen set of an instrument's scales is called a *module*.** The
  entries below describe that feature under its final names. *For development-
  version users only:* these were briefly called `hitop_subset()` and `subset`
  before release. Both still work and warn, every function taking a `module`
  also accepts a descriptor built by `hitop_subset()`, and supplying both
  `module` and `subset` in one call is an error. One further consequence of
  the argument rename: in `score_hitopsr()`, the abbreviation `m = ` is now
  ambiguous between `module` and `missing` and errors — write `mo = ` and
  `mi = `, or spell the arguments out.

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

* **Scoring modules.** `score_hitopsr()` and `reliability_hitopsr()` gain a
  `module` argument taking the same description that `hitop_module()` builds and
  the `generate_*_hitopsr()` functions consume. Give it the item columns you
  actually collected and it scores only that module's scales, returning the same
  values a full 405-item administration would have produced for them. Without
  the argument both functions behave exactly as before. See
  `vignette("hitopsr_scoring")`.

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
  Once both files are on disk, the console names the descriptor it saved, after
  the message naming the form itself; a call that passes no `descriptor` says
  nothing about one.
  `write_module()` now writes an `item_order` attribute as the file's
  `itemOrder` field, so a descriptor read and written again keeps the order it
  recorded.

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

* **The [browser module
  builder](https://jmgirard.github.io/hitop-builder/) shows those definitions
  while you pick.** Pointing at a scale, or reaching its checkbox with the Tab
  key, brings up that scale's definition; Escape dismisses it. The page reads
  the text from the installed package rather than keeping a copy, so a version
  that does not supply it shows the list exactly as before.

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

* **`rank_scales()` gains a `name` argument** (default `"top_scales"`) naming its
  output column, which was previously hard-coded as `"out"`. It also gains
  `reverse` and `srange` arguments: scales named in `reverse` are reflected via
  `sum(srange) - value` before ranking, so a reverse-directioned scale (e.g. a
  well-being scale, where higher = healthier) ranks on the same "higher = more
  elevated" metric as the other scales

## Improvements and fixes

* **Clearer errors for bad arguments.** Every argument check across the package
  now reports which argument was wrong, what was supplied, and which function
  was called, instead of printing the internal test that failed. This affects
  `data`, `prefix`, `name`, `append`, `calc_se`, `alpha`, `omega`, and `top`
  throughout the scoring, reliability, norming, labelling, and ranking
  functions. A bad
  `dir` in `rank_scales()` now lists the permitted values and suggests the
  closest match. No function accepts or rejects anything it did not before —
  only the messages changed.

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

* Qualtrics question IDs are now zero-padded to the width of the largest item
  number rather than the number of items. Output for every full instrument is
  unchanged; the change keeps IDs uniform in a module file.

* **PID-5 Word forms print the response options on two lines.** The response
  scale printed above the items on the PID-5, PID-5-SF, and PID-5-BF Word forms
  now runs across two lines — `0` and `1` on the first, `2` and `3` on the
  second — so that no option phrase is broken partway through by the column
  width. The option values and wording are unchanged, and the HiTOP-SR and
  HiTOP-BR forms keep their single-line scale. All six PID Word files
  (US and A4) were regenerated, with new `hitop_artifacts` entries.

* **The REDCap generators no longer need an external `zip` program.** They
  built the instrument archive by running the system's `zip` command, which
  silently failed wherever no such program was installed -- commonly on
  Windows. The archive is now written by the {zip} package, in R, with no
  outside program involved. The file REDCap receives is unchanged.

* **The jsonlite package moved from Suggests to Imports**, so it is now
  installed with hitop rather than optionally. `write_module()` needs it, and
  the browser module builder runs in an environment where suggested packages
  are not installed.

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

* **Standardized item-text punctuation** in `hitopsr_items` (7 items) and
  `hitopbr_items` (1 item): every item now ends in a period. The affected items
  (HSR 5, 27, 30, 284, 314, 332, 382 and HBR 41) lack the period in the source
  instrument itself, where 398 of 405 HiTOP-SR items have one; the omissions are
  treated as typographical oversights. The derived `*_scales`/`*_subscales`
  tables and the prebuilt DOCX/Qualtrics/REDCap artifacts in `inst/extdata/`
  were regenerated to match

## Documentation and website

* **A new article, [Building HiTOP-SR
  Modules](https://jmgirard.github.io/hitop/articles/modules-hitopsr.html),
  walks the whole module workflow**: choosing scales, describing the module,
  generating the paper, Qualtrics, and REDCap files, selecting the collected
  item columns, and scoring plus reliability. The HiTOP-SR scoring vignette now
  links to it rather than carrying its own shorter copy.

* **The modules article and the three HiTOP-SR generator help pages describe
  the module behavior the generators have.** [Building HiTOP-SR
  Modules](https://jmgirard.github.io/hitop/articles/modules-hitopsr.html) now
  names the `HiTOP-SR Module (v1.0)` header a module Word form carries, and
  `title =` as the way to override it; says that a form built with
  `renumber = FALSE` prints no printed-number crosswalk; and says that
  `include_subscales = TRUE` cannot be combined with `module`. The recipe for
  putting columns collected on a shuffled form back into instrument order,
  `collected[order(item_order)]`, now states -- in the article and in
  `?generate_docx_hitopsr` -- that it applies only to columns that are in the
  order the form printed. And the `descriptor` argument of all three
  `generate_*_hitopsr()` functions now says the descriptor's path is announced
  on the console once both files are written. No behavior changed.

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

* The Qualtrics import instructions note that a browser may save the survey
  file with a `.txt` extension, and that renaming it back to `.qsf` is safe.

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

* **Documentation accuracy and polish** across the scoring tutorials and pkgdown
  instrument pages: corrected stale column/dataset names in the HiTOP-SR tutorial
  (leftovers from an earlier "HiTOP-PRO" draft), updated the HiTOP-BR scale count
  (8, not 7) and the PID-5 appended-column count (now includes the 5 domains),
  finished the previously "work in progress" PID-5-BF tutorial, added the missing
  Scale Reliability sections to the HiTOP-BR and PID-5-BF tutorials, fixed a
  mis-targeted REDCap "Import Instructions" link on the PID-5 download page, and
  reconciled the instrument download pages so each describes only the resources it
  actually links

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
