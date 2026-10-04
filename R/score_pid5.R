#' Score the Personality Inventory for DSM-5
#'
#' Calculate scale scores on the Personality Inventory for DSM-5: full version
#' (PID-5, 220 items), short form version (PID-5-SF, 100 items), brief form
#' version (PID-5-BF, 25 items), modified brief form (PID5BF+M, 36 items;
#' Bach et al., 2020), Informant Form (PID-5-IRF, 218 items; Markon et al.,
#' 2013), or Forensic Faceted Brief Form (PID-5-FFBF, 100 items; Niemeyer et
#' al., 2022) from item-level data.
#'
#' @param data A data frame containing (at least) all the PID items (numerically
#'   scored and in order).
#' @param items A vector of column names (as strings) or numbers (as integers)
#'   corresponding to the PID items in order. Items must be supplied in
#'   instrument order; a misordered mapping silently scores the wrong items, so a
#'   warning is issued when the names share a common prefix and trailing number
#'   but those numbers are not ascending. Duplicated entries are an error.
#'   Each column must be numeric or logical, or character holding only
#'   numbers (blank cells and `NA` values count as missing, but the text
#'   `"NA"` is refused). A haven labelled column is read as its plain values.
#'   Any other column, such as the choice text of an online export or a
#'   factor, is an error of class `hitop_nonnumeric_items`. So is a 64-bit
#'   integer (`integer64`) column, and, in a UTF-8 session, a column holding
#'   text that is not valid UTF-8 (text marked Latin-1 is read as its text);
#'   convert such text with
#'   `iconv()`. So is an SPSS column (`haven_labelled_spss`) that holds a
#'   value it declares missing; turn those codes into `NA` with
#'   `haven::zap_missing()` first. A declared value that is blank is not
#'   refused, since blank cells count as missing. The SPSS class of haven
#'   before 2.0 (`labelled_spss`) is refused the same way, but
#'   `haven::zap_missing()` leaves it unchanged, so set the values its
#'   `na_values` or `na_range` attribute declares to `NA` first.
#' @param version A string indicating the version of the PID to score: "FULL",
#'   "SF", "BF", "BFPM" (the 36-item PID5BF+M), "IRF" (the 218-item
#'   Informant Form), or "FFBF" (the 100-item Forensic Faceted Brief Form).
#'   Will be automatically capitalized. A unique start of a name is accepted,
#'   but `"F"` is refused, because it starts both "FULL" and "FFBF".
#'   (default = `"FULL"`)
#' @param srange An optional numeric vector specifying the minimum and maximum
#'   values of the items, used for reverse-coding. (default = `c(0, 3)`)
#' @param prefix An optional string to add before each scale column name. If no
#'   prefix is desired, set to an empty string `""`. (default = `"pid_"`)
#' @param missing A string selecting how missing item responses are handled when
#'   computing scale scores. `"apa"` (the default) follows the published APA
#'   scoring key: a facet or domain-item scale with more than 25% of its items
#'   unanswered is set to `NA`, and otherwise the raw score is prorated to the
#'   full item count and rounded to the nearest whole number before averaging (a
#'   FULL, SF, IRF, FFBF or BFPM domain is `NA` if any one of its three contributing
#'   facets is `NA`). The PID-5-IRF key prints this step as "round up to the
#'   nearest whole number". The package reads it as the nearest-whole-number
#'   rule that every other APA PID-5 key states, with halves rounded up, so an
#'   informant facet prorates exactly as the matching self-report facet does.
#'   `"available"` averages whatever items are present (`rowMeans(na.rm = TRUE)`).
#'   `"complete"` returns `NA` for any scale with a missing item
#'   (`rowMeans(na.rm = FALSE)`). With no missing items the three agree. (default
#'   = `"apa"`)
#' @param calc_se **Deprecated.** This argument, and the `_se` columns it
#'   adds, will be removed in a future release; a call with `calc_se = TRUE`
#'   warns; the warning is classed `hitop_deprecated_calc_se`, so a caller can
#'   silence it by name. This package has no interval function for the PID-5, so there is
#'   no replacement for it on this instrument; for measurement precision see
#'   [reliability_pid5()]. What it does while it lasts:
#'   an optional logical indicating whether to calculate a
#'   standard error for each scale score. For the facets, and for the brief
#'   form's domains and total, this is the SD of the items the respondent
#'   actually answered divided by the square root of how many of those items
#'   they answered. The FULL, SF, IRF, FFBF and BFPM domain scores are means of three
#'   facet scores rather than of items, so their standard errors are taken one
#'   level up: the SD of the three contributing facet scores divided by the
#'   square root of 3. Standard errors are `NA` wherever their scale score is
#'   `NA`. A BFPM facet scored from one answered item under
#'   `missing = "available"` has a score but an `NA` standard error, because
#'   the SD of one value is undefined.
#'   Each one summarizes how much a respondent's answers varied within a scale.
#'   It is not a standard error of measurement — no reliability estimate enters
#'   it — so it does not give a confidence interval for a respondent's true
#'   score; for measurement precision see [reliability_pid5()].
#'   (default = `FALSE`)
#' @param append An optional logical indicating whether the new columns should
#'   be added to the end of the `data` input. (default = `TRUE`)
#'
#' @details For the FULL, SF and IRF versions, the output includes the 25 facet
#'   scores followed by the 5 personality-trait domain scores; the FFBF version
#'   adds 2 forensic domains after them (see below). Following the APA
#'   scoring key (Step 3), each domain score is the mean of the average scores of
#'   its 3 primary facets (the map is stored in `pid_domains`). The BF version
#'   scores its 5 domains directly from its items, and adds a `total` score. By
#'   default (`missing = "apa"`) all versions apply the APA missing-data and
#'   proration rule; use `missing = "available"` or `missing = "complete"` for
#'   the traditional `rowMeans()` behaviors. For per-scale reliability estimates
#'   (Cronbach's alpha, McDonald's omega), use [reliability_pid5()].
#'
#' @details ## The PID5BF+M
#'
#'   `version = "BFPM"` scores the PID5BF+M of Bach et al. (2020), a 36-item
#'   form with 6 domains: the 5 PID-5 trait domains and Anankastia. Every item
#'   is a PID-5 item, and none is reverse-keyed. The output
#'   is 18 facets of 2 items each, then 6 domains, in the order of the form's
#'   key: Negative affectivity, Detachment, Antagonism, Disinhibition,
#'   Anankastia and Psychoticism. Each domain score is the mean of its 3 facet
#'   scores (the map is stored in [pid_bfpm_domains]). The 15 facets the form
#'   shares with the PID-5 keep their PID-5 column names. The three Anankastia
#'   facets are `perfectionism`, `rigidity` and `orderliness`. All six of their
#'   items are PID-5 Rigid Perfectionism items, but these facets are not parts
#'   of `rigidPerfectionism`, which this version does not score.
#'
#'   Scores are item means on the 0 to 3 scale, as for the other versions. The
#'   form's published key sums the 2 items of a facet and averages the facet
#'   sums for a domain. On complete data, the key's facet sum is
#'   `2 * pid_<facet>` and its domain score is `2 * pid_<domain>`.
#'
#'   No missing-data rule is published for this form. Under the default
#'   `missing = "apa"`, the 25% rule applied to a 2-item facet means that any
#'   missing item makes the facet `NA`, and an `NA` facet makes its domain
#'   `NA`. So with whole-number responses `"apa"` gives the same output as
#'   `"complete"` here. (The APA rule rounds each scale's sum, so responses
#'   with decimals can differ.) Under `missing = "available"`, a facet can be
#'   scored from one item and a domain from one or two of its facets.
#'
#' @details ## The PID-5 Informant Form
#'
#'   `version = "IRF"` scores the PID-5 Informant Form (PID-5-IRF; Markon et
#'   al., 2013), the APA's 218-item form on which an informant rates the person
#'   receiving care. Its output is the FULL version's: the same 25 facets and 5
#'   domains, with the same column names and domain map ([pid_domains]). The
#'   form has no counterpart to self-report items 96 and 177, so its
#'   Anxiousness facet has 8 items and its Suspiciousness facet 6. Informant
#'   item n is self-report item n through 95, n + 1 through 175, and n + 2
#'   after that; `pid_items$IRF` holds the mapping and `pid_items$TextIRF` the
#'   informant wording.
#'
#'   Fourteen items are reverse-scored: 7, 30, 35, 58, 87, 90, 96, 97, 130,
#'   141, 154, 163, 208 and 213, the items the key's Facet Table marks R. The
#'   key's Step 1 also lists items 98 and 176, which the Facet Table does not
#'   mark and whose wording is not reversed; the package does not reverse them.
#'
#'   The key prints its proration step as "round up to the nearest whole
#'   number". The package applies the nearest-whole-number rule of every other
#'   APA PID-5 key, halves up (see `missing`). The choice matters only under
#'   `missing = "apa"`, for a facet with at least one but no more than 25% of
#'   its items unanswered whose prorated raw score has a fractional part
#'   strictly between 0 and one half. There the
#'   package's facet score is 1/n lower than a ceiling would give, for a facet
#'   of n items.
#'
#'   Informant scores have the full form's column names, and nothing in the
#'   output records the version. Do not pass them to [norm_pid5()] or
#'   [plot_pid5()] as `version = "FULL"`: those use self-report norms, and the
#'   informant norms (Markon et al., 2024, Tables A–10 and A–11) are not in
#'   `pid_norms`. [validity_pid5()] has no informant validity scales.
#'
#' @details ## The PID-5 Forensic Faceted Brief Form
#'
#'   `version = "FFBF"` scores the PID-5 Forensic Faceted Brief Form
#'   (PID-5-FFBF; Niemeyer et al., 2022), an adaptation of the 100-item faceted
#'   short form for people in prison, with a self-report and an informant
#'   version. The form was validated in German. The English text in
#'   [pid_ffbf_items] is the authors' English version, and the study
#'   validated only the German version (p. 40). Items they did not adapt keep
#'   the APA PID-5 wording. Many items are rewritten for prisoners, and the
#'   authors numbered the form afresh, so its items are not the SF's: matched by number, 98 of the
#'   100 SF items fall in a different facet. Facet k, in alphabetical
#'   order, holds items k, k + 25, k + 50 and k + 75. Items 12 and 26 are
#'   reverse-scored.
#'
#'   The output is 25 facets, named and ordered as the SF's, then 7 domains:
#'   the 5 APA domains of [pid_domains] and the two domains of the paper's
#'   four-factor solution that are not APA domains, Disinhibited Aggression
#'   (Emotional Lability, Hostility, Impulsivity) and Insecurity (Separation
#'   Insecurity, Anxiousness, Perceptual Dysregulation). The paper's
#'   four-factor Antagonism and Detachment have the facets of the APA domains
#'   of the same names, so they are not repeated. [pid_ffbf_domains] holds the
#'   map. The four-factor structure comes from one exploratory study of 199
#'   male prisoners, and the authors leave open whether Insecurity is needed
#'   (Niemeyer et al., 2022, p. 39). The seven domains share facets, so they
#'   are not independent scores.
#'
#'   **Missing data.** The paper prints no missing-data rule. Under
#'   `missing = "apa"` the package applies the full-form APA rule, as it does
#'   for the SF: a facet with 1 of its 4 items missing is prorated and
#'   rounded, a facet with 2 or more missing is `NA`, and a domain with an
#'   `NA` facet is `NA`. The authors' published analysis code scores
#'   differently. It takes the unrounded mean of the answered items for a
#'   facet with 1 item missing, and it scores a domain as the mean of its 12
#'   items when up to 3 of them are missing, wherever they fall. With complete
#'   data the two rules agree. With missing items a facet can differ by up to
#'   1/12 (for example 1.75 here against 1.67 for answers 0, 2 and 3), and a
#'   domain can be `NA` here where the code gives a value. The paper's
#'   descriptive statistics were computed under the code's rule. No `missing`
#'   mode reproduces it.
#'
#'   **Informant reports.** Informant data are scored with the same version.
#'   The paper averaged the responses of two informants item by item before
#'   scoring (p. 33), and the authors' code uses one informant's response
#'   where the other is missing; do that step first, for example with
#'   `rowMeans(cbind(x1, x2), na.rm = TRUE)` for each item, then set an item
#'   that both informants skipped (`NaN` from that call) to `NA`. Averaged responses
#'   can be halves, and they score like any other values. Under
#'   `missing = "apa"` the proration step rounds them too: a facet with one
#'   item missing and answers 0.5, 1 and 1 scores 0.75, where
#'   `missing = "available"` gives 0.833. One informant's responses score
#'   directly.
#'
#'   [norm_pid5()], [plot_pid5()] and [validity_pid5()] do not take the FFBF,
#'   because no FFBF norms or validity keys are published. The FFBF facet and
#'   APA domain columns carry the SF's names, so [norm_pid5()] converts them
#'   as `version = "SF"` with no version warning and [plot_pid5()] plots the
#'   result; do not do that, as it compares them with SF self-report norms
#'   from another population.
#'
#' @details ## The PID-5 child forms
#'
#'   `version = "FULL"` scores the APA's 220-item PID-5 child form for ages 11
#'   to 17, and `version = "BF"` scores its 25-item PID-5-BF child form
#'   (Krueger et al., 2013). The child forms print the adult forms' items in
#'   the same order, and their keys reverse the same items and assign them to
#'   the same facets and domains. So no separate version is needed, and data
#'   collected with
#'   [generate_docx_pid5child()], [generate_qualtrics_pid5child()] or
#'   [generate_redcap_pid5child()] (or their `pid5bfchild` counterparts) score
#'   as adult data do. `pid_norms` holds no child norms, so [norm_pid5()] and
#'   [plot_pid5()] would compare child scores with norms from another
#'   population. The child forms print no validity scales, and no source on
#'   this package's shelf gives validity cut scores for them, so the cut
#'   scores [validity_pid5()] applies are not established for child data.
#'
#' @details ## The PID-5-BF total score
#'
#'   `version = "BF"` returns a `total` column after its 5 domains. Markon et al.
#'   (2024, p. 23) define it as the item-level mean over **all 25 items**, not
#'   the mean of the 5 domain means: the total "can be computed by averaging the
#'   overall score by the total number of items in the measure (i.e., 25)". With
#'   five equal-sized domains the two definitions coincide on complete data and
#'   differ only when items are missing, where the published rule above governs.
#'
#'   The total is scored like any other scale, so `missing` applies to it at the
#'   25-item level. Under `missing = "apa"` that means it is `NA` when more than
#'   a quarter of the 25 items are unanswered (7 or more) and prorated otherwise,
#'   independently of the domains. Because a 5-item domain is dropped at 2
#'   unanswered items while the total tolerates 6, **a total can be reported
#'   alongside one or more `NA` domains** (at most 3 of the 5; blanking all five
#'   requires 10 unanswered items, which blanks the total as well). This is the
#'   published rule applied as written, not an oversight.
#'
#'   The FULL, SF, IRF, FFBF and BFPM versions have no total score: the PID-5
#'   book defines one only for the brief form, and the PID5BF+M, PID-5-IRF and
#'   PID-5-FFBF sources define none.
#'
#'   **Errors.** With `append = TRUE`, a column of `data` whose name this call
#'   would also produce is an error rather than an overwrite or a duplicated
#'   column: the message names every colliding column. Re-run with
#'   `append = FALSE` to return only the new columns, or drop the colliding
#'   columns from `data` first. The condition is classed
#'   `hitop_append_collision`, so a caller can catch this refusal by name.
#'
#'   An item column that cannot be read as numbers (see `items`) is an error
#'   of class `hitop_nonnumeric_items`, raised before the collision check.
#'
#' @return A \link[tibble]{tibble} containing all scale scores and standard
#'   errors (if requested) and all original `data` columns (if requested)
#'
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2012). Initial construction of a maladaptive personality
#'   trait model and inventory for DSM-5. *Psychological Medicine, 42*,
#'   1879-1890. \doi{10.1017/s0033291711002674}
#' @references Anderson, J. L., Sellbom, M., & Salekin, R. T. (2016). Utility of
#'   the Personality Inventory for DSM-5-Brief Form (PID-5-BF) in the
#'   measurement of maladaptive personality and psychopathology. *Assessment,
#'   25*(5), 596–607. \doi{10.1177/1073191116676889}
#'
#' @references Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
#'   *Understanding the Personality Inventory for DSM-5 (PID-5).* American
#'   Psychiatric Association Publishing. The source for the PID-5-BF total
#'   score's definition (p. 23) and for the normative tables in `pid_norms`.
#'
#' @references Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W. L.,
#'   Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon, K.
#'   E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
#'   personality disorder trait model can be measured with a reduced set of
#'   items: An item response theory investigation of the personality inventory
#'   for DSM-5. *Psychological Assessment, 27*(4), 1195–1210.
#'   \doi{10.1037/pas0000120}
#'
#' @references Bach, B., Kerber, A., Aluja, A., Bastiaens, T., Keeley, J. W.,
#'   Claes, L., Fossati, A., Gutierrez, F., Oliveira, S. E. S., Pires, R.,
#'   Riegel, K. D., Rolland, J.-P., Roskam, I., Sellbom, M., Somma, A.,
#'   Spanemberg, L., Strus, W., Thimm, J. C., Wright, A. G. C., & Zimmermann, J.
#'   (2020). International assessment of DSM-5 and ICD-11 personality disorder
#'   traits: Toward a common nosology in DSM-5.1. *Psychopathology, 53*(3-4),
#'   179-188. \doi{10.1159/000507589} The source of the PID5BF+M.
#'
#' @references Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F.
#'   (2013). The development and psychometric properties of an informant-report
#'   form of the Personality Inventory for DSM-5 (PID-5). *Assessment, 20*(3),
#'   370-383. \doi{10.1177/1073191113486513}
#' @references Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F.
#'   (2013). *The Personality Inventory for DSM-5—Informant Form
#'   (PID-5-IRF)—Adult*. American Psychiatric Association. The scoring key for
#'   `version = "IRF"`.
#' @references Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D.
#'   (2022). Assessing maladaptive personality in the forensic context:
#'   Development and validation of the Personality Inventory for DSM-5
#'   Forensic Faceted Brief Form (PID-5-FFBF). *Journal of Personality
#'   Assessment, 104*(1), 30-43. \doi{10.1080/00223891.2021.1923522} The
#'   source of the PID-5-FFBF, its items (supplement Table S3) and its domains.
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2013). *The Personality Inventory for DSM-5 (PID-5)—Child
#'   Age 11–17* and *The Personality Inventory for DSM-5—Brief Form
#'   (PID-5-BF)—Child Age 11–17*. American Psychiatric Association. The child
#'   forms that `version = "FULL"` and `version = "BF"` also score.
#'
#' @examples
#' # Score the full PID-5 (25 facets + 5 domains) from the simulated data
#' score_pid5(sim_pid5, items = 1:220, version = "FULL", append = FALSE)
#'
#' # Short form, using the item column names instead of positions
#' score_pid5(sim_pid5sf, items = sprintf("pid5sf_%03d", 1:100), version = "SF",
#'            append = FALSE)
#'
#' # Brief form (5 domains + the total) with standard errors. `calc_se` is
#' # deprecated, so this call warns; the PID-5 has no interval function to
#' # replace it with.
#' score_pid5(sim_pid5bf, items = 1:25, version = "BF", calc_se = TRUE,
#'            append = FALSE)
#'
#' # PID5BF+M (18 facets + 6 domains). No BF+M dataset ships, but every BF+M
#' # item is a PID-5 item, so take its 36 items from the full-form data in
#' # BF+M order.
#' bfpm_rows <- pid_items[!is.na(pid_items$BFPM), ]
#' bfpm_rows <- bfpm_rows[order(bfpm_rows$BFPM), ]
#' sim_bfpm <- sim_pid5[sprintf("pid5_%03d", bfpm_rows$FULL)]
#' score_pid5(sim_bfpm, items = 1:36, version = "BFPM", append = FALSE)
#'
#' # PID-5 Informant Form (25 facets + 5 domains). No informant dataset ships;
#' # for illustration, take the 218 full-form items the informant form maps to.
#' irf_rows <- pid_items[!is.na(pid_items$IRF), ]
#' sim_irf <- sim_pid5[sprintf("pid5_%03d", irf_rows$FULL)]
#' score_pid5(sim_irf, items = 1:218, version = "IRF", append = FALSE)
#'
#' # PID-5-FFBF (25 facets + 7 domains). No FFBF dataset ships; for
#' # illustration, simulate answers to its 100 items.
#' set.seed(1)
#' sim_ffbf <- as.data.frame(matrix(sample(0:3, 500, replace = TRUE), 5, 100))
#' score_pid5(sim_ffbf, items = 1:100, version = "FFBF", append = FALSE)
#'
#' @export
score_pid5 <- function(
  data,
  items,
  version = c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"),
  srange = c(0, 3),
  prefix = "pid_",
  missing = c("apa", "available", "complete"),
  calc_se = FALSE,
  append = TRUE
) {
  ## Resolve the version and its item count (shared arg validation runs in the
  ## engine; version is PID-specific and resolved here)
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"))
  missing <- match.arg(missing)
  n_items <- switch(
    version,
    "FULL" = 220,
    "SF" = 100,
    "BF" = 25,
    "BFPM" = 36,
    "IRF" = 218,
    "FFBF" = 100,
    cli::cli_abort("Invalid `version` argument")
  )

  ## Resolve this version's instrument data: which items reverse, the per-scale
  ## item-number lists, and (FULL/SF/IRF/FFBF/BFPM) the domain -> facet map. The
  ## BFPM domains are the means of their facets, as the FULL/SF domains are, so
  ## the engine scores them the same way from their own map (D-088(c), (d)).
  ## The IRF has the full form's 25 facets and 5 domains (D-089(d)). The FFBF
  ## keeps its items in its own table, and its 7 domains are the 5 APA domains
  ## and 2 forensic domains of `pid_ffbf_domains` (D-092).
  reverse_items <- if (version == "FFBF") {
    pid_ffbf_items$FFBF[pid_ffbf_items$Reverse]
  } else {
    drop_na(pid_items[pid_items$Reverse == TRUE, version, drop = TRUE])
  }
  items_scales <- pid_scales[[version]]$itemNumbers
  domain_map <- if (version %in% c("FULL", "SF", "IRF")) {
    setNames(pid_domains$facetStems, pid_domains$camelCase)
  } else if (version == "BFPM") {
    setNames(pid_bfpm_domains$facetStems, pid_bfpm_domains$camelCase)
  } else if (version == "FFBF") {
    setNames(pid_ffbf_domains$facetStems, pid_ffbf_domains$camelCase)
  } else {
    NULL
  }

  score_engine(
    data = data,
    items = items,
    n_items = n_items,
    reverse_items = reverse_items,
    items_scales = items_scales,
    srange = srange,
    prefix = prefix,
    missing = missing,
    calc_se = calc_se,
    se_instead = paste(
      "This package has no interval function for the PID-5;",
      "for measurement precision see {.fn reliability_pid5}."
    ),
    append = append,
    domain_map = domain_map,
    mask_se_na = TRUE
  )
}
