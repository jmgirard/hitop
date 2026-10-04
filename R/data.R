#' Personality Inventory for DSM-5 Item Data
#'
#' Information about the items in different versions of the PID-5.
#'
#' @format A \link[tibble]{tibble} with 220 rows and 18 columns:
#' \describe{
#'   \item{FULL, SF, BF}{Item number on the full PID-5, PID-5 faceted short form, and PID-5 brief form (integer)}
#'   \item{BFPM}{Item number on the PID5BF+M, the 36-item modified brief form (integer). Its keying is in `pid_scales$BFPM` and [pid_bfpm_domains]}
#'   \item{IRF}{Item number on the PID-5 Informant Form, the 218-item
#'   informant-report form (integer). It is `NA` on self-report items 96 and
#'   177, which the informant form does not have, so IRF item n is self-report
#'   item n, n + 1 or n + 2. Each informant item shares its row's facet and
#'   reverse keying}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{INC,INCS}{Item number on the response inconsistency scale full and short forms (integer)}
#'   \item{ORS,ORSS}{Item number on the overreporting scale full and short forms (integer)}
#'   \item{PRD,PRDS}{Item number on the positive impression management response distortion scale full and short forms (integer)}
#'   \item{SDTD,SDTDS}{Item number on the social desirability-total denial scale full and short forms (integer)}
#'   \item{Facet}{Name of the PID-5 facet. The PID5BF+M regroups six of the
#'   Rigid Perfectionism items into its three anankastia facets, whose item
#'   membership is held only in `pid_scales$BFPM`}
#'   \item{Domain}{Name of the domain}
#'   \item{Text}{Item text, copyright APA}
#'   \item{TextIRF}{Informant Form item text, copyright APA, from Markon, K.
#'   E., Quilty, L. C., Bagby, R. M., & Krueger, R. F. (2013), *The
#'   Personality Inventory for DSM-5—Informant Form (PID-5-IRF)—Adult*,
#'   American Psychiatric Association. Each item completes the stem "He or
#'   she..."; this column does not include the stem, the leading ellipsis or
#'   the final period, and uses straight quotes and apostrophes}
#' }
#' @examples
#' pid_items
"pid_items"

#' Personality Inventory for DSM-5 Scale Data
#'
#' Information about the scales (facets) in different versions of the PID-5,
#' used by `score_pid5()` to map each scale to its item numbers. It is also read
#' by `reliability_pid5()` and by the printed scoring table in
#' `generate_docx_pid5*()`, so adding or removing a row changes all three.
#'
#' @format A named \link{list} of length 6 (elements `FULL`, `SF`, `BF`,
#'   `BFPM`, `IRF`, and `FFBF`), one per PID-5 version. Each element is a
#'   \link[tibble]{tibble} with one row per scale and 5 columns:
#' \describe{
#'   \item{Facet (named `Domain` in the BF element)}{Name of the scale: the
#'   facet for the FULL, SF, BFPM, IRF, and FFBF versions, the domain for the BF
#'   version. The BF element carries a sixth row, `Total`, which is not a
#'   domain but the whole 25-item form scored as one scale (see
#'   [score_pid5()]). The BFPM element holds the 18 facets of the PID5BF+M, 2
#'   items each, grouped by domain in the order of its key; its domains are in
#'   [pid_bfpm_domains]. The IRF element holds the 25 facets of the Informant
#'   Form in the FULL element's order, numbered by informant item, with the
#'   informant text in `itemdata`. The FFBF element holds the 25 facets of the
#'   PID-5 Forensic Faceted Brief Form in the SF element's order, numbered by
#'   FFBF item, with the English self-report text in `itemdata`; its items are
#'   in [pid_ffbf_items] and its domains in [pid_ffbf_domains]}
#'   \item{itemdata}{A list column containing one item-data tibble per scale; its item-number column is an integer}
#'   \item{nItems}{The number of items in the scale (integer)}
#'   \item{itemNumbers}{A list column containing one integer item-number vector per scale}
#'   \item{camelCase}{The name of the scale converted to camel case (the score-output column stem)}
#' }
#' @examples
#' pid_scales[["BF"]]
"pid_scales"

#' Personality Inventory for DSM-5 Domain Data
#'
#' The map from each of the 5 PID-5 personality-trait domains to the 3 facets
#' contributing primarily to it, used to compute domain scores for the FULL, SF
#' and IRF versions (APA scoring keys, Step 3; the PID-5 Informant Form's Domain
#' Table names the same primary facets). This is the 15-facet primary subset,
#' not the broader `pid_items$Domain` grouping.
#'
#' @format A \link[tibble]{tibble} with 5 rows and 4 columns:
#' \describe{
#'   \item{Domain}{Name of the domain (matches `pid_items$Domain`)}
#'   \item{camelCase}{The domain name in camel case (the score-output column stem)}
#'   \item{primaryFacets}{A list column of the 3 primary facet names per domain}
#'   \item{facetStems}{A list column of those 3 facet names in camel case (the facet score-output column stems)}
#' }
#' @examples
#' pid_domains
"pid_domains"

#' PID5BF+M Domain Data
#'
#' The map from each of the 6 domains of the PID5BF+M (the 36-item modified
#' brief form of the PID-5) to its 3 facets, used to compute the domain scores
#' of `score_pid5(version = "BFPM")` and the domain rows of
#' `reliability_pid5(version = "BFPM")`. Each domain score is the mean of its 3
#' facet scores (Bach et al., 2020). The rows are in the order of the form's
#' key, with Anankastia fifth.
#'
#' @format A \link[tibble]{tibble} with 6 rows and 4 columns, the columns of
#'   [pid_domains]:
#' \describe{
#'   \item{Domain}{Name of the domain. The five domains the form shares with
#'   the PID-5 are spelled as in [pid_domains]}
#'   \item{camelCase}{The domain name in camel case (the score-output column stem)}
#'   \item{primaryFacets}{A list column of the 3 facet names per domain, as
#'   `pid_scales$BFPM$Facet` spells them}
#'   \item{facetStems}{A list column of those 3 facet names in camel case (the facet score-output column stems)}
#' }
#' @source Bach, B., Kerber, A., Aluja, A., Bastiaens, T., Keeley, J. W.,
#'   Claes, L., Fossati, A., Gutierrez, F., Oliveira, S. E. S., Pires, R.,
#'   Riegel, K. D., Rolland, J.-P., Roskam, I., Sellbom, M., Somma, A.,
#'   Spanemberg, L., Strus, W., Thimm, J. C., Wright, A. G. C., & Zimmermann, J.
#'   (2020). International assessment of DSM-5 and ICD-11 personality disorder
#'   traits: Toward a common nosology in DSM-5.1. *Psychopathology, 53*(3-4),
#'   179-188. \doi{10.1159/000507589}
#'
#'   The keying itself is transcribed from the form's coding scheme: Kerber,
#'   A. (2020). *Persönlichkeitsinventar für DSM-5 und ICD-11: Kurzform
#'   Modifiziert (PID5BF+ M)* \[Questionnaire and coding scheme, German\].
#'   Freie Universität Berlin, p. 2.
#' @examples
#' pid_bfpm_domains
"pid_bfpm_domains"

#' PID-5 Forensic Faceted Brief Form Item Data
#'
#' The 100 items of the PID-5 Forensic Faceted Brief Form (PID-5-FFBF), an
#' adaptation of the PID-5 faceted short form for people in prison, with self-
#' and informant-report versions in English and German. Most items are
#' rewritten for the prison setting, so they have their own table rather than
#' columns of [pid_items]. The form was validated in German. Its English text
#' is the authors' English version, from Table S3, and the study validated
#' only the German version (Niemeyer et al., 2022, p. 40). Items they did not
#' adapt, and items they replaced with an item of the 220-item PID-5, take
#' their wording from the APA PID-5. Scored by `score_pid5(version = "FFBF")`.
#'
#' The texts are those of Table S3 of the form's supplement, without its
#' source notes, its reverse marks, the stray markers E14, E18 and E77, a
#' leading ellipsis, or the final period, and with straight quotes and
#' apostrophes (including an acute accent used as an apostrophe in item 50).
#' Hyphens inside German words are removed, and English words split at a
#' hyphen across a line are joined with the hyphen kept. The printed wording is otherwise
#' kept, typos included.
#'
#' @format A \link[tibble]{tibble} with 100 rows and 7 columns:
#' \describe{
#'   \item{FFBF}{Item number on the PID-5-FFBF (integer)}
#'   \item{Facet}{Name of the PID-5 facet, spelled as in `pid_items$Facet`. Each
#'   facet has 4 items: facet k, in alphabetical order, holds items k, k + 25,
#'   k + 50, and k + 75}
#'   \item{Reverse}{Whether the item needs to be reverse scored (items 12 and
#'   26)}
#'   \item{Text}{Self-report item text, English}
#'   \item{TextIRF}{Informant-report item text, English, without a subject
#'   (Table S3 prints each with a leading ellipsis)}
#'   \item{TextDE}{Self-report item text, German}
#'   \item{TextIRFDE}{Informant-report item text, German, without a subject}
#' }
#' @source Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D.
#'   (2022). Assessing maladaptive personality in the forensic context:
#'   Development and validation of the Personality Inventory for DSM-5
#'   Forensic Faceted Brief Form (PID-5-FFBF). *Journal of Personality
#'   Assessment, 104*(1), 30-43. \doi{10.1080/00223891.2021.1923522}. Item
#'   text from its supplement, Table S3 (<https://osf.io/fzyvr/>): Copyright
#'   2013 American Psychiatric Association, German version 2015, developed
#'   with permission from Hogrefe and the APA.
#' @examples
#' pid_ffbf_items
"pid_ffbf_items"

#' PID-5 Forensic Faceted Brief Form Domain Data
#'
#' The map from each of the 7 domains that `score_pid5(version = "FFBF")`
#' returns to its 3 facets. The first 5 rows are the APA domains of
#' [pid_domains], each the mean of its 3 primary facets (Niemeyer et al.,
#' 2022, p. 33). The last 2 rows are the domains of the paper's four-factor
#' solution that are not APA domains, Disinhibited Aggression and Insecurity,
#' with the facets of the authors' analysis code. The paper's four-factor
#' Antagonism and Detachment have the facets of the APA domains of the same
#' names, so they are not repeated.
#'
#' @format A \link[tibble]{tibble} with 7 rows and 4 columns, the columns of
#'   [pid_domains]:
#' \describe{
#'   \item{Domain}{Name of the domain. The APA domains are spelled as in
#'   [pid_domains] and the two forensic domains as the paper prints them}
#'   \item{camelCase}{The domain name in camel case (the score-output column stem)}
#'   \item{primaryFacets}{A list column of the 3 facet names per domain}
#'   \item{facetStems}{A list column of those 3 facet names in camel case (the facet score-output column stems)}
#' }
#' @source Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D.
#'   (2022). *Journal of Personality Assessment, 104*(1), 30-43.
#'   \doi{10.1080/00223891.2021.1923522}. The four-factor domain facets are
#'   from the authors' analysis code on the paper's OSF project
#'   (<https://osf.io/m42gn/>).
#' @examples
#' pid_ffbf_domains
"pid_ffbf_domains"

#' Personality Inventory for DSM-5 Normative Tables
#'
#' Published normative score distributions for the PID-5, PID-5-SF, and
#' PID-5-BF, in long form: the raw score and percentile at each T score for the
#' five domain scales, the 25 facet (trait) scales of the full and short forms,
#' and the brief form's total score; and the percentile at each raw score for
#' the validity scales, which are tabled without T scores.
#'
#' @format A \link[tibble]{tibble} with 4606 rows and 5 columns:
#' \describe{
#'   \item{version}{The PID-5 version the row norms: `"FULL"`, `"SF"`, or `"BF"`}
#'   \item{scale}{Name of the scale, as the score-output column stem used by
#'   `score_pid5()` and `validity_pid5()` (i.e., without their `prefix`), so a
#'   lookup joins to scored output with no crosswalk. Every scale normed here is
#'   produced by one of those two functions, the brief form's `"total"`
#'   included (see [score_pid5()])}
#'   \item{tscore}{The T score, or `NA` for the validity scales, whose tables
#'   print none}
#'   \item{raw}{The raw scale score, on the metric `score_pid5()` and
#'   `validity_pid5()` return: for the FULL and SF domains, the mean of the
#'   three primary facet scores (themselves item means, and the facets differ
#'   in length, so this is not a mean over the domain's items); for the FULL and
#'   SF facets and for the BF domains and total, a mean item response; for the
#'   validity scales, an item sum. 42 of the 50 facet columns print raws above
#'   the 3.00 a mean of 0-3 items can reach, up to 4.00, and 19 of those repeat
#'   their top raw across consecutive T scores; all such rows ship as published
#'   and are simply unattainable (see [norm_pid5()])}
#'   \item{percentile}{The percentile of the normative distribution at that
#'   score, as a proportion between 0 and 1}
#' }
#' @details The `INC` and `INCS` scales are called the Variable Response
#'   Inconsistency (VRIN) scale by Markon et al. (2024), so a reader coming from
#'   the book will find those tables here under the package's own names.
#'
#'   Markon et al. call the 25 facets *trait scales*; they are tabled under the
#'   book's own captions, which the package maps onto the [pid_scales] facet
#'   names.
#'
#'   Norms come from a sample of 1,082 individuals from a U.S. Census-matched
#'   panel. The validity-scale distributions use all 1,082; the FULL and SF
#'   domain and facet distributions use the 995 respondents who scored below 17
#'   on the inconsistency scale, left no more than a quarter of responses
#'   missing, and did not endorse both infrequency items. The source states no
#'   separate sample size for the brief form tables. All T scores and
#'   percentiles were computed with sampling weights reflecting U.S. Census
#'   data.
#'
#'   The published informant-form tables are not included.
#'
#' @source Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
#'   *Understanding the Personality Inventory for DSM-5 (PID-5).* American
#'   Psychiatric Association Publishing. Appendix, Tables A-1 to A-9
#'   (pp. 113-219).
#' @examples
#' pid_norms
"pid_norms"

#' HiTOP-SR Item Data
#'
#' Information about items in the HiTOP-SR.
#'
#' @format A \link[tibble]{tibble} with 405 rows and 6 columns:
#' \describe{
#'   \item{HSR}{Item number on the full HiTOP-SR (integer)}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Scale}{Name of the scale (level 2)}
#'   \item{Subscale}{Name of the subscale (level 1)}
#'   \item{Text}{Item text}
#'   \item{Original}{Item ID in the original, development item pool}
#' }
#' @details
#' Two scales carry other names in the literature and in earlier versions of
#' this package. The scale this table calls `Non-suicidal Self-injury` is
#' widely written by its abbreviation, **NSSI**, and was named that way here
#' before version 0.2.0; the scale it calls `Appearance Focus` was named **Body
#' Focus** here before version 0.2.0. Both names are the ones printed in the
#' HiTOP-SR introduction paper's Table 1. Scoring functions derive a column
#' name from each scale name, so those scales' scored columns are
#' `hsr_nonSuicidalSelfInjury` and `hsr_appearanceFocus`.
#' @examples
#' hitopsr_items
"hitopsr_items"

#' HiTOP-SR Scale Data
#'
#' Information about scales in the HiTOP-SR.
#'
#' @format A \link[tibble]{tibble} with 76 rows and 5 columns:
#' \describe{
#'   \item{Scale}{Name of the scale}
#'   \item{itemdata}{A list column containing one item-data tibble per scale; its item-number column is an integer}
#'   \item{nItems}{The number of items in the scale (integer)}
#'   \item{itemNumbers}{A list column containing one integer item-number vector per scale}
#'   \item{camelCase}{The name of the scale converted to camel case}
#' }
#' @examples
#' hitopsr_scales
"hitopsr_scales"

#' HiTOP-SR Subscale Data
#'
#' Information about subscales in the HiTOP-SR.
#'
#' @format A \link[tibble]{tibble} with 17 rows and 6 columns:
#' \describe{
#'   \item{Subscale}{Name of the subscale}
#'   \item{Scale}{Name of the scale that the subscale is part of}
#'   \item{itemdata}{A list column containing one item-data tibble per subscale; its item-number column is an integer}
#'   \item{nItems}{The number of items in the subscale (integer)}
#'   \item{itemNumbers}{A list column containing one integer item-number vector per subscale}
#'   \item{camelCase}{The name of the subscale converted to camel case}
#' }
#' @examples
#' hitopsr_subscales
"hitopsr_subscales"

#' HiTOP-SR Definitions
#'
#' Brief clinician and client-facing definitions of each scale and subscale in
#' the HiTOP-SR
#'
#' @format A \link[tibble]{tibble} with 93 rows and 5 columns:
#' \describe{
#'   \item{Scale}{The name of the scale}
#'   \item{Subscale}{The name of the subscale (or NA if not a subscale)}
#'   \item{Brief}{The brief clinician-facing definition (10-20 words)}
#'   \item{Client}{The client-facing definition with examples (30-40 words)}
#'   \item{camelCase}{The camel case name of whatever the row defines: the
#'     subscale where there is one, otherwise the scale. Matches
#'     \link{hitopsr_scales}$camelCase on the scale rows and
#'     \link{hitopsr_subscales}$camelCase on the subscale rows.}
#' }
#' @examples
#' hitopsr_definitions
"hitopsr_definitions"

#' HiTOP-SR Development-Sample Statistics
#'
#' Descriptive statistics for each HiTOP-SR primary scale and subscale, as
#' printed in Table 1 of the HiTOP-SR introduction paper. The reference group is
#' that paper's **Development Sample 2**, N = 780 Prolific Academic participants
#' stratified by sex and age to approximate a community-representative United
#' States population. It is a development sample, not a community norm: no
#' weighting to a census frame was applied and the paper publishes no raw-score
#' to T-score table. Read a score against these statistics as a comparison with
#' the sample the instrument was developed on.
#'
#' Every statistic is a printed cell of that table, transcribed and verified
#' against it; nothing here is computed from data by this package. The `mean` and
#' `sd` are on the HiTOP-SR's own four-option 1-4 response coding, and scale
#' scores are item means, so a score computed on another coding is not comparable
#' to them. [interval_hitopsr()] reads this table.
#'
#' @format A \link[tibble]{tibble} with 93 rows and 8 columns:
#' \describe{
#'   \item{Scale}{The name of the scale or subscale. Matches
#'     \link{hitopsr_scales}$Scale on the scale rows and
#'     \link{hitopsr_subscales}$Subscale on the subscale rows.}
#'   \item{camelCase}{That name converted to camel case -- the stem
#'     [score_hitopsr()] appends to its `prefix` when it names a score column}
#'   \item{type}{Either `"scale"` (76 rows) or `"subscale"` (17 rows)}
#'   \item{nItems}{The number of items in the scale or subscale (integer)}
#'   \item{reliability}{The internal-consistency reliability coefficient printed
#'     for that scale}
#'   \item{reliabilityType}{What that coefficient is. `"alpha"` throughout:
#'     Cronbach's alpha is what the paper prints. Supplied by this package, not
#'     read from the table.}
#'   \item{mean}{The scale score's mean in the development sample}
#'   \item{sd}{The scale score's standard deviation in the development sample}
#' }
#' @examples
#' hitopsr_devstats
"hitopsr_devstats"

#' HiTOP-BR Development-Sample Statistics
#'
#' Descriptive statistics for each HiTOP-BR scale, as printed in the
#' "Superspectra and Spectra Scales" block of Table 1 of the HiTOP-SR
#' introduction paper. The reference group is that paper's **Development Sample
#' 2**, N = 780 Prolific Academic participants stratified by sex and age to
#' approximate a community-representative United States population. It is a
#' development sample, not a community norm: no weighting to a census frame was
#' applied and the paper publishes no raw-score to T-score table. Read a score
#' against these statistics as a comparison with the sample the instrument was
#' developed on.
#'
#' Every statistic is a printed cell of that table, transcribed and verified
#' against it; nothing here is computed from data by this package. The `mean` and
#' `sd` are on the HiTOP-BR's own four-option 1-4 response coding, and scale
#' scores are item means, so a score computed on another coding is not comparable
#' to them. [interval_hitopbr()] reads this table.
#'
#' The HiTOP-BR scales were developed independently of the HiTOP-SR primary
#' scales, drawing on the same item pool, and are not a short form of them
#' (Table 1's Note), so these statistics are not comparable with
#' [hitopsr_devstats].
#'
#' @section Item counts: Table 1's printed `# Items` agrees with the item count
#'   [hitopbr_scales] derives from [hitopbr_items] for all eight scales. It did
#'   not always: item 36 ("I had a hard time asserting myself to others.") was
#'   keyed to `Detachment` in this package until it was corrected to
#'   `Internalizing`, the scale the instrument's development workbook gives it in
#'   both its item-to-scale sheet and its scoring syntax, and the scale the
#'   paper's own factor table loads it on. `Detachment` therefore has 5 items and
#'   `Internalizing` 8, which is what Table 1 prints for each.
#'
#' @format A \link[tibble]{tibble} with 8 rows and 8 columns:
#' \describe{
#'   \item{Scale}{The name of the scale. Matches \link{hitopbr_scales}$Scale.}
#'   \item{camelCase}{That name converted to camel case -- the stem
#'     [score_hitopbr()] appends to its `prefix` when it names a score column}
#'   \item{type}{`"scale"` throughout. Table 1 prints all eight rows under one
#'     heading and labels none of them a superspectrum or a spectrum, so no such
#'     distinction is recorded here.}
#'   \item{nItems}{The number of items in the scale (integer)}
#'   \item{reliability}{The internal-consistency reliability coefficient printed
#'     for that scale}
#'   \item{reliabilityType}{What that coefficient is. `"alpha"` throughout:
#'     Cronbach's alpha is what the paper prints. Supplied by this package, not
#'     read from the table.}
#'   \item{mean}{The scale score's mean in the development sample}
#'   \item{sd}{The scale score's standard deviation in the development sample}
#' }
#' @examples
#' hitopbr_devstats
"hitopbr_devstats"

#' HiTOP-BR Item Data
#'
#' Information about items in the HiTOP-BR.
#'
#' @format A \link[tibble]{tibble} with 45 rows and 8 columns:
#' \describe{
#'   \item{HBR}{Item number on the HITOP-BR (integer)}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Scale}{Name of the scale}
#'   \item{Externalizing}{Whether the item is part of the Externalizing scale}
#'   \item{Pfactor}{Whether the item is part of the p-Factor scale}
#'   \item{Text}{Item text}
#'   \item{HSR}{Item number on the HiTOP-SR (integer)}
#'   \item{Original}{Item ID in the original, development item pool}
#' }
#' @examples
#' hitopbr_items
"hitopbr_items"

#' HiTOP-BR Scale Data
#'
#' Information about scales in the HiTOP-BR.
#'
#' @format A \link[tibble]{tibble} with 8 rows and 5 columns:
#' \describe{
#'   \item{Scale}{Name of the scale}
#'   \item{itemdata}{A list column containing one item-data tibble per scale; its two item-number columns are integers}
#'   \item{nItems}{The number of items in the scale (integer)}
#'   \item{itemNumbers}{A list column containing one integer item-number vector per scale}
#'   \item{camelCase}{The name of the scale converted to camel case}
#' }
#' @examples
#' hitopbr_scales
"hitopbr_scales"

#' HiTOP-HSUM Item Data
#'
#' Information about the items in the HiTOP-HSUM (Harmful Substance Use Measure).
#' Used by the HiTOP-HSUM instrument generators (e.g. `generate_redcap_hitophsum()`).
#'
#' @format A \link[tibble]{tibble} with 650 rows and 9 columns:
#' \describe{
#'   \item{Item}{Item number (integer)}
#'   \item{Variable}{Variable name for the item}
#'   \item{Substance}{Name of the substance the item refers to}
#'   \item{Tier}{The assessment tier the item belongs to (e.g. Screening)}
#'   \item{Field_Type}{The response field type (e.g. radio)}
#'   \item{Gate_Variable}{Name of the gating variable, or NA if ungated}
#'   \item{Gate_Value}{Value of the gating variable required to show the item, or NA}
#'   \item{Choice_Set}{Name of the response choice set (see `hitophsum_choices`)}
#'   \item{Text}{Item text}
#' }
#' @examples
#' hitophsum_items
"hitophsum_items"

#' HiTOP-HSUM Choice Sets
#'
#' Response choice sets referenced by `hitophsum_items$Choice_Set`. Used by the
#' HiTOP-HSUM instrument generators (e.g. `generate_redcap_hitophsum()`).
#'
#' @format A \link[tibble]{tibble} with 185 rows and 3 columns:
#' \describe{
#'   \item{Choice_Set}{Name of the choice set}
#'   \item{Value}{Coded response value}
#'   \item{Label}{Response label displayed to respondents}
#' }
#' @examples
#' hitophsum_choices
"hitophsum_choices"

#' HiTOP-DAT Item Data
#'
#' The items of the HiTOP-DAT (HiTOP Digital Assessment and Tracker), a battery
#' of seven measures: the WHODAS (12 items), the IDAS-II (99), the AUDIT (10),
#' the DUDIT (11), the positive items of the CAPE (20), the CAT-PD static form
#' (216) and the PHQ-15 (14). The battery numbers run from 1 to 382
#' in the order the battery gives the measures, which is the order listed here.
#'
#' The item text is taken from the battery's Qualtrics file, with its markup
#' removed and line breaks within an item made spaces. CAT-PD item 194, cut
#' short in the file, is completed from the IPIP key. The file moves the
#' PHQ-15's item 4 (menstrual problems) out of the battery, so the PHQ-15 has
#' 14 items here and its own numbers skip 4. The
#' battery gives only the CAPE's positive items, and they keep their CAPE
#' numbers (2 to 42, with gaps). The battery has no scoring function in this
#' package yet.
#'
#' @format A \link[tibble]{tibble} with 382 rows and 5 columns:
#' \describe{
#'   \item{Item}{The item's number in the battery, 1 to 382 (integer)}
#'   \item{Measure}{The measure the item belongs to}
#'   \item{MeasureItem}{The item's number in its own measure (integer)}
#'   \item{Text}{Item text}
#'   \item{Choice_Set}{Name of the item's answer set (see [hitopdat_choices])}
#' }
#' @seealso [hitopdat_choices], [hitopdat_scales]
#' @keywords internal
#' @examples
#' hitopdat_items
"hitopdat_items"

#' HiTOP-DAT Answer Sets
#'
#' The answer sets referenced by `hitopdat_items$Choice_Set`, one row per
#' answer. `Value` is the value the battery's Qualtrics file gives the answer
#' when it scores an item in the forward direction. The file also offers a
#' "Skip" answer on every item, which is not included.
#'
#' @format A \link[tibble]{tibble} with 58 rows and 3 columns:
#' \describe{
#'   \item{Choice_Set}{Name of the answer set}
#'   \item{Value}{Coded response value (integer)}
#'   \item{Label}{Response label displayed to respondents}
#' }
#' @seealso [hitopdat_items]
#' @keywords internal
#' @examples
#' hitopdat_choices
"hitopdat_choices"

#' HiTOP-DAT Scale Data
#'
#' The scales the HiTOP-DAT scores, one row per scale: the 19 IDAS-II scales,
#' the 33 CAT-PD facets, and one total each for the WHODAS, AUDIT, DUDIT, CAPE
#' positive items and PHQ-15. `Scale` is the name the HiTOP-DAT manual (2021)
#' gives the scale. Item numbers are battery numbers, as in
#' `hitopdat_items$Item`.
#'
#' Scale membership and reverse keying come from the scoring of the battery's
#' Qualtrics file. The CAT-PD facets are checked against the IPIP CAT-PD-SF
#' v1.1 key, and the IDAS-II scales against the IDAS-II scoring key (Watson,
#' 2011). An item can be reversed in one scale and not in another: the
#' IDAS-II's General Depression reverses two Well-Being items that Well-Being
#' scores forward.
#'
#' @format A \link[tibble]{tibble} with 57 rows and 6 columns:
#' \describe{
#'   \item{Measure}{The measure the scale belongs to, as in
#'     `hitopdat_items$Measure`}
#'   \item{Scale}{Name of the scale}
#'   \item{camelCase}{The name of the scale converted to camel case}
#'   \item{itemNumbers}{A list column containing one integer item-number vector
#'     per scale}
#'   \item{reverseNumbers}{A list column containing, per scale, the integer
#'     numbers of the items that scale reverses (empty when it reverses none)}
#'   \item{nItems}{The number of items in the scale (integer)}
#' }
#' @seealso [hitopdat_items]
#' @keywords internal
#' @examples
#' hitopdat_scales
"hitopdat_scales"

#' Distribution Artifact Manifest
#'
#' Version manifest for the prebuilt instrument artifacts. They ship in the
#' package at `inst/extdata/` and are distributed from the package website's
#' download pages, which serve their own byte-identical copy. Each build of
#' an artifact adds a row (the full history is kept), so the latest row per
#' `file` describes the currently distributed file. Artifact revisions are
#' identified by build date; the instrument version (e.g., `"1.0"`) is the
#' version of the instrument itself and changes only when its publisher
#' revises it. To check which build you have, compare your downloaded file's
#' MD5 checksum (e.g., `tools::md5sum()`) against the `md5` column.
#'
#' @format A \link[tibble]{tibble} with one row per artifact build and 7
#'   columns:
#' \describe{
#'   \item{file}{Artifact file name, the same in `inst/extdata/` and on the
#'     website's download pages}
#'   \item{instrument}{Instrument the artifact administers}
#'   \item{format}{Artifact format: `"docx_us"`, `"docx_a4"`, `"qualtrics"`,
#'     `"redcap"`, or `"json"`}
#'   \item{instrument_version}{Version of the instrument itself}
#'   \item{build_date}{Date this build of the artifact was generated}
#'   \item{md5}{MD5 checksum of the built file}
#'   \item{changes}{What changed in this build}
#' }
#' @examples
#' hitop_artifacts
"hitop_artifacts"

#' Simulated HiTOP-SR Data
#'
#' Simulated responses to items on the full HiTOP-SR (with 405 items). Note that
#' this is a naive simulation where response options 1 to 4 are all equally
#' likely and generated independently per item. Thus, responses are not
#' clustered within scales, and these data can be used (eventually) to test
#' validity tools intended to detect inconsistent/random responding.
#'
#' @format A \link[tibble]{tibble} with 100 rows and 405 columns.
#' \describe{
#'   \item{hsr_001 to hsr_405}{Responses on each item}
#' }
#' @examples
#' sim_hitopsr
"sim_hitopsr"

#' Simulated HiTOP-BR Data
#'
#' Simulated responses to items on the HiTOP-BR (with 45 items). Note that
#' this is a naive simulation where response options 1 to 4 are all equally
#' likely and generated independently per item. Thus, responses are not
#' clustered within scales, and these data can be used (eventually) to test
#' validity tools intended to detect inconsistent/random responding.
#'
#' @format A \link[tibble]{tibble} with 100 rows and 45 columns.
#' \describe{
#'   \item{hbr_01 to hbr_45}{Responses on each item}
#' }
#' @examples
#' sim_hitopbr
"sim_hitopbr"

#' Simulated PID-5 Data
#'
#' Simulated responses to items on the full PID-5 (with 220 items).
#'
#' @format A \link[tibble]{tibble} with 100 rows and 220 columns.
#' \describe{
#'   \item{pid5_001 to pid5_220}{Responses on each item}
#' }
#' @examples
#' sim_pid5
"sim_pid5"

#' Simulated PID-5-SF Data
#'
#' Simulated responses to items on the PID-5-SF (with 100 items).
#'
#' @format A \link[tibble]{tibble} with 100 rows and 100 columns.
#' \describe{
#'   \item{pid5sf_001 to pid5sf_100}{Responses on each item}
#' }
#' @examples
#' sim_pid5sf
"sim_pid5sf"

#' Simulated PID-5-BF Data
#'
#' Simulated responses to items on the PID-5-BF (with 25 items).
#'
#' @format A \link[tibble]{tibble} with 100 rows and 25 columns.
#' \describe{
#'   \item{pid5bf_01 to pid5bf_25}{Responses on each item}
#' }
#' @examples
#' sim_pid5bf
"sim_pid5bf"

#' Real PID-5-SF Data
#'
#' Real responses to items on the PID-5-SF (with 100 items) from University of
#' Kansas students.
#'
#' @format A \link[tibble]{tibble} with 386 rows and 101 columns.
#' \describe{
#'   \item{response_id}{An anonymized id for each participant}
#'   \item{pid5sf_001 to pid5sf_100}{Responses on each item}
#' }
#' @examples
#' ku_pid5sf
"ku_pid5sf"

#' Real HiTOP-BR Data
#'
#' Real responses to items on the HiTOP-BR from University of Kansas students.
#'
#' @format A \link[tibble]{tibble} with 411 rows and 47 columns.
#' \describe{
#'   \item{participant}{An anonymized id for each participant}
#'   \item{biosex}{A factor indicating each participant's biological sex}
#'   \item{hbr_01 to hbr_45}{Responses on each item}
#' }
#' @examples
#' ku_hitopbr
"ku_hitopbr"

#' Real HiTOP-SR Data
#'
#' Real responses to items on the HiTOP-SR from University of Kansas students.
#'
#' @format A \link[tibble]{tibble} with 411 rows and 407 columns.
#' \describe{
#'   \item{participant}{An anonymized id for each participant}
#'   \item{biosex}{A factor indicating each participant's biological sex}
#'   \item{hsr_001 to hsr_405}{Responses on each item}
#' }
#' @examples
#' ku_hitopsr
"ku_hitopsr"
