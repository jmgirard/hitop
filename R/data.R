#' Personality Inventory for DSM-5 Item Data
#'
#' Information about the items in different versions of the PID-5.
#'
#' @format A \link[tibble]{tibble} with 220 rows and 15 columns:
#' \describe{
#'   \item{FULL, SF, BF}{Item number on the full PID-5, PID-5 faceted short form, and PID-5 brief form}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{INC,INCS}{Item number on the response inconsistency scale full and short forms}
#'   \item{ORS,ORSS}{Item number on the overreporting scale full and short forms}
#'   \item{PRD,PRDS}{Item number on the positive impression management response distortion scale full and short forms}
#'   \item{SDTD,SDTDS}{Item number on the social desirability-total denial scale full and short forms}
#'   \item{Facet}{Name of the facet}
#'   \item{Domain}{Name of the domain}
#'   \item{Text}{Item text, copyright APA}
#' }
#' @examples
#' pid_items
"pid_items"

#' Personality Inventory for DSM-5 Scale Data
#'
#' Information about the scales (facets) in different versions of the PID-5,
#' used by `score_pid5()` to map each scale to its item numbers.
#'
#' @format A named \link{list} of length 3 (elements `FULL`, `SF`, and `BF`),
#'   one per PID-5 version. Each element is a \link[tibble]{tibble} with one row
#'   per scale and 5 columns:
#' \describe{
#'   \item{Facet (named `Domain` in the BF element)}{Name of the scale: the
#'   facet for the FULL and SF versions, the domain for the BF version}
#'   \item{itemdata}{A list column containing one item-data tibble per scale}
#'   \item{nItems}{The number of items in the scale}
#'   \item{itemNumbers}{A list column containing one item-number vector per scale}
#'   \item{camelCase}{The name of the scale converted to camel case (the score-output column stem)}
#' }
#' @examples
#' pid_scales[["BF"]]
"pid_scales"

#' Personality Inventory for DSM-5 Domain Data
#'
#' The map from each of the 5 PID-5 personality-trait domains to the 3 facets
#' contributing primarily to it, used to compute domain scores for the FULL and
#' SF versions (APA scoring key Step 3). This is the 15-facet primary subset, not
#' the broader `pid_items$Domain` grouping.
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

#' HiTOP-SR Item Data
#'
#' Information about items in the HiTOP-SR.
#'
#' @format A \link[tibble]{tibble} with 405 rows and 6 columns:
#' \describe{
#'   \item{HSR}{Item number on the full HiTOP-SR}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Scale}{Name of the scale (level 2)}
#'   \item{Subscale}{Name of the subscale (level 1)}
#'   \item{Text}{Item text}
#'   \item{Original}{Item ID in the original, development item pool}
#' }
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
#'   \item{itemdata}{A list column containing one item-data tibble per scale}
#'   \item{nItems}{The number of items in the scale}
#'   \item{itemNumbers}{A list column containing one item-number vector per scale}
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
#'   \item{itemdata}{A list column containing one item-data tibble per subscale}
#'   \item{nItems}{The number of items in the subscale}
#'   \item{itemNumbers}{A list column containing one item-number vector per subscale}
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
#' @format A \link[tibble]{tibble} with 93 rows and 4 columns:
#' \describe{
#'   \item{Scale}{The name of the scale}
#'   \item{Subscale}{The name of the subscale (or NA if not a subscale)}
#'   \item{Brief}{The brief clinician-facing definition (10-20 words)}
#'   \item{Client}{The client-facing definition with examples (30-40 words)}
#' }
#' @examples
#' hitopsr_definitions
"hitopsr_definitions"

#' HiTOP-BR Item Data
#'
#' Information about items in the HiTOP-BR.
#'
#' @format A \link[tibble]{tibble} with 45 rows and 8 columns:
#' \describe{
#'   \item{HBR}{Item number on the HITOP-BR}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Scale}{Name of the scale}
#'   \item{Externalizing}{Whether the item is part of the Externalizing scale}
#'   \item{Pfactor}{Whether the item is part of the p-Factor scale}
#'   \item{Text}{Item text}
#'   \item{HSR}{Item number on the HiTOP-SR}
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
#'   \item{itemdata}{A list column containing one item-data tibble per scale}
#'   \item{nItems}{The number of items in the scale}
#'   \item{itemNumbers}{A list column containing one item-number vector per scale}
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
#'   \item{Item}{Item number}
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
#' @format A \link[tibble]{tibble} with 42 rows and 3 columns:
#' \describe{
#'   \item{Choice_Set}{Name of the choice set}
#'   \item{Value}{Coded response value}
#'   \item{Label}{Response label displayed to respondents}
#' }
#' @examples
#' hitophsum_choices
"hitophsum_choices"

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
#'   \item{hsr_1 to hsr_405}{Responses on each item}
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
#'   \item{hitopbr_1 to hitopbr_45}{Responses on each item}
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
#'   \item{pid_1 to pid_220}{Responses on each item}
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
#'   \item{pid_1 to pid_100}{Responses on each item}
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
#'   \item{pid_1 to pid_25}{Responses on each item}
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
#'   \item{pid_1 to pid_100}{Responses on each item}
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
#'   \item{hbr01 to hbr45}{Responses on each item}
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
#'   \item{hsr001 to hsr405}{Responses on each item}
#' }
#' @examples
#' ku_hitopsr
"ku_hitopsr"
