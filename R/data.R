#' Personality Inventory for DSM-5 Item Data
#'
#' Information about the items in different versions of the PID-5.
#'
#' @format ## `pid_items` A \link[tibble]{tibble} with 220 rows and 12 columns:
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
"pid_items"

#' Personality Inventory for DSM-5 Scale Data
#'
#' Information about the scales in different versions of the PID-5.
"pid_scales"

#' HiTOP-SR Item Data
#'
#' Information about items in the HiTOP-SR.
#'
#' @format A \link[tibble]{tibble} with 405 rows and 8 columns:
#' \describe{
#'   \item{HSR}{Item number on the full HiTOP-SR}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Spectrum}{Name of the spectrum (level 4)}
#'   \item{Subfactor}{Name of the subfactor (level 3)}
#'   \item{Scale}{Name of the scale (level 2)}
#'   \item{Subscale}{Name of the subscale (level 1)}
#'   \item{Text}{Item text}
#'   \item{Original}{Item ID in the original, development item pool}
#' }
"hitopsr_items"

#' HiTOP-SR Scale Data
#'
#' Information about scales in the HiTOP-SR.
"hitopsr_scales"

#' HiTOP-SR Subscale Data
#'
#' Information about subscales in the HiTOP-SR.
"hitopsr_subscales"

#' HiTOP-BR Item Data
#'
#' Information about items in the HiTOP-BR.
#'
#' @format A \link[tibble]{tibble} with 45 rows and 5 columns:
#' \describe{
#'   \item{HBR}{Item number on the HITOP-BR}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Scale}{Name of the scale}
#'   \item{Externalizing}{Whether the item is part of the Externalizing scale}
#'   \item{PFactor}{Whether the item is part of the p-Factor scale}
#'   \item{Text}{Item text}
#'   \item{HSR}{Item number on the HiTOP-SR}
#'   \item{Original}{Item ID in the original, development item pool}
#' }
"hitopbr_items"

#' HiTOP-BR Scale Data
#'
#' Information about scales in the HiTOP-BR.
"hitopbr_scales"

#' Simulated HiTOP-SR Data
#'
#' Simulated responses to items on the full HiTOP-SR (with 405 items).
#'
#' @format ## `sim_hitopsr` A \link[tibble]{tibble} with 100 rows and 405 columns.
#' \describe{
#'   \item{hsr_1 to hsr_405}{Responses on each item}
#' }
"sim_hitopsr"

#' Simulated HiTOP-BR Data
#'
#' Simulated responses to items on the HiTOP-BR (with 45 items).
#'
#' @format ## `sim_hitopbr` A \link[tibble]{tibble} with 100 rows and 45 columns.
#' \describe{
#'   \item{hbr_1 to hbr_45}{Responses on each item}
#' }
"sim_hitopbr"

#' Simulated PID-5 Data
#'
#' Simulated responses to items on the full PID-5 (with 220 items).
#'
#' @format ## `sim_pid5` A \link[tibble]{tibble} with 100 rows and 220 columns.
#' \describe{
#'   \item{pid_1 to pid_220}{Responses on each item}
#' }
"sim_pid5"

#' Simulated PID-5-SF Data
#'
#' Simulated responses to items on the PID-5-SF (with 100 items).
#'
#' @format ## `sim_pid5sf` A \link[tibble]{tibble} with 100 rows and 100 columns.
#' \describe{
#'   \item{pid_1 to pid_100}{Responses on each item}
#' }
"sim_pid5sf"

#' Simulated PID-5-BF Data
#'
#' Simulated responses to items on the PID-5-BF (with 25 items).
#'
#' @format ## `sim_pid5bf` A \link[tibble]{tibble} with 100 rows and 25 columns.
#' \describe{
#'   \item{pid_1 to pid_25}{Responses on each item}
#' }
"sim_pid5bf"

#' Real PID-5-SF Data
#'
#' Real responses to items on the PID-5-SF (with 100 items) from University of
#' Kansas students.
#'
#' @format ## `ku_pid5sf` A \link[tibble]{tibble} with 386 rows and 101 columns.
#' \describe{
#'   \item{response_id}{An anonymized id for each participant}
#'   \item{pid_1 to pid_100}{Responses on each item}
#' }
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
"ku_hitopbr"

#' Real HiTOP-SR Data
#'
#' Real responses to items on the HiTOP-SR from University of Kansas students.
#'
#' @format A \link[tibble]{tibble} with 4111 rows and 407 columns.
#' \describe{
#'   \item{participant}{An anonymized id for each participant}
#'   \item{biosex}{A factor indicating each participant's biological sex}
#'   \item{hsr001 to hsr405}{Responses on each item}
#' }
"ku_hitopsr"
