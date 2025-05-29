#' Personality Inventory for DSM-5 Item Data
#'
#' Information about the items in different forms of the PID-5.
#'
#' @format ## `pid_items` A data frame with 220 rows and 12 columns:
#' \describe{
#'   \item{PID5, PID5FSF, PID5BF}{Item number on the full PID-5, PID-5 faceted short form, and PID-5 brief form}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{INC,INCS}{Item number on the response inconsistency scale full and short forms}
#'   \item{ORS}{Item number on the overreporting scale}
#'   \item{PRD}{Item number on the positive impression management response distortion scale}
#'   \item{SDTD}{Item number on the social desirability-total denial scale}
#'   \item{Facet}{Name of the facet}
#'   \item{Domain}{Name of the domain}
#'   \item{Text}{Item text, copyright APA}
#' }
"pid_items"

#' HiTOP-PRO Item Data
#' 
#' Information about items in the HiTOP-PRO.
#' 
#' @format A data frame with 405 rows and 8 columns:
#' \describe{
#'   \item{PRO}{Item number on the full HiTOP-PRO}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{Spectrum}{Name of the spectrum (level 4)}
#'   \item{Subfactor}{Name of the subfactor (level 3)}
#'   \item{Scale}{Name of the scale (level 2)}
#'   \item{Subscale}{Name of the subscale (level 1)}
#'   \item{Text}{Item text}
#' }
"hitop_pro_items"

#' Simulated PID-5 Data
#'
#' Simulated responses to items on the full PID-5 (with 220 items).
#'
#' @format ## `sim_pid5` A data frame with 100 rows and 220 columns.
#' \describe{
#'   \item{pid_1 to pid_220}{Responses on each item}
#' }
"sim_pid5"

#' Simulated PID-5-FSF Data
#'
#' Simulated responses to items on the PID-5-FSF (with 100 items).
#'
#' @format ## `sim_pid5fsf` A data frame with 100 rows and 100 columns.
#' \describe{
#'   \item{pid_1 to pid_100}{Responses on each item}
#' }
"sim_pid5fsf"

#' Simulated PID-5-BF Data
#'
#' Simulated responses to items on the PID-5-BF (with 25 items).
#'
#' @format ## `sim_pid5bf` A data frame with 100 rows and 25 columns.
#' \describe{
#'   \item{pid_1 to pid_25}{Responses on each item}
#' }
"sim_pid5fsf"

#' Real PID-5-FSF Data
#'
#' Real responses to items on the PID-5-FSF (with 100 items) from University of 
#' Kansas students.
#'
#' @format ## `ku_pid5fsf` A data frame with 386 rows and 101 columns.
#' \describe{
#'   \item{response_id}{An anonymized id for each participant}
#'   \item{pid_1 to pid_100}{Responses on each item}
#' }
"ku_pid5fsf"

#' Real HiTOP-PRO Data
#'
#' Real responses to items on the HiTOP-PRO from University of Kansas students.
#'
#' @format A data frame with 131 rows and 407 columns.
#' \describe{
#'   \item{participant}{An anonymized id for each participant}
#'   \item{biosex}{A factor indicating each participant's biological sex}
#'   \item{hitop001 to hitop405}{Responses on each item}
#' }
"ku_hitoppro"
