#' Personality Inventory for DSM-5 Item Data
#'
#' Information about the items in different forms of the PID-5.
#'
#' @format ## `pid_items` A data frame with 220 rows and 7 columns:
#' \describe{
#'   \item{PID5, PID5FSF, PID5BF}{Item number on the full PID-5, PID-5 faceted short form, and PID-5 brief form}
#'   \item{Reverse}{Whether the item needs to be reverse scored}
#'   \item{RIS}{Item number on the response inconsistency scale}
#'   \item{ORS}{Item number on the overreporting scale}
#'   \item{Facet}{Name of the facet}
#' }
"pid_items"

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

#' Real PID-5-FSF Data
#'
#' Real responses to items on the PID-5-FSF (with 100 items) from KU students.
#'
#' @format ## `ku_pid5fsf` A data frame with 386 rows and 101 columns.
#' \describe{
#'   \item{response_id}{An anonymized id for each participant}
#'   \item{pid_1 to pid_100}{Responses on each item}
#' }
"ku_pid5fsf"
