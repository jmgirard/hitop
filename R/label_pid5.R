#' Label PID-5 Columns with Semantic Descriptions
#'
#' Add literal item text or clean scale names as attributes to data frame
#' columns, making them readable by data viewers and reporting packages.
#'
#' @param data A data frame containing PID-5 items or scales.
#' @param target A string specifying what to label: `"items"` to label raw item
#'   columns with questionnaire text, or `"scales"` to label computed scale
#'   columns. (default = `"items"`)
#' @param version A string specifying the PID-5 form the columns belong to:
#'   `"FULL"` (220 items), `"SF"` (100 items), or `"BF"` (25 items). Matched
#'   case-insensitively. The three forms number their items independently and
#'   score different sets of scales, so the form named here decides both the
#'   text attached to an item column and which scale columns are recognized.
#'   (default = `"FULL"`)
#' @param prefix A string specifying the prefix used on the column names.
#'   `NULL` resolves to the default for the given `target` and `version`: under
#'   `target = "items"`, the form's own stem (`"pid5_"`, `"pid5sf_"` or
#'   `"pid5bf_"`), the pattern the shipped datasets and the package's REDCap
#'   export use; under `target = "scales"`, `"pid_"`, which is what
#'   [score_pid5()] writes under its own default `prefix`. (default = `NULL`)
#'
#'   Item columns are expected as the prefix followed by the item number
#'   zero-padded to the width of the form's largest item number (`pid5_001` to
#'   `pid5_220` for the full form, `pid5bf_01` to `pid5bf_25` for the brief
#'   form). Columns that carry the prefix and a number without that padding are
#'   not labelled, and a warning of class `hitop_unpadded_items` names them.
#'   Scale columns are expected as the prefix followed by the scale's
#'   `camelCase` name.
#'
#' @return A data frame with labeled columns. Columns the named form does not
#'   recognize keep whatever attributes they had. The validity-scale columns
#'   [validity_pid5()] writes and the `_se` columns
#'   `score_pid5(calc_se = TRUE)` writes are not labelled.
#'
#' @examples
#' # Attach item text as a `label` attribute to the raw item columns
#' labeled <- label_pid5(sim_pid5bf, target = "items", version = "BF")
#' attr(labeled$pid5bf_01, "label")
#'
#' @export
label_pid5 <- function(
  data,
  target = c("items", "scales"),
  version = c("FULL", "SF", "BF"),
  prefix = NULL
) {
  target <- match.arg(target)

  ## Assertions
  validate_data(data)
  validate_string(prefix, arg = "prefix", allow_null = TRUE)

  ## Resolve the version, as `score_pid5()` does
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))

  data_cols <- colnames(data)

  if (target == "items") {
    if (is.null(prefix)) {
      prefix <- switch(
        version,
        "FULL" = "pid5_",
        "SF" = "pid5sf_",
        "BF" = "pid5bf_"
      )
    }

    ## This form's rows, in `pid_items` row order; `expected_names` and the
    ## text column stay in step, so a match location indexes both.
    form <- pid_items[!is.na(pid_items[[version]]), ]
    max_n <- max(form[[version]])
    expected_names <- item_names(prefix, form[[version]], max_n = max_n)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected item names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- form$Text[locs[i]]
    }
    warn_unpadded_items(
      unpadded_item_cols(data_cols, prefix, expected_names),
      width = nchar(as.character(max_n)),
      instrument = switch(
        version,
        "FULL" = "PID-5",
        "SF" = "PID-5-SF",
        "BF" = "PID-5-BF"
      )
    )
  } else if (target == "scales") {
    if (is.null(prefix)) prefix <- "pid_"

    ## The FULL and SF forms score 25 facets from `pid_scales[[version]]` and 5
    ## domains from `pid_domains`; the BF form scores its 5 domains and a total
    ## directly, all six carried by `pid_scales$BF`.
    tbl <- pid_scales[[version]]
    stems <- tbl$camelCase
    names_out <- if (version == "BF") tbl$Domain else tbl$Facet
    if (version != "BF") {
      stems <- c(stems, pid_domains$camelCase)
      names_out <- c(names_out, pid_domains$Domain)
    }

    expected_names <- paste0(prefix, stems)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected scale names with prefix {.str {prefix}}."
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- names_out[locs[i]]
    }
  }

  data
}
