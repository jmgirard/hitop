#' Rename Columns to Standard PID-5 Item Names
#'
#' Rename data frame columns to the standard PID-5 item names of one form,
#' matching either on the item number carried in the current column name or on
#' the literal item prompt text. The standard names are the ones the package's
#' REDCap and Qualtrics exports write and the shipped datasets carry:
#' `pid5_001` to `pid5_220` for the full form, `pid5sf_001` to `pid5sf_100` for
#' the short form, and `pid5bf_01` to `pid5bf_25` for the brief form.
#'
#' @param data A data frame containing the PID-5 items.
#' @param version A string specifying the PID-5 form the items belong to:
#'   `"FULL"` (220 items), `"SF"` (100 items), or `"BF"` (25 items). Matched
#'   case-insensitively. (default = `"FULL"`)
#' @param method A string specifying the matching method: `"number"` to rename
#'   columns spelled `from_prefix` followed by an item number, or `"text"` to
#'   match against the literal item prompt text in `pid_items$Text`.
#'   (default = `"number"`)
#'
#'   The three forms number their items independently, so `"number"` reads the
#'   digits as an item number of the form named by `version`: under
#'   `version = "SF"`, `pid_7` is short-form item 7, not the full-form item the
#'   short form numbers 7. Data labelled by full-form item numbers must be
#'   renamed with `version = "FULL"` first, or matched with `method = "text"`.
#' @param item_cols An optional character vector of current column names to
#'   be renamed. Required if `method = "text"`.
#' @param item_text An optional character vector of item texts corresponding
#'   exactly to the columns specified in `item_cols`. Required if
#'   `method = "text"`.
#' @param from_prefix A string matched literally at the start of a column name
#'   under `method = "number"`, before the item number. The default is the
#'   spelling this package's own PID-5 datasets used before they were renamed
#'   to match the exports. (default = `"pid_"`)
#' @param prefix A string pasted literally before each standardized item
#'   number, which is zero-padded to the width of the form's largest item
#'   number. `NULL` resolves to the form's own stem: `"pid5_"`, `"pid5sf_"` or
#'   `"pid5bf_"`. (default = `NULL`)
#'
#' @return A data frame with renamed column names for the matched PID-5 items.
#'   Columns that could not be matched keep their names. Under
#'   `method = "number"`, a column spelled like an item of the instrument whose
#'   number names no item of this form, and under `method = "text"`, an
#'   `item_text` entry matching no item of this form, are skipped and named in
#'   a warning of class `hitop_unmatched_items`, which callers may catch or
#'   suppress by class. A column not spelled like an item number is left alone
#'   and not reported.
#'
#' @examples
#' # Rename columns named as this package's datasets were before the rename
#' df <- data.frame(pid_1 = c(0, 1), pid_2 = c(2, 3), age = c(30, 40))
#' names(suppressWarnings(rename_pid5_items(df, version = "FULL")))
#'
#' @export
rename_pid5_items <- function(
  data,
  version = c("FULL", "SF", "BF"),
  method = c("number", "text"),
  item_cols = NULL,
  item_text = NULL,
  from_prefix = "pid_",
  prefix = NULL
) {
  method <- match.arg(method)

  ## Assertions
  validate_data(data)
  validate_string(from_prefix, arg = "from_prefix")
  validate_string(prefix, arg = "prefix", allow_null = TRUE)

  ## Resolve the version, as `score_pid5()` does
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF"))

  ## Resolve this form's rows, its output stem and its padding width
  form <- pid_items[!is.na(pid_items[[version]]), ]
  form_numbers <- form[[version]]
  n_items <- length(form_numbers)
  max_n <- max(form_numbers)
  if (is.null(prefix)) {
    prefix <- switch(
      version,
      "FULL" = "pid5_",
      "SF" = "pid5sf_",
      "BF" = "pid5bf_"
    )
  }
  label <- switch(
    version,
    "FULL" = "PID-5",
    "SF" = "PID-5-SF",
    "BF" = "PID-5-BF"
  )

  ## Track matched standard item numbers for the final summary warning
  matched_n <- integer(0)

  if (method == "number") {
    data_cols <- colnames(data)
    pattern <- paste0(
      "^",
      gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", from_prefix),
      "([0-9]+)$"
    )
    shaped <- grepl(pattern, data_cols)

    if (!any(shaped)) {
      cli::cli_warn(
        "No columns are named {.code {from_prefix}} followed by an item number.",
        class = "hitop_no_columns_matched"
      )
      return(data)
    }

    numbers <- rep(NA_integer_, length(data_cols))
    numbers[shaped] <- as.integer(sub(pattern, "\\1", data_cols[shaped]))

    named <- shaped & numbers %in% form_numbers
    unnamed <- shaped & !named

    warn_unmatched_items(data_cols[unnamed], "column")

    if (any(named)) {
      matched_n <- numbers[named]
      colnames(data)[named] <- item_names(prefix, matched_n, max_n = max_n)
    }
  } else if (method == "text") {
    if (is.null(item_cols) || is.null(item_text)) {
      cli::cli_abort(
        "Both {.arg item_cols} and {.arg item_text} must be provided when {.code method = 'text'}."
      )
    }
    if (length(item_cols) != length(item_text)) {
      cli::cli_abort(
        "{.arg item_cols} and {.arg item_text} must be of the same length."
      )
    }

    ## Verify columns exist in data
    data_locs <- match(item_cols, colnames(data))
    if (any(is.na(data_locs))) {
      cli::cli_abort(
        "Some names in {.arg item_cols} were not found in the data frame columns."
      )
    }

    ## Match text against this form's items only (using trimws for robustness
    ## against surrounding whitespace, as `rename_hitopsr_items()` does)
    locs <- match(trimws(item_text), trimws(form$Text))

    if (any(is.na(locs))) {
      missing_idx <- which(is.na(locs))
      warn_unmatched_items(item_text[missing_idx], "item text")
      data_locs <- data_locs[-missing_idx]
      locs <- locs[-missing_idx]
    }

    if (length(locs) > 0) {
      matched_n <- form_numbers[locs]
      colnames(data)[data_locs] <- item_names(prefix, matched_n, max_n = max_n)
    }
  }

  ## Check for completeness and warn if fewer than all items were matched
  n_matched <- length(unique(matched_n))
  if (n_matched > 0 && n_matched < n_items) {
    cli::cli_warn(c(
      "Only {n_matched} out of {n_items} {label} items were successfully matched and renamed.",
      "i" = "Note: If you plan to use {.fn score_pid5}, ensure uncollected items exist in the data frame as {.code NA} columns."
    ), class = "hitop_incomplete_rename")
  }

  ## Return output
  data
}
