#' Rename Columns to Standard PID-5 Item Names
#'
#' Rename data frame columns to the standard PID-5 item names of one form,
#' matching either on the item number carried in the current column name or on
#' the literal item prompt text. The standard names are the ones the package's
#' REDCap and Qualtrics exports write and the shipped datasets carry:
#' `pid5_001` to `pid5_220` for the full form, `pid5sf_001` to `pid5sf_100` for
#' the short form, and `pid5bf_01` to `pid5bf_25` for the brief form. The
#' PID5BF+M names, `pid5bfpm_01` to `pid5bfpm_36`, the Informant Form names,
#' `pid5irf_001` to `pid5irf_218`, and the Forensic Faceted Brief Form names,
#' `pid5ffbf_001` to `pid5ffbf_100`, follow the same pattern.
#'
#' @param data A data frame containing the PID-5 items.
#' @param version A string specifying the PID-5 form the items belong to:
#'   `"FULL"` (220 items), `"SF"` (100 items), `"BF"` (25 items), `"BFPM"`
#'   (the 36-item PID5BF+M), `"IRF"` (the 218-item Informant Form, whose
#'   text matches `pid_items$TextIRF`), or `"FFBF"` (the 100-item Forensic
#'   Faceted Brief Form, whose text matches any of the four texts in
#'   [pid_ffbf_items]: self or informant report, English or German). Only a
#'   full name is accepted, in any letter case. Any other value, a start of a
#'   name such as `"F"` included, is an error of class
#'   `hitop_unknown_version`. (default = `"FULL"`)
#' @param method A string specifying the matching method: `"number"` to rename
#'   columns spelled `from_prefix` followed by an item number, or `"text"` to
#'   match against the literal item prompt text in `pid_items$Text`
#'   (`pid_items$TextIRF` for `version = "IRF"`; for `version = "FFBF"`, any
#'   of the four texts in [pid_ffbf_items]). Four differences do not count:
#'   typographic quotes (‘ ’ “ ”) in place of straight ones, a leading
#'   ellipsis (`...` or `…`), a final period, and spaces, tabs or line breaks
#'   around the text. The informant text has no "He or she..." stem, so
#'   remove that stem from text copied from the printed informant form first.
#'   (default = `"number"`)
#'
#'   The forms number their items independently, so `"number"` reads the
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
#'   number. `NULL` resolves to the form's own stem: `"pid5_"`, `"pid5sf_"`,
#'   `"pid5bf_"`, `"pid5bfpm_"`, `"pid5irf_"` or `"pid5ffbf_"`. (default = `NULL`)
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
#'   Two other warnings carry a class of their own. Under `method = "number"`,
#'   if no column at all is named `from_prefix` followed by a number, nothing
#'   is renamed and the report is `hitop_no_columns_matched`. Under either
#'   method, if some but not all of the form's items were renamed, the
#'   completeness report is `hitop_incomplete_rename`.
#'
#'   Under `method = "text"`, a call in which two columns match the same item
#'   is an error of class `hitop_duplicate_item_match`, since both would take
#'   the same name. The message names the item and the columns. For the FFBF
#'   this happens when a data frame holds two texts of one item, for example
#'   its self-report and informant texts, or its English and German texts.
#'   Rename each form's columns in its own data frame, or give each call its
#'   own `prefix`.
#'
#' @references Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F.
#'   (2013). *The Personality Inventory for DSM-5—Informant Form
#'   (PID-5-IRF)—Adult*. American Psychiatric Association. The source of the
#'   informant wording that `version = "IRF"` matches under
#'   `method = "text"`. See also Markon et al. (2013), *Assessment, 20*(3),
#'   370-383. \doi{10.1177/1073191113486513}
#'
#' @examples
#' # Rename columns named as this package's datasets were before the rename
#' df <- data.frame(pid_1 = c(0, 1), pid_2 = c(2, 3), age = c(30, 40))
#' names(suppressWarnings(rename_pid5_items(df, version = "FULL")))
#'
#' @export
rename_pid5_items <- function(
  data,
  version = c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"),
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
  version <- resolve_pid5_version(version, c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"))

  ## Resolve this form's rows, its text, its output stem and its padding
  ## width. The informant form has its own wording (D-089(b)).
  ## The FFBF has its own item table (D-092), and its text method matches any
  ## of the form's four texts: self or informant report, English or German.
  if (version == "FFBF") {
    form_numbers <- pid_ffbf_items$FFBF
    text_pool <- c(
      pid_ffbf_items$Text, pid_ffbf_items$TextIRF,
      pid_ffbf_items$TextDE, pid_ffbf_items$TextIRFDE
    )
    pool_numbers <- rep(form_numbers, 4)
  } else {
    form <- pid_items[!is.na(pid_items[[version]]), ]
    form_text <- if (version == "IRF") form$TextIRF else form$Text
    form_numbers <- form[[version]]
    text_pool <- form_text
    pool_numbers <- form_numbers
  }
  n_items <- length(form_numbers)
  max_n <- max(form_numbers)
  if (is.null(prefix)) {
    prefix <- switch(
      version,
      "FULL" = "pid5_",
      "SF" = "pid5sf_",
      "BF" = "pid5bf_",
      "BFPM" = "pid5bfpm_",
      "IRF" = "pid5irf_",
      "FFBF" = "pid5ffbf_"
    )
  }
  label <- switch(
    version,
    "FULL" = "PID-5",
    "SF" = "PID-5-SF",
    "BF" = "PID-5-BF",
    "BFPM" = "PID5BF+M",
    "IRF" = "PID-5-IRF",
    "FFBF" = "PID-5-FFBF"
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

    ## `item_col_numbers()` strips `from_prefix` by its own length, which on a
    ## shaped column leaves exactly the digits the pattern would have captured,
    ## and it silences the coercion of a number past R's integer range: such a
    ## column comes back `NA` and reads below as naming no item, where a bare
    ## `as.integer()` also leaked base R's "NAs introduced by coercion".
    numbers <- rep(NA_integer_, length(data_cols))
    numbers[shaped] <- item_col_numbers(data_cols[shaped], from_prefix)

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

    ## Match text against this form's item texts only, both sides put in one
    ## form first (typographic quotes, a leading ellipsis, a final period and
    ## surrounding whitespace do not count). For the FFBF the pool holds four
    ## texts per item.
    locs <- match(normalize_item_text(item_text), normalize_item_text(text_pool))

    if (any(is.na(locs))) {
      missing_idx <- which(is.na(locs))
      warn_unmatched_items(item_text[missing_idx], "item text")
      data_locs <- data_locs[-missing_idx]
      locs <- locs[-missing_idx]
    }

    if (length(locs) > 0) {
      matched_n <- pool_numbers[locs]
      ## Two columns matching one item would take the same name, in any form:
      ## a repeated text, or two of the FFBF's four texts of one item (self and
      ## informant, or English and German). D-094(c).
      dup_n <- unique(matched_n[duplicated(matched_n)])
      if (length(dup_n) > 0) {
        dup_cols <- colnames(data)[data_locs[matched_n %in% dup_n]]
        cli::cli_abort(c(
          "Two or more columns match the same {label} item: {.val {dup_n}}.",
          "x" = "The columns are {.val {dup_cols}}.",
          "i" = "Give each item one column. Rename each form's columns in its own data frame, or give each call its own {.arg prefix}."
        ), class = "hitop_duplicate_item_match")
      }
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
