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
#'   `"FULL"` (220 items), `"SF"` (100 items), `"BF"` (25 items), `"BFPM"`
#'   (the 36-item PID5BF+M), `"IRF"` (the 218-item Informant Form, labelled
#'   with its informant wording from `pid_items$TextIRF`), or `"FFBF"` (the
#'   100-item Forensic Faceted Brief Form, labelled with its English
#'   self-report text from `pid_ffbf_items$Text`, also for informant data). Matched
#'   case-insensitively. The forms number their
#'   items independently and
#'   score different sets of scales, so the form named here decides both the
#'   text attached to an item column and which scale columns are recognized.
#'   (default = `"FULL"`)
#' @param prefix A string specifying the prefix used on the column names.
#'   `NULL` resolves to the default for the given `target` and `version`: under
#'   `target = "items"`, the form's own stem (`"pid5_"`, `"pid5sf_"`,
#'   `"pid5bf_"`, `"pid5bfpm_"`, `"pid5irf_"` or `"pid5ffbf_"`); for the full, short and brief forms this is
#'   the pattern the shipped datasets and the package's REDCap export use; under `target = "scales"`, `"pid_"`, which is what
#'   [score_pid5()] writes under its own default `prefix`. (default = `NULL`)
#'
#'   Item columns are expected as the prefix followed by the item number
#'   zero-padded to the width of the form's largest item number (`pid5_001` to
#'   `pid5_220` for the full form, `pid5bf_01` to `pid5bf_25` for the brief
#'   form). A column carrying the prefix and a number that is not one of those
#'   expected names is not labelled, and a warning of class
#'   `hitop_unpadded_items` names it, in a sentence per kind: a number padded to
#'   some other width is reported as not zero-padded to the form's width, and a
#'   number outside the form's range is reported as out of range, whatever its
#'   padding. That warning is raised whether or not any other column matched.
#'   Scale columns are expected as the prefix followed by the scale's
#'   `camelCase` name.
#'
#' @return A data frame with labeled columns. Columns the named form does not
#'   recognize keep whatever attributes they had. The validity-scale columns
#'   [validity_pid5()] writes and the `_se` columns
#'   `score_pid5(calc_se = TRUE)` writes are not labelled. If no column matched
#'   the expected names at all, `data` is returned unchanged and a warning of
#'   class `hitop_no_columns_matched` says so; the `hitop_unpadded_items`
#'   report still names every prefixed item column it found. Both classes may
#'   be caught or suppressed by callers.
#'
#' @references Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F.
#'   (2013). *The Personality Inventory for DSM-5—Informant Form
#'   (PID-5-IRF)—Adult*. American Psychiatric Association. The source of the
#'   informant wording that `version = "IRF"` labels items with. See also
#'   Markon et al. (2013), *Assessment, 20*(3), 370-383.
#'   \doi{10.1177/1073191113486513}
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
  version = c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"),
  prefix = NULL
) {
  target <- match.arg(target)

  ## Assertions
  validate_data(data)
  validate_string(prefix, arg = "prefix", allow_null = TRUE)

  ## Resolve the version, as `score_pid5()` does
  version <- toupper(version)
  version <- match.arg(version, choices = c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF"))

  data_cols <- colnames(data)

  if (target == "items") {
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

    ## This form's rows, in `pid_items` row order; `expected_names` and the
    ## text column stay in step, so a match location indexes both. The
    ## informant form labels with its own wording (D-089(b)).
    ## The FFBF has its own item table and labels with its English
    ## self-report text (D-092).
    if (version == "FFBF") {
      numbers <- pid_ffbf_items$FFBF
      text <- pid_ffbf_items$Text
    } else {
      form <- pid_items[!is.na(pid_items[[version]]), ]
      numbers <- form[[version]]
      text <- if (version == "IRF") form$TextIRF else form$Text
    }
    max_n <- max(numbers)
    expected_names <- item_names(prefix, numbers, max_n = max_n)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected item names with prefix {.str {prefix}}.",
        class = "hitop_no_columns_matched"
      )
    } else {
      for (i in matched_idx) {
        attr(data[[i]], "label") <- text[locs[i]]
      }
    }
    ## The report runs whether or not anything matched, and after the no-match
    ## warning: a frame whose item columns are ALL mis-padded is exactly the
    ## case worth naming, and it is the one an early return used to swallow.
    warn_unpadded_items(
      data_cols,
      prefix = prefix,
      expected = expected_names,
      max_n = max_n,
      instrument = switch(
        version,
        "FULL" = "PID-5",
        "SF" = "PID-5-SF",
        "BF" = "PID-5-BF",
        "BFPM" = "PID5BF+M",
        "IRF" = "PID-5-IRF",
        "FFBF" = "PID-5-FFBF"
      )
    )
  } else if (target == "scales") {
    if (is.null(prefix)) prefix <- "pid_"

    ## The FULL, SF and IRF forms score 25 facets from `pid_scales[[version]]`
    ## and 5 domains from `pid_domains`; the BFPM form scores 18 facets from
    ## `pid_scales$BFPM` and 6 domains from `pid_bfpm_domains`; the BF form
    ## scores its 5 domains and a total directly, all six carried by
    ## `pid_scales$BF`.
    tbl <- pid_scales[[version]]
    stems <- tbl$camelCase
    names_out <- if (version == "BF") tbl$Domain else tbl$Facet
    domains <- if (version %in% c("FULL", "SF", "IRF")) {
      pid_domains
    } else if (version == "BFPM") {
      pid_bfpm_domains
    } else if (version == "FFBF") {
      pid_ffbf_domains
    } else {
      NULL
    }
    if (!is.null(domains)) {
      stems <- c(stems, domains$camelCase)
      names_out <- c(names_out, domains$Domain)
    }

    expected_names <- paste0(prefix, stems)
    locs <- match(data_cols, expected_names)
    matched_idx <- which(!is.na(locs))

    if (length(matched_idx) == 0) {
      cli::cli_warn(
        "No columns matched the expected scale names with prefix {.str {prefix}}.",
        class = "hitop_no_columns_matched"
      )
      return(data)
    }

    for (i in matched_idx) {
      attr(data[[i]], "label") <- names_out[locs[i]]
    }
  }

  data
}
