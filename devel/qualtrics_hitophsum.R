# Maintainer script: build the HiTOP-HSUM Qualtrics survey via the Qualtrics
# survey-definitions API, from the keying tables (M19; D-014/D-015).
#
# The committed artifact inst/extdata/hitophsum_qualtrics.qsf is DERIVED from
# hitophsum_items/hitophsum_choices via this script — never hand-edited in the
# Qualtrics builder. tests/testthat/test-qualtrics-hitophsum.R locks the
# committed QSF to the keying tables, so a stale artifact fails the suite.
#
# Requirements (local only — devel/ is .Rbuildignore'd, so none of this
# touches DESCRIPTION): install.packages(c("httr2", "cli")); a Qualtrics API
# token with survey-definitions access; your datacenter id (the subdomain in
# your Qualtrics URL, e.g. "ca1").
#
# One-time setup — put credentials in ~/.Renviron (never in the repo):
#   usethis::edit_r_environ()   # add these two lines, then restart R:
#   QUALTRICS_API_KEY=<your token>      (Account Settings > Qualtrics IDs)
#   QUALTRICS_DATACENTER=<e.g. ca1>     (the subdomain in your Qualtrics URL)
#
# Run recipe (fully automated: creates the survey, pushes all questions,
# exports the QSF, and overwrites inst/extdata/hitophsum_qualtrics.qsf):
#   1. devtools::load_all()  # for hitophsum_items/choices/instructions
#   2. source("devel/qualtrics_hitophsum.R")
#   3. rebuild_hitophsum_qsf()
#   4. Rscript -e 'devtools::test(filter = "qualtrics")'  # must be green
#
# If the automated QSF export fails (the ?format=qsf endpoint is not part of
# the documented API surface), the function says so and you fall back to the
# manual export: open the created survey > Tools > Import/Export > Export
# Survey, then save the download over inst/extdata/hitophsum_qualtrics.qsf.
# Do not edit survey content in the Qualtrics builder either way — the
# committed QSF must stay a pure product of the keying tables.
#
# Text adaptations applied to hitophsum_items$Text (mirrored by the test and
# documented in cairn/SOURCES.md):
#   1. "stem: symptom" items (the withdrawal set) render the symptom bolded
#      on its own line: "stem: <br><br><b>symptom</b>".
#   2. "other specified substances" is followed by the respondent's own
#      wording, piped from hsum_oth_txt: "other specified substances
#      (${q://QID/ChoiceTextEntryValue})".

# Shared request skeleton for the survey-definitions API.
qualtrics_request <- function(api_token, data_center, path) {
  httr2::request(
    sprintf("https://%s.qualtrics.com/API/v3/%s", data_center, path)
  ) |>
    httr2::req_headers(
      "X-API-TOKEN" = api_token,
      "Accept" = "application/json"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

qualtrics_stop_on_error <- function(resp) {
  if (httr2::resp_status(resp) >= 400) {
    data <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    msg <- data$meta$error$errorMessage
    stop(if (is.null(msg)) "Unknown API Error" else msg, call. = FALSE)
  }
  invisible(resp)
}

# Create a new blank survey; returns its SurveyID.
create_qualtrics_survey <- function(api_token, data_center, name) {
  resp <- qualtrics_request(api_token, data_center, "survey-definitions") |>
    httr2::req_body_json(list(
      SurveyName = name,
      Language = "EN",
      ProjectCategory = "CORE"
    )) |>
    httr2::req_perform()
  qualtrics_stop_on_error(resp)
  httr2::resp_body_json(resp)$result$SurveyID
}

# Export a survey as QSF text. Uses the ?format=qsf export; validated by
# shape (a QSF is a JSON object with SurveyEntry + SurveyElements) so a
# non-QSF response fails loudly instead of writing garbage.
export_qualtrics_qsf <- function(api_token, data_center, survey_id) {
  resp <- qualtrics_request(
    api_token,
    data_center,
    sprintf("survey-definitions/%s?format=qsf", survey_id)
  ) |>
    httr2::req_perform()
  qualtrics_stop_on_error(resp)
  txt <- httr2::resp_body_string(resp)
  parsed <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) NULL
  )
  # Never re-serialize a wrapped response — a jsonlite round-trip is not
  # byte-faithful (M19 review F4). Anything that is not the bare QSF stops.
  if (is.null(parsed$SurveyEntry) || is.null(parsed$SurveyElements)) {
    stop(
      "The export response is not QSF-shaped; export manually via ",
      "Tools > Import/Export > Export Survey.",
      call. = FALSE
    )
  }
  txt
}

# One-shot rebuild: create survey -> push all questions -> export QSF ->
# overwrite the committed artifact. Credentials come from ~/.Renviron.
rebuild_hitophsum_qsf <- function(
  api_token = Sys.getenv("QUALTRICS_API_KEY"),
  data_center = Sys.getenv("QUALTRICS_DATACENTER"),
  name = paste0("HiTOP-HSUM v1.0 (rebuilt ", Sys.Date(), ")"),
  file = "inst/extdata/hitophsum_qualtrics.qsf"
) {
  if (!nzchar(api_token) || !nzchar(data_center)) {
    stop(
      "Set QUALTRICS_API_KEY and QUALTRICS_DATACENTER in ~/.Renviron ",
      "(usethis::edit_r_environ()) and restart R.",
      call. = FALSE
    )
  }
  survey_id <- create_qualtrics_survey(api_token, data_center, name)
  cli::cli_alert_info("Created survey {survey_id} ({name})")
  push_hitophsum_to_qualtrics(
    api_token,
    data_center,
    survey_id,
    items = hitophsum_items,
    choices = hitophsum_choices,
    instructions = hitophsum_instructions
  )
  qsf <- export_qualtrics_qsf(api_token, data_center, survey_id)
  writeLines(qsf, file, useBytes = TRUE)
  cli::cli_alert_success(
    "Wrote {.file {file}} — run devtools::test(filter = \"qualtrics\")"
  )
  invisible(survey_id)
}

# Send a Question Payload to the Qualtrics API (with error parsing)
create_qualtrics_question <- function(
  api_token,
  data_center,
  survey_id,
  payload
) {
  base_url <- sprintf(
    "https://%s.qualtrics.com/API/v3/survey-definitions/%s/questions",
    data_center,
    survey_id
  )

  req <- httr2::request(base_url) |>
    httr2::req_headers(
      "X-API-TOKEN" = api_token,
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(payload) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  resp_data <- httr2::resp_body_json(req)

  if (httr2::resp_status(req) >= 400) {
    error_msg <- "Unknown API Error"
    if (!is.null(resp_data$meta$error$errorMessage)) {
      error_msg <- resp_data$meta$error$errorMessage
    }
    stop(error_msg, call. = FALSE)
  }

  resp_data
}

# Generate Discrete Display Logic for Qualtrics API
build_display_logic <- function(gate_qid, gate_vals) {
  if (length(gate_vals) == 0) {
    return(NULL)
  }

  if_block <- list(Type = "If")

  for (i in seq_along(gate_vals)) {
    val <- as.character(gate_vals[i])
    idx <- as.character(i - 1)

    expr <- list(
      Type = "Expression",
      LogicType = "Question",
      QuestionID = gate_qid,
      QuestionIsInLoop = "no",
      ChoiceLocator = paste0("q://", gate_qid, "/SelectableChoice/", val),
      Operator = "Selected",
      QuestionIDFromLocator = gate_qid,
      LeftOperand = paste0("q://", gate_qid, "/SelectableChoice/", val)
    )

    if (i > 1) {
      expr$Conjunction <- "Or"
    }

    if_block[[idx]] <- expr
  }

  list(Type = "BooleanExpression", inPage = FALSE, `0` = if_block)
}

# Generate Count Logic for Qualtrics API (e.g., count>1)
build_count_logic <- function(gate_qid, threshold = 1) {
  list(
    Type = "BooleanExpression",
    inPage = FALSE,
    `0` = list(
      Type = "If",
      `0` = list(
        Type = "Expression",
        LogicType = "Question",
        QuestionID = gate_qid,
        QuestionIsInLoop = "no",
        ChoiceLocator = paste0("q://", gate_qid, "/SelectedChoicesCount"),
        Operator = "GreaterThan",
        QuestionIDFromLocator = gate_qid,
        LeftOperand = paste0("q://", gate_qid, "/SelectedChoicesCount"),
        RightOperand = as.character(threshold)
      )
    )
  )
}

# Push HiTOP-HSUM to Qualtrics via API
push_hitophsum_to_qualtrics <- function(
  api_token,
  data_center,
  survey_id,
  items,
  choices,
  instructions = NULL
) {
  qid_map <- list()
  failed <- character(0)

  # 0. Inject Global Instructions at the Beginning
  if (!is.null(instructions) && length(instructions) > 0) {
    instruction_text <- instructions$start[1]

    if (
      !is.null(instruction_text) &&
        !is.na(instruction_text) &&
        nchar(instruction_text) > 0
    ) {
      cli::cli_progress_step("Creating question: hitophsum_instructions")

      instruction_payload <- list(
        QuestionText = instruction_text,
        DataExportTag = "hitophsum_instructions",
        QuestionType = "DB",
        Selector = "TB",
        Configuration = list(QuestionDescriptionOption = "UseText")
      )

      resp <- tryCatch(
        {
          create_qualtrics_question(
            api_token,
            data_center,
            survey_id,
            instruction_payload
          )
        },
        error = function(e) {
          cli::cli_alert_danger("Failed on instructions: {e$message}")
          NULL
        }
      )

      if (!is.null(resp) && !is.null(resp$result$QuestionID)) {
        qid_map[["hitophsum_instructions"]] <- resp$result$QuestionID
      } else {
        failed <- c(failed, "hitophsum_instructions")
      }
    }
  }

  for (i in seq_len(nrow(items))) {
    var_name <- items$Variable[i]
    text <- items$Text[i]
    field_type <- tolower(items$Field_Type[i])
    charset <- items$Choice_Set[i]

    # Text adaptation 1: bold the symptom in "stem: symptom" items.
    if (grepl(":\\s*\\S", text)) {
      text <- sub("(.*?:\\s*)(.*)", "\\1<br><br><b>\\2</b>", text)
    }

    # Text adaptation 2: pipe the respondent's "other" substance wording.
    if (grepl("other specified substances", text, fixed = TRUE)) {
      oth_txt_qid <- qid_map[["hsum_oth_txt"]]
      if (!is.null(oth_txt_qid)) {
        piped_string <- sprintf(
          "other specified substances (${q://%s/ChoiceTextEntryValue})",
          oth_txt_qid
        )
        text <- gsub(
          "other specified substances",
          piped_string,
          text,
          fixed = TRUE
        )
      }
    }

    # 1. QuestionType/Selector resolve from Field_Type (never variable-name
    # regexes — the regex path is what shipped an empty cigar dropdown; M18).
    q_type <- switch(field_type, text = "TE", descriptive = "DB", "MC")
    q_selector <- switch(
      field_type,
      radio = "SAVR",
      checkbox = "MAVR",
      dropdown = "DL",
      text = "SL",
      descriptive = "TB"
    )

    # 2. Choices resolve from Choice_Set (quant_* sets included since M18).
    api_choices <- list()
    if (q_type == "MC") {
      sub_choices <- choices[choices$Choice_Set == charset, ]
      if (nrow(sub_choices) == 0) {
        stop("No choices found for ", var_name, " (", charset, ")")
      }
      for (j in seq_len(nrow(sub_choices))) {
        api_choices[[as.character(sub_choices$Value[j])]] <- list(
          Display = sub_choices$Label[j]
        )
      }
    }

    # 3. Initialize Payload
    payload <- list(
      QuestionText = text,
      DataExportTag = var_name,
      QuestionType = q_type,
      Selector = q_selector,
      Configuration = list(QuestionDescriptionOption = "UseText")
    )

    if (q_type == "MC" && q_selector %in% c("SAVR", "MAVR")) {
      payload$SubSelector <- "TX"
    }

    if (length(api_choices) > 0 && q_type == "MC") {
      payload$Choices <- api_choices
      payload$ChoiceOrder <- I(names(api_choices))
    }

    # 4. Display logic from Gate_Variable/Gate_Value. Comparison gates
    # enumerate the qualifying choice values and never include 99, so
    # "Prefer not to say" cannot satisfy a gate (the QSF-side analog of the
    # REDCap "<> '99'" guard).
    gate_var <- items$Gate_Variable[i]
    gate_val <- trimws(items$Gate_Value[i])

    if (
      !is.na(gate_var) && gate_var != "" && !is.na(gate_val) && gate_val != ""
    ) {
      gate_qid <- qid_map[[gate_var]]

      if (!is.null(gate_qid)) {
        if (gate_val == "count>1") {
          payload$DisplayLogic <- build_count_logic(gate_qid, 1)
        } else if (grepl("^(>=|<=|>|<|!=)", gate_val)) {
          parent_idx <- which(items$Variable == gate_var)[1]
          p_charset <- items$Choice_Set[parent_idx]
          p_values <- choices$Value[choices$Choice_Set == p_charset]

          operator <- regmatches(
            gate_val,
            regexec("^(>=|<=|>|<|!=)", gate_val)
          )[[1]][1]
          val_part <- trimws(sub(operator, "", gate_val, fixed = TRUE))
          num_thresh <- suppressWarnings(as.numeric(val_part))

          matched_vals <- switch(
            operator,
            ">=" = p_values[p_values >= num_thresh],
            "<=" = p_values[p_values <= num_thresh],
            ">" = p_values[p_values > num_thresh],
            "<" = p_values[p_values < num_thresh],
            "!=" = p_values[p_values != num_thresh]
          )
          matched_vals <- matched_vals[
            !is.na(matched_vals) & matched_vals != 99
          ]

          if (length(matched_vals) > 0) {
            payload$DisplayLogic <- build_display_logic(gate_qid, matched_vals)
          }
        } else {
          matched_vals <- trimws(strsplit(gate_val, "[,|]")[[1]])
          matched_vals <- matched_vals[matched_vals != ""]
          if (length(matched_vals) > 0) {
            payload$DisplayLogic <- build_display_logic(gate_qid, matched_vals)
          }
        }
      }
    }

    # 5. Send Request
    cli::cli_progress_step("Creating question: {var_name}")
    resp <- tryCatch(
      {
        create_qualtrics_question(api_token, data_center, survey_id, payload)
      },
      error = function(e) {
        cli::cli_alert_danger("Failed on {var_name}: {e$message}")
        NULL
      }
    )

    # 6. Store New QID mapping
    if (!is.null(resp) && !is.null(resp$result$QuestionID)) {
      qid_map[[var_name]] <- resp$result$QuestionID
    } else {
      failed <- c(failed, var_name)
    }
  }

  # A partial survey must never look like success: a missing gate parent
  # would ship its children ungated, and rebuild_hitophsum_qsf() would
  # overwrite the good committed artifact (M19 review F1).
  if (length(failed) > 0) {
    stop(
      length(failed),
      " question(s) failed to push (survey ",
      survey_id,
      " is incomplete; fix and rerun): ",
      paste(failed, collapse = ", "),
      call. = FALSE
    )
  }

  cli::cli_alert_success("Finished pushing items to Survey ID: {survey_id}")
  invisible(qid_map)
}
