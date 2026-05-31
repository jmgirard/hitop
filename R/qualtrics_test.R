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

#' Generate Discrete Display Logic for Qualtrics API
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
      }
    }
  }

  is_alc_quantity <- grepl(
    "alc.*quant|quant.*alc",
    items$Variable,
    ignore.case = TRUE
  )
  # UPDATE: Added 'cgr' to the regex to catch hsum_nic_quant_cgr
  is_cig_quantity <- grepl(
    "(cig|cigar|cgr).*quant|quant.*(cig|cigar|cgr)",
    items$Variable,
    ignore.case = TRUE
  )
  is_oth_quantity <- grepl(
    "_oth$|_other$",
    items$Variable,
    ignore.case = TRUE
  ) &
    grepl("quant", items$Variable, ignore.case = TRUE)

  for (i in seq_len(nrow(items))) {
    var_name <- items$Variable[i]
    text <- items$Text[i]
    field_type <- tolower(items$Field_Type[i])
    charset <- items$Choice_Set[i]

    # TEXT FORMATTING UPDATES -----------------------------------------------

    # 1. Line Break and Bold for items with a colon (e.g., withdrawal items)
    if (grepl(":", text)) {
      text <- sub("(.*?:\\s*)(.*)", "\\1<br><br><b>\\2</b>", text)
    }

    # 2. Dynamic Piped Text for "other specified substances"
    if (grepl("other specified substances", text)) {
      # Grab the Question ID generated for the text entry item
      oth_txt_qid <- qid_map[["hsum_oth_txt"]]

      # Only pipe if the text entry item has already been successfully created
      if (!is.null(oth_txt_qid)) {
        piped_string <- sprintf(
          "other specified substances (${q://%s/ChoiceTextEntryValue})",
          oth_txt_qid
        )
        text <- gsub("other specified substances", piped_string, text)
      }
    }
    # -----------------------------------------------------------------------

    # 1. Determine Qualtrics QuestionType and Selector
    q_type <- "MC"
    q_selector <- "SAVR"

    if (field_type == "checkbox") {
      q_selector <- "MAVR"
    }
    if (is_alc_quantity[i] || is_cig_quantity[i]) {
      q_selector <- "DL"
    }
    if (is_oth_quantity[i] || field_type == "text") {
      q_type <- "TE"
      q_selector <- "SL"
    }
    # Support inline descriptive text from the items dataframe
    if (field_type == "descriptive") {
      q_type <- "DB"
      q_selector <- "TB"
    }

    # 2. Build Choices List
    api_choices <- list()
    if (q_type == "MC") {
      if (is_alc_quantity[i]) {
        vals <- c(1:20, 99)
        labs <- c(as.character(1:19), "20+", "Prefer not to say")
      } else if (is_cig_quantity[i]) {
        vals <- c(1:60, 99)
        labs <- c(as.character(1:59), "60+", "Prefer not to say")
      } else {
        sub_choices <- choices[choices$Choice_Set == charset, ]
        vals <- sub_choices$Value
        labs <- sub_choices$Label
      }

      for (j in seq_along(vals)) {
        api_choices[[as.character(vals[j])]] <- list(Display = labs[j])
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

    # Force the mandatory SubSelector rule for standard MC questions
    if (q_type == "MC" && q_selector %in% c("SAVR", "MAVR")) {
      payload$SubSelector <- "TX"
    }

    if (length(api_choices) > 0 && q_type != "DB") {
      payload$Choices <- api_choices
      payload$ChoiceOrder <- I(names(api_choices))
    }

    # 4. Handle Branching Logic
    gate_var <- items$Gate_Variable[i]
    gate_val <- trimws(items$Gate_Value[i])

    if (
      !is.na(gate_var) && gate_var != "" && !is.na(gate_val) && gate_val != ""
    ) {
      gate_qid <- qid_map[[gate_var]]

      if (!is.null(gate_qid)) {
        if (gate_val == "count>1") {
          payload$DisplayLogic <- build_count_logic(gate_qid, 1)
        } else {
          p_is_alc <- grepl(
            "alc.*quant|quant.*alc",
            gate_var,
            ignore.case = TRUE
          )
          # UPDATE: Apply the 'cgr' regex fix to logic gates as well
          p_is_cig <- grepl(
            "(cig|cigar|cgr).*quant|quant.*(cig|cigar|cgr)",
            gate_var,
            ignore.case = TRUE
          )

          if (p_is_alc) {
            p_values <- c(1:20, 99)
          } else if (p_is_cig) {
            p_values <- c(1:60, 99)
          } else {
            parent_idx <- which(items$Variable == gate_var)[1]
            p_charset <- items$Choice_Set[parent_idx]
            p_values <- choices$Value[choices$Choice_Set == p_charset]
          }

          matched_vals <- character(0)

          if (gate_val == "any_other") {
            matched_vals <- p_values[
              !is.na(p_values) & !(p_values %in% c(0, 1, 99))
            ]
          } else if (grepl("^(>=|<=|>|<|!=)", gate_val)) {
            operator <- regmatches(
              gate_val,
              regexec("^(>=|<=|>|<|!=)", gate_val)
            )[[1]][1]
            val_part <- trimws(sub(operator, "", gate_val, fixed = TRUE))
            num_thresh <- suppressWarnings(as.numeric(val_part))

            if (operator == "!=") {
              matched_vals <- p_values[p_values != num_thresh]
            }
            if (operator == ">") {
              matched_vals <- p_values[p_values > num_thresh]
            }
            if (operator == ">=") {
              matched_vals <- p_values[p_values >= num_thresh]
            }
            if (operator == "<") {
              matched_vals <- p_values[p_values < num_thresh]
            }
            if (operator == "<=") {
              matched_vals <- p_values[p_values <= num_thresh]
            }

            matched_vals <- matched_vals[
              !is.na(matched_vals) & matched_vals != 99
            ]
          } else {
            matched_vals <- trimws(strsplit(gate_val, "[,|]")[[1]])
          }

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
    }
  }

  cli::cli_alert_success("Finished pushing items to Survey ID: {survey_id}")
  invisible(qid_map)
}
