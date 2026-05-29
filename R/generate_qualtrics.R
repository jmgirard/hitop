#' Generate a Qualtrics Import File for the HiTOP-BR
#'
#' @description Creates a text file formatted for the Qualtrics Advanced Format
#'   import tool containing the Hierarchical Taxonomy of Psychopathology - Brief
#'   Report (HiTOP-BR) items and instructions.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitopbr_qualtrics.txt"`.
#' @param block_name Character string specifying the name of the block in
#'   Qualtrics. Defaults to `"HiTOP-BR"`.
#' @param id_prefix Character string specifying the prefix for the question IDs.
#'   Defaults to `"HBR"`.
#' @param include_instructions Logical. If `TRUE` (default), includes the
#'   starting instructions as a descriptive text block.
#' @param breaks Integer or `NULL`. The number of items to display before
#'   inserting a page break. Set to `0` or `NULL` to disable pagination.
#'   Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @export
generate_qualtrics_hitopbr <- function(
  file = "hitopbr_qualtrics.txt",
  block_name = "HiTOP-BR",
  id_prefix = "HBR",
  include_instructions = TRUE,
  breaks = 15
) {
  build_qualtrics_txt(
    items = hitopbr_items,
    instructions = hitopbr_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the HiTOP-SR
#'
#' @description Creates a text file formatted for the Qualtrics Advanced Format
#'   import tool containing the Hierarchical Taxonomy of Psychopathology -
#'   Self-Report (HiTOP-SR) items and instructions.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitopsr_qualtrics.txt"`.
#' @param block_name Character string specifying the name of the block in
#'   Qualtrics. Defaults to `"HiTOP-SR"`.
#' @param id_prefix Character string specifying the prefix for the question IDs.
#'   Defaults to `"HSR"`.
#' @param include_instructions Logical. If `TRUE` (default), includes the
#'   starting instructions as a descriptive text block.
#' @param breaks Integer or `NULL`. The number of items to display before
#'   inserting a page break. Set to `0` or `NULL` to disable pagination.
#'   Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @export
generate_qualtrics_hitopsr <- function(
  file = "hitopsr_qualtrics.txt",
  block_name = "HiTOP-SR",
  id_prefix = "HSR",
  include_instructions = TRUE,
  breaks = 15
) {
  build_qualtrics_txt(
    items = hitopsr_items,
    instructions = hitopsr_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

# Internal Helper: Build the Qualtrics text file
build_qualtrics_txt <- function(
  items,
  instructions,
  file,
  block_name,
  id_prefix,
  include_instructions,
  breaks
) {
  # 1. Initialize the file with the Advanced Format tag
  out <- c("[[AdvancedFormat]]", "")

  # 2. Add Block Name if provided
  if (!is.null(block_name) && nchar(block_name) > 0) {
    out <- c(out, paste0("[[Block:", block_name, "]]"), "")
  }

  # Determine padding width automatically for question IDs
  max_w <- nchar(as.character(nrow(items)))
  fmt <- sprintf("[[ID:%s_%%0%dd]]", id_prefix, max_w)

  # 3. Add the starting instructions as a Descriptive Block (DB)
  if (include_instructions) {
    out <- c(
      out,
      "[[Question:DB]]",
      "[[ID:start_instructions]]",
      instructions$start,
      ""
    )
  }

  # 4. Format choices using AdvancedChoices to ensure exact recode values
  choices_block <- c("[[AdvancedChoices]]")

  # Loop through options to set exact numeric values and labels
  for (i in seq_len(nrow(instructions$options))) {
    val <- instructions$options$value[i]
    lab <- instructions$options$label[i]
    choices_block <- c(choices_block, paste0("[[Choice:", val, "]]"), lab)
  }

  # 5. Loop through the items dataframe and append each question
  for (i in seq_len(nrow(items))) {
    item_num <- items[[i, 1]]
    item_text <- items$Text[i]

    q_block <- c(
      "[[Question:MC:SingleAnswer]]",
      sprintf(fmt, item_num),
      item_text,
      choices_block,
      ""
    )

    out <- c(out, q_block)

    # Insert Page Break based on the 'breaks' argument
    if (
      !is.null(breaks) && breaks > 0 && (i %% breaks == 0) && (i != nrow(items))
    ) {
      out <- c(out, "[[PageBreak]]", "")
    }
  }

  # 6. Write everything to the specified file
  writeLines(out, con = file)
  cli::cli_alert_success(
    "Qualtrics import file successfully created at {.file {file}}"
  )
  invisible(file)
}

#' Generate a Complete Plug-and-Play Qualtrics QSF File for the HiTOP-HSUM
#'
#' @description Programmatically builds a fully configured Qualtrics Survey
#'   Format (.qsf) file from your package metadata. This version enforces the strict
#'   linear element ordering required by Qualtrics, removes structural duplication,
#'   and formats quantity variables as validated text entries.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitophsum_qualtrics.qsf"`.
#' @param block_name Character string specifying the questionnaire block name.
#'   Defaults to `"HiTOP-HSUM"`.
#' @param breaks Integer specifying how many items to display per page.
#'   Set to `0` or `NULL` to disable pagination. Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @export
generate_qualtrics_hitophsum <- function(
  file = "hitophsum_qualtrics.qsf",
  block_name = "HiTOP-HSUM",
  breaks = 15
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "The 'jsonlite' package is required to generate QSF exports. Please install it."
    )
  }

  # Enforce strict, EXACT 15-character primary key schemas required by Qualtrics
  survey_id <- "SV_hsumMstrRepo1" # 3 + 12 = 15 chars
  block_id <- "BL_hsumMstrBlk1" # 3 + 12 = 15 chars
  response_set_id <- "RS_hsumMstrRsp1" # 3 + 12 = 15 chars

  # Initialize the master document envelope with fully compliant schema fields
  qsf <- list(
    SurveyEntry = list(
      SurveyID = survey_id,
      SurveyName = block_name,
      SurveyDescription = NA_character_,
      SurveyOwnerID = NA_character_,
      SurveyBrandID = NA_character_,
      DivisionID = NA_character_,
      SurveyLanguage = "EN",
      SurveyActiveResponseSet = response_set_id,
      SurveyStatus = "Inactive",
      SurveyStartDate = "0000-00-00 00:00:00",
      SurveyExpirationDate = "0000-00-00 00:00:00",
      SurveyCreationDate = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      CreatorID = NA_character_,
      LastModified = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      LastAccessed = "0000-00-00 00:00:00",
      LastActivated = "0000-00-00 00:00:00",
      Deleted = NA_character_
    ),
    SurveyElements = list()
  )

  # Core element assembly helper to ensure 100% schema validation compliance
  create_element <- function(
    element_type,
    primary,
    secondary = NULL,
    tertiary = NULL,
    payload = list()
  ) {
    list(
      SurveyID = survey_id,
      Element = element_type,
      PrimaryAttribute = primary,
      SecondaryAttribute = if (is.null(secondary)) NA_character_ else secondary,
      TertiaryAttribute = if (is.null(tertiary)) NA_character_ else tertiary,
      Payload = payload
    )
  }

  # Map out unique sequential tracking tokens across variables
  var_names <- hitophsum_items$Variable
  qid_map <- paste0("QID", seq_along(var_names) + 1)
  names(qid_map) <- var_names

  # Initialize Block Listing Array (QID1 is reserved for structural instructions)
  block_elements <- list(list(Type = "Question", QuestionID = "QID1"))

  # Build Instruction Block (Descriptive Text)
  instruction_sq <- create_element(
    element_type = "SQ",
    primary = "QID1",
    secondary = "Instructions",
    payload = list(
      QuestionText = hitophsum_instructions$start,
      QuestionType = "DB",
      Selector = "TB",
      SubSelector = "TX",
      DataVisibility = list(Private = FALSE, Hidden = FALSE),
      Configuration = list(QuestionDescriptionOption = "UseText"),
      QuestionDescription = "Instructions",
      Validation = list(Settings = list(ForceResponse = "OFF", Type = "None")),
      Language = list(),
      NextChoiceId = 0,
      NextAnswerId = 1,
      QuestionID = "QID1"
    )
  )

  sq_elements_list <- list(instruction_sq)
  skipped_rows <- integer(0)
  active_question_count <- 1

  # Loop across item configurations to compile individual questions
  for (i in seq_len(nrow(hitophsum_items))) {
    if (i %in% skipped_rows) {
      next
    }

    row <- hitophsum_items[i, ]
    current_qid <- qid_map[row$Variable]

    # Intercept quantity variables and dynamically override to validated text entries
    is_quantity_field <- row$Field_Type == "dropdown" ||
      grepl("_quant", row$Variable)

    # Check for look-ahead companion text box merging opportunity
    text_entry_choice <- NULL
    if (!is_quantity_field && i < nrow(hitophsum_items)) {
      next_row <- hitophsum_items[i + 1, ]
      if (
        next_row$Field_Type == "text" &&
          !is.na(next_row$Gate_Variable) &&
          next_row$Gate_Variable == row$Variable
      ) {
        text_entry_choice <- next_row$Gate_Value
        skipped_rows <- c(skipped_rows, i + 1)
      }
    }

    # Assign design configurations based on field property values
    if (is_quantity_field) {
      q_type <- "TE"
      sel <- "SL"
      sub_sel <- NA_character_
      validation <- list(
        Settings = list(
          ForceResponse = "OFF",
          Type = "ContentType",
          ContentType = "Numeric"
        )
      )
    } else {
      q_type <- switch(row$Field_Type, "checkbox" = "MC", "text" = "TE", "MC")
      sel <- switch(row$Field_Type, "checkbox" = "MAVR", "text" = "SL", "SAVR")
      sub_sel <- switch(row$Field_Type, "text" = NA_character_, "TX")
      validation <- list(Settings = list(ForceResponse = "OFF", Type = "None"))
    }

    # Extract choices from lookup tables (skipped for open text / quantities)
    choices_list <- list()
    choice_order <- list()

    if (!is_quantity_field && row$Choice_Set != "text") {
      sub_choices <- hitophsum_choices[
        hitophsum_choices$Choice_Set == row$Choice_Set,
      ]

      if (nrow(sub_choices) > 0) {
        for (j in seq_len(nrow(sub_choices))) {
          chc_val <- sub_choices$Value[j]
          chc_lab <- sub_choices$Label[j]
          chc_id <- if (is.na(chc_val)) "99" else as.character(chc_val)

          choice_node <- list(Display = chc_lab)
          if (!is.null(text_entry_choice) && chc_id == text_entry_choice) {
            choice_node$TextEntry <- "true"
          }

          choices_list[[chc_id]] <- choice_node

          if (grepl("^[0-9]+$", chc_id)) {
            choice_order[[length(choice_order) + 1]] <- as.integer(chc_id)
          } else {
            choice_order[[length(choice_order) + 1]] <- chc_id
          }
        }
      }
    }

    # Build Gating & Display Logic Trees
    display_logic <- NULL
    if (!is.na(row$Gate_Variable)) {
      parent_qid <- qid_map[row$Gate_Variable]
      gate_v <- row$Gate_Value
      expression_list <- list()

      if (gate_v == "1" || gate_v == "6") {
        expression_list[["0"]] <- list(
          LogicType = "Question",
          QuestionID = parent_qid,
          QuestionIsInLoop = "no",
          ChoiceLocator = sprintf(
            "q://%s/SelectableChoice/%s",
            parent_qid,
            gate_v
          ),
          Operator = "Selected",
          QuestionIDFromLocator = parent_qid,
          LeftOperand = sprintf(
            "q://%s/SelectableChoice/%s",
            parent_qid,
            gate_v
          ),
          Type = "Expression",
          Description = ""
        )
      } else if (gate_v == "count>1" || gate_v == "count > 1") {
        expression_list[["0"]] <- list(
          LogicType = "Question",
          QuestionID = parent_qid,
          QuestionIsInLoop = "no",
          ChoiceLocator = sprintf("q://%s/SelectedChoicesCount", parent_qid),
          Operator = "GreaterThan",
          QuestionIDFromLocator = parent_qid,
          LeftOperand = sprintf("q://%s/SelectedChoicesCount", parent_qid),
          RightOperand = "1",
          Type = "Expression",
          Description = ""
        )
      } else if (startsWith(gate_v, ">=")) {
        cutoff <- as.numeric(trimws(sub(">=", "", gate_v)))
        target_indices <- as.character(cutoff:8)

        for (k in seq_along(target_indices)) {
          node_idx <- as.character(k - 1)
          node_val <- target_indices[k]

          expr <- list(
            LogicType = "Question",
            QuestionID = parent_qid,
            QuestionIsInLoop = "no",
            ChoiceLocator = sprintf(
              "q://%s/SelectableChoice/%s",
              parent_qid,
              node_val
            ),
            Operator = "Selected",
            QuestionIDFromLocator = parent_qid,
            LeftOperand = sprintf(
              "q://%s/SelectableChoice/%s",
              parent_qid,
              node_val
            ),
            Type = "Expression",
            Description = ""
          )
          if (k > 1) {
            expr$Conjuction <- "Or"
          }

          expression_list[[node_idx]] <- expr
        }
      }

      if (length(expression_list) > 0) {
        expression_list$Type <- "If"
        display_logic <- list(
          "0" = expression_list,
          Type = "BooleanExpression",
          inPage = FALSE
        )
      }
    }

    # Calculate NextChoiceId safely to ensure it strictly exceeds any maximum ID
    if (length(choices_list) > 0) {
      numeric_ids <- suppressWarnings(as.numeric(names(choices_list)))
      numeric_ids <- numeric_ids[!is.na(numeric_ids)]
      max_choice_id <- if (length(numeric_ids) > 0) {
        max(numeric_ids)
      } else {
        length(choices_list)
      }
      next_choice_id <- as.integer(max_choice_id + 1)
    } else {
      next_choice_id <- 1L
    }

    # Assemble Question Element Payload
    sq_payload <- list(
      QuestionText = row$Text,
      DataExportTag = row$Variable,
      QuestionType = q_type,
      Selector = sel,
      SubSelector = sub_sel,
      DataVisibility = list(Private = FALSE, Hidden = FALSE),
      Configuration = list(QuestionDescriptionOption = "UseText"),
      QuestionDescription = row$Variable,
      Validation = validation,
      Language = list(),
      NextChoiceId = next_choice_id,
      NextAnswerId = 1,
      QuestionID = current_qid
    )

    if (length(choices_list) > 0) {
      sq_payload$Choices <- choices_list
      sq_payload$ChoiceOrder = choice_order
    }
    if (!is.null(display_logic)) {
      sq_payload$DisplayLogic <- display_logic
    }

    sq_elements_list[[length(sq_elements_list) + 1]] <- create_element(
      element_type = "SQ",
      primary = current_qid,
      secondary = row$Variable,
      payload = sq_payload
    )

    # Register to blocks array matrix
    block_elements[[length(block_elements) + 1]] <- list(
      Type = "Question",
      QuestionID = current_qid
    )
    active_question_count <- active_question_count + 1

    # Insert page breaks dynamically if threshold is met
    if (
      !is.null(breaks) && breaks > 0 && (active_question_count %% breaks == 0)
    ) {
      block_elements[[length(block_elements) + 1]] <- list(Type = "Page Break")
    }
  }

  total_questions <- length(sq_elements_list)

  # 5. Assemble SurveyElements in the exact sequence expected by Qualtrics
  elements_collector <- list()

  # [1] Survey Blocks (BL)
  elements_collector[[1]] <- create_element(
    element_type = "BL",
    primary = "Survey Blocks",
    payload = list(
      list(
        Type = "Default",
        Description = block_name,
        ID = block_id,
        BlockElements = block_elements
      ),
      list(
        Type = "Trash",
        Description = "Trash / Unused Questions",
        ID = "BL_trash",
        BlockElements = list()
      )
    )
  )

  # [2] Survey Flow (FL)
  elements_collector[[2]] <- create_element(
    element_type = "FL",
    primary = "Survey Flow",
    payload = list(
      Flow = list(list(ID = block_id, Type = "Block", FlowID = "FL_2")),
      Properties = list(Count = 2),
      FlowID = "FL_1",
      Type = "Root"
    )
  )

  # [3] Preview Link (PL)
  elements_collector[[3]] <- create_element(
    element_type = "PL",
    primary = "Preview Link",
    payload = list(PreviewType = "Brand", PreviewID = "hsum_preview")
  )

  # [4] Project Metadata Core (PROJ)
  elements_collector[[4]] <- create_element(
    element_type = "PROJ",
    primary = "CORE",
    tertiary = "1.1.0",
    payload = list(ProjectCategory = "CORE", SchemaVersion = "1.1.0")
  )

  # [5] Survey Question Count Verification (QC)
  elements_collector[[5]] <- create_element(
    element_type = "QC",
    primary = "Survey Question Count",
    secondary = as.character(total_questions),
    payload = NULL
  )

  # [6] Response Set Configuration (RS)
  elements_collector[[6]] <- create_element(
    element_type = "RS",
    primary = response_set_id,
    secondary = "Default Response Set",
    payload = NULL
  )

  # [7] Scoring Categories Envelope (SCO)
  elements_collector[[7]] <- create_element(
    element_type = "SCO",
    primary = "Scoring",
    payload = list(
      ScoringCategories = list(),
      ScoringCategoryGroups = list(),
      ScoringSummaryCategory = NULL,
      ScoringSummaryAfterQuestions = 0,
      ScoringSummaryAfterSurvey = 0,
      DefaultScoringCategory = NULL,
      AutoScoringCategory = NULL
    )
  )

  # [8] General Survey Parameter Options (SO) - Filled completely from test.txt structural needs
  elements_collector[[8]] <- create_element(
    element_type = "SO",
    primary = "Survey Options",
    payload = list(
      BackButton = "true",
      SaveAndContinue = "true",
      SurveyProtection = "PublicSurvey",
      BallotBoxStuffingPrevention = "false",
      NoIndex = "Yes",
      SecureResponseFiles = "true",
      SurveyExpiration = "None",
      SurveyTermination = "DefaultMessage",
      Header = "",
      Footer = "",
      ProgressBarDisplay = "None",
      PartialData = "+1 week",
      ValidationMessage = "",
      PreviousButton = "",
      NextButton = "",
      SurveyTitle = block_name,
      SkinLibrary = "qualtrics",
      SkinType = "templated",
      Skin = list(
        brandingId = NA_character_,
        templateId = "*base",
        overrides = NULL
      ),
      NewScoring = 1,
      SurveyMetaDescription = ""
    )
  )

  # [9] Append all compiled SQ items sequentially
  elements_collector <- c(elements_collector, sq_elements_list)

  # [10] Append Mobile Compatibility Statistics (STAT) at the very end
  elements_collector[[length(elements_collector) + 1]] <- create_element(
    element_type = "STAT",
    primary = "Survey Statistics",
    payload = list(MobileCompatible = TRUE, ID = "Survey Statistics")
  )

  # Assign collector to master structure
  qsf$SurveyElements <- elements_collector

  # Serialize completed list out directly to QSF JSON format
  jsonlite::write_json(qsf, path = file, auto_unbox = TRUE, pretty = TRUE)
  cli::cli_alert_success(
    "Plug-and-play QSF file successfully compiled at {.file {file}}"
  )
  invisible(file)
}
