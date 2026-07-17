#' Generate a REDCap Instrument ZIP File for the HiTOP-BR
#'
#' @description Generates a REDCap-compatible data dictionary for the
#'   Hierarchical Taxonomy of Psychopathology - Brief Report (HiTOP-BR) and
#'   packages it into an Instrument ZIP file for easy uploading.
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @param file Character string. The destination path for the output ZIP file.
#'   Defaults to `"hitopbr_redcap.zip"`.
#' @param form_name Character string. The internal name of the form in REDCap.
#'   Defaults to `"hitopbr_questionnaire"`.
#' @param required Logical. Whether the items should be marked as required.
#'   Defaults to `TRUE`.
#' @param breaks Integer or `NULL`. The number of items to display before
#'   inserting a page break. Set to `0` or `NULL` to disable pagination
#'   entirely. Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' # Write a HiTOP-BR REDCap instrument ZIP to a temporary location
#' generate_redcap_hitopbr(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_hitopbr <- function(
  file = "hitopbr_redcap.zip",
  form_name = "hitopbr_questionnaire",
  required = TRUE,
  breaks = 15
) {
  build_redcap_zip(
    items = hitopbr_items,
    instructions = hitopbr_instructions,
    file = file,
    instrument = "HBR",
    form_name = form_name,
    required = required,
    breaks = breaks
  )
}

#' Generate a REDCap Instrument ZIP File for the HiTOP-SR
#'
#' @description Generates a REDCap-compatible data dictionary for the
#'   Hierarchical Taxonomy of Psychopathology - Self-Report (HiTOP-SR) and
#'   packages it into an Instrument ZIP file for easy uploading.
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @param file Character string. The destination path for the output ZIP file.
#'   Defaults to `"hitopsr_redcap.zip"`.
#' @param form_name Character string. The internal name of the form in REDCap.
#'   Defaults to `"hitopsr_questionnaire"`.
#' @param required Logical. Whether the items should be marked as required.
#'   Defaults to `TRUE`.
#' @param breaks Integer or `NULL`. The number of items to display before
#'   inserting a page break. Set to `0` or `NULL` to disable pagination
#'   entirely. Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' # Write a HiTOP-SR REDCap instrument ZIP to a temporary location
#' generate_redcap_hitopsr(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_hitopsr <- function(
  file = "hitopsr_redcap.zip",
  form_name = "hitopsr_questionnaire",
  required = TRUE,
  breaks = 15
) {
  build_redcap_zip(
    items = hitopsr_items,
    instructions = hitopsr_instructions,
    file = file,
    instrument = "HSR",
    form_name = form_name,
    required = required,
    breaks = breaks
  )
}

#' Generate a REDCap Instrument ZIP File for the PID-5 (Full)
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @param file Character string. The destination path for the output ZIP file.
#' @param form_name Character string. The internal name of the form in REDCap.
#' @param required Logical. Whether the items should be marked as required.
#' @param breaks Integer or `NULL`. The number of items to display before a page break.
#'
#' @examples
#' # Write a PID-5 (full) REDCap instrument ZIP to a temporary location
#' generate_redcap_pid5(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_pid5 <- function(
  file = "pid5_redcap.zip",
  form_name = "pid5_questionnaire",
  required = TRUE,
  breaks = 15
) {
  # Filter to non-missing FULL items and sort
  items <- pid_items[!is.na(pid_items$FULL), ]
  items <- items[order(items$FULL), ]

  # Relocate the FULL column to the first position
  items <- items[, c("FULL", setdiff(names(items), "FULL"))]

  build_redcap_zip(
    items = items,
    instructions = pid_instructions,
    file = file,
    instrument = "PID5",
    form_name = form_name,
    required = required,
    breaks = breaks
  )
}

#' Generate a REDCap Instrument ZIP File for the PID-5-SF
#'
#' @inheritParams generate_redcap_pid5
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @examples
#' # Write a PID-5-SF REDCap instrument ZIP to a temporary location
#' generate_redcap_pid5sf(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_pid5sf <- function(
  file = "pid5sf_redcap.zip",
  form_name = "pid5sf_questionnaire",
  required = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$SF), ]
  items <- items[order(items$SF), ]
  items <- items[, c("SF", setdiff(names(items), "SF"))]

  build_redcap_zip(
    items = items,
    instructions = pid_instructions,
    file = file,
    instrument = "PID5SF",
    form_name = form_name,
    required = required,
    breaks = breaks
  )
}

#' Generate a REDCap Instrument ZIP File for the PID-5-BF
#'
#' @inheritParams generate_redcap_pid5
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @examples
#' # Write a PID-5-BF REDCap instrument ZIP to a temporary location
#' generate_redcap_pid5bf(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_pid5bf <- function(
  file = "pid5bf_redcap.zip",
  form_name = "pid5bf_questionnaire",
  required = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$BF), ]
  items <- items[order(items$BF), ]
  items <- items[, c("BF", setdiff(names(items), "BF"))]

  build_redcap_zip(
    items = items,
    instructions = pid_instructions,
    file = file,
    instrument = "PID5BF",
    form_name = form_name,
    required = required,
    breaks = breaks
  )
}

# Internal Helper: Build the REDCap ZIP file
build_redcap_zip <- function(
  items,
  instructions,
  file,
  instrument,
  form_name,
  required,
  breaks
) {
  # 1. Format the REDCap choices string from instructions
  choice_pairs <- paste(
    instructions$options$value,
    instructions$options$label,
    sep = ", "
  )
  formatted_choices <- paste(choice_pairs, collapse = " | ")

  # 2. Determine the padding width automatically
  max_w <- max(nchar(items[[1]]), na.rm = TRUE)

  # 3. Create the zero-padded format string and apply it
  fmt <- sprintf("%%0%dd", max_w)
  padded_num <- sprintf(fmt, items[[1]])

  # 4. Build the complete Data Dictionary data frame for items
  item_rows <- data.frame(
    `Variable / Field Name` = paste0(tolower(instrument), "_", padded_num),
    `Form Name` = form_name,
    `Section Header` = NA_character_,
    `Field Type` = "radio",
    `Field Label` = items$Text,
    `Choices, Calculations, OR Slider Labels` = formatted_choices,
    `Field Note` = NA_character_,
    `Text Validation Type OR Show Slider Number` = NA_character_,
    `Text Validation Min` = NA_character_,
    `Text Validation Max` = NA_character_,
    `Identifier?` = NA_character_,
    `Branching Logic (Show field only if...)` = NA_character_,
    `Required Field?` = ifelse(required, "y", "n"),
    `Custom Alignment` = NA_character_,
    `Question Number (surveys only)` = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 5. Insert Page Breaks based on the 'breaks' argument
  if (!is.null(breaks) && breaks > 0 && nrow(item_rows) > breaks) {
    # Find the positions where breaks should occur
    break_positions <- seq(from = breaks + 1, to = nrow(item_rows), by = breaks)

    # REDCap triggers a page break when there is text in the Section Header column
    item_rows$`Section Header`[break_positions] <- "<br>"
  }

  # 6. Build the Instructions Row
  instruction_text <- instructions$start[1]
  if (
    !is.null(instruction_text) &&
      !is.na(instruction_text) &&
      nchar(instruction_text) > 0
  ) {
    instruction_row <- data.frame(
      `Variable / Field Name` = paste0(tolower(instrument), "_instructions"),
      `Form Name` = form_name,
      `Section Header` = NA_character_,
      `Field Type` = "descriptive",
      `Field Label` = instruction_text,
      `Choices, Calculations, OR Slider Labels` = NA_character_,
      `Field Note` = NA_character_,
      `Text Validation Type OR Show Slider Number` = NA_character_,
      `Text Validation Min` = NA_character_,
      `Text Validation Max` = NA_character_,
      `Identifier?` = NA_character_,
      `Branching Logic (Show field only if...)` = NA_character_,
      `Required Field?` = NA_character_,
      `Custom Alignment` = NA_character_,
      `Question Number (surveys only)` = NA_character_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # Bind the instructions to the top of the items
    data_dictionary <- rbind(instruction_row, item_rows)
  } else {
    data_dictionary <- item_rows
  }

  # 7. Export directly to ZIP
  if (!grepl("^/|^[A-Za-z]:", file)) {
    absolute_file_path <- file.path(getwd(), file)
  } else {
    absolute_file_path <- file
  }

  temp_csv <- file.path(tempdir(), "instrument.csv")
  write.csv(data_dictionary, file = temp_csv, row.names = FALSE, na = "")

  utils::zip(
    zipfile = absolute_file_path,
    files = temp_csv,
    extras = c("-q", "-j")
  )

  file.remove(temp_csv)

  cli::cli_alert_success("Instrument successfully zipped to {.file {file}}")

  invisible(file)
}

#' Generate a REDCap Instrument ZIP File for the HiTOP-HSUM
#'
#' @description Generates a REDCap-compatible data dictionary for the
#'   Hierarchical Taxonomy of Psychopathology - Substance Use Module (HiTOP-HSUM)
#'   and packages it into an Instrument ZIP file for easy uploading.
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @param file Character string. The destination path for the output ZIP file.
#'   Defaults to `"hitophsum_redcap.zip"`.
#' @param form_name Character string. The internal name of the form in REDCap.
#'   Defaults to `"hitophsum_questionnaire"`.
#' @param required Logical. Whether the items should be marked as required.
#'   Defaults to `TRUE`.
#' @param other_drug_rule Character string. How symptom items are gated for
#'   the "other drug" substances (every substance except alcohol and
#'   nicotine). The default, `"most_frequent"`, follows the source module's
#'   looping rule: symptom items are shown only for the most frequently used
#'   other drug that is used at least monthly (drugs whose frequency is
#'   "Prefer not to say" are ignored; when two or more drugs tie for the
#'   highest frequency, the symptom items are shown for each of them).
#'   `"per_drug"` loosens the gate so that every other drug used at least
#'   monthly gets its own symptom items, regardless of relative frequency.
#'   Alcohol and nicotine symptom gating is unaffected by this argument.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' # Write a HiTOP-HSUM REDCap instrument ZIP to a temporary location
#' generate_redcap_hitophsum(file = tempfile(fileext = ".zip"))
#'
#' @export
generate_redcap_hitophsum <- function(
  file = "hitophsum_redcap.zip",
  form_name = "hitophsum_questionnaire",
  required = TRUE,
  other_drug_rule = c("most_frequent", "per_drug")
) {
  other_drug_rule <- match.arg(other_drug_rule)

  # 1. Format choices strings using split and vapply
  choice_pairs <- paste(
    hitophsum_choices$Value,
    hitophsum_choices$Label,
    sep = ", "
  )
  choices_list <- split(choice_pairs, hitophsum_choices$Choice_Set)
  choices_vector <- vapply(
    choices_list,
    function(x) paste(x, collapse = " | "),
    character(1)
  )

  # 2. Map parent field attributes using named vectors
  unique_items <- hitophsum_items[!duplicated(hitophsum_items$Variable), ]

  resolved_types <- tolower(unique_items$Field_Type)
  resolved_types[unique_items$Choice_Set == "yn_pnts"] <- "radio"

  parent_types <- setNames(resolved_types, unique_items$Variable)
  parent_charsets <- setNames(unique_items$Choice_Set, unique_items$Variable)

  # 3. Parse Vectorized Branching Logic
  branching_logic <- mapply(
    function(gate_var, gate_val) {
      if (
        is.na(gate_var) || gate_var == "" || is.na(gate_val) || gate_val == ""
      ) {
        return(NA_character_)
      }

      p_type <- parent_types[gate_var]
      p_charset <- parent_charsets[gate_var]

      if (is.na(p_type)) {
        return(NA_character_)
      }

      # Clean string whitespace
      gate_val <- trimws(gate_val)

      # Rule Type A: Matrix count rule (count>1)
      if (gate_val == "count>1") {
        p_values <- hitophsum_choices$Value[
          hitophsum_choices$Choice_Set == p_charset
        ]
        if (length(p_values) == 0) {
          return(NA_character_)
        }

        if (p_type == "checkbox") {
          conds <- paste0("if([", gate_var, "(", p_values, ")],1,0)")
          return(paste0("(", paste(conds, collapse = " + "), ") > 1"))
        } else {
          return(NA_character_)
        }
      }

      # Rule Type B: Handle comparison operators (e.g., >=4, !=1, >1)
      if (grepl("^(>=|<=|>|<|!=)", gate_val)) {
        operator <- regmatches(gate_val, regexec("^(>=|<=|>|<|!=)", gate_val))[[
          1
        ]][1]
        val_part <- trimws(sub(operator, "", gate_val, fixed = TRUE))
        num_threshold <- suppressWarnings(as.numeric(val_part))

        if (p_type == "checkbox") {
          p_values <- hitophsum_choices$Value[
            hitophsum_choices$Choice_Set == p_charset
          ]

          if (operator == "!=") {
            matched_vals <- p_values[p_values != num_threshold]
          } else if (operator == ">") {
            matched_vals <- p_values[p_values > num_threshold]
          } else if (operator == ">=") {
            matched_vals <- p_values[p_values >= num_threshold]
          } else if (operator == "<") {
            matched_vals <- p_values[p_values < num_threshold]
          } else if (operator == "<=") {
            matched_vals <- p_values[p_values <= num_threshold]
          } else {
            matched_vals <- integer(0)
          }

          matched_vals <- matched_vals[
            !is.na(matched_vals) & matched_vals != 99
          ]
          if (length(matched_vals) == 0) {
            return(NA_character_)
          }

          conds <- paste0("[", gate_var, "(", matched_vals, ")] = '1'")
          return(paste0("(", paste(conds, collapse = " OR "), ")"))
        } else {
          if (!is.na(num_threshold)) {
            base <- paste0("[", gate_var, "] ", operator, " ", val_part)
            # A "Prefer not to say" value (99) satisfies any usable
            # threshold numerically, so guard it out of comparison gates.
            p_values <- hitophsum_choices$Value[
              hitophsum_choices$Choice_Set == p_charset
            ]
            if (any(p_values == 99, na.rm = TRUE)) {
              base <- paste0(base, " and [", gate_var, "] <> '99'")
            }
            return(base)
          } else {
            return(paste0("[", gate_var, "] ", operator, " '", val_part, "'"))
          }
        }
      }

      # Rule Type C: Handle discrete options (e.g., 2,4,5)
      vals <- trimws(strsplit(gate_val, "[,|]")[[1]])
      vals <- vals[vals != ""]

      if (p_type == "checkbox") {
        conds <- paste0("[", gate_var, "(", vals, ")] = '1'")
      } else {
        conds <- paste0("[", gate_var, "] = '", vals, "'")
      }

      if (length(conds) > 1) {
        return(paste0("(", paste(conds, collapse = " OR "), ")"))
      } else {
        return(conds)
      }
    },
    hitophsum_items$Gate_Variable,
    hitophsum_items$Gate_Value,
    USE.NAMES = FALSE
  )

  # 4. Apply the other-drug looping rule: under "most_frequent", a symptom
  # item for an other drug (any substance but alcohol/nicotine) is shown only
  # if no competing other drug has a strictly higher usable frequency (99 =
  # Prefer not to say never competes; ties show every tied drug).
  is_symptom <- grepl("^Symptom", hitophsum_items$Tier)
  is_other_drug <- !hitophsum_items$Substance %in% c("Alcohol", "Nicotine")
  if (other_drug_rule == "most_frequent") {
    other_freq_vars <- unique(
      hitophsum_items$Gate_Variable[is_symptom & is_other_drug]
    )
    for (i in which(is_symptom & is_other_drug)) {
      self <- hitophsum_items$Gate_Variable[i]
      competitors <- setdiff(other_freq_vars, self)
      outranked <- paste0(
        "if([", competitors, "] <> '99' and [", competitors, "] > [",
        self, "],1,0)"
      )
      branching_logic[i] <- paste0(
        branching_logic[i],
        " and (", paste(outranked, collapse = " + "), ") = 0"
      )
    }
  }

  # 5. Build Final Choice Strings and Field Types
  choices_string_vec <- unname(choices_vector[hitophsum_items$Choice_Set])
  final_field_types <- unname(parent_types[hitophsum_items$Variable])
  choices_string_vec[final_field_types == "text"] <- NA_character_

  # Every choice-bearing field must resolve a choice set; an unresolved one
  # would import into REDCap as an invalid field.
  missing_choices <- final_field_types %in% c("radio", "dropdown", "checkbox") &
    is.na(choices_string_vec)
  if (any(missing_choices)) {
    cli::cli_abort(
      "No choices found for field{?s} {.val {hitophsum_items$Variable[missing_choices]}}."
    )
  }

  # 6. Build the complete Data Dictionary data frame for items
  item_rows <- data.frame(
    `Variable / Field Name` = hitophsum_items$Variable,
    `Form Name` = form_name,
    `Section Header` = NA_character_,
    `Field Type` = final_field_types,
    `Field Label` = hitophsum_items$Text,
    `Choices, Calculations, OR Slider Labels` = choices_string_vec,
    `Field Note` = NA_character_,
    `Text Validation Type OR Show Slider Number` = NA_character_,
    `Text Validation Min` = NA_character_,
    `Text Validation Max` = NA_character_,
    `Identifier?` = NA_character_,
    `Branching Logic (Show field only if...)` = branching_logic,
    `Required Field?` = ifelse(
      required & final_field_types != "descriptive",
      "y",
      "n"
    ),
    `Custom Alignment` = NA_character_,
    `Question Number (surveys only)` = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 7. Build the Instructions Row
  instruction_text <- hitophsum_instructions$start[1]
  if (
    !is.null(instruction_text) &&
      !is.na(instruction_text) &&
      nchar(instruction_text) > 0
  ) {
    instruction_row <- data.frame(
      `Variable / Field Name` = "hitophsum_instructions",
      `Form Name` = form_name,
      `Section Header` = NA_character_,
      `Field Type` = "descriptive",
      `Field Label` = instruction_text,
      `Choices, Calculations, OR Slider Labels` = NA_character_,
      `Field Note` = NA_character_,
      `Text Validation Type OR Show Slider Number` = NA_character_,
      `Text Validation Min` = NA_character_,
      `Text Validation Max` = NA_character_,
      `Identifier?` = NA_character_,
      `Branching Logic (Show field only if...)` = NA_character_,
      `Required Field?` = NA_character_,
      `Custom Alignment` = NA_character_,
      `Question Number (surveys only)` = NA_character_,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    data_dictionary <- rbind(instruction_row, item_rows)
  } else {
    data_dictionary <- item_rows
  }

  # 9. Export directly to ZIP
  if (!grepl("^/|^[A-Za-z]:", file)) {
    absolute_file_path <- file.path(getwd(), file)
  } else {
    absolute_file_path <- file
  }

  temp_csv <- file.path(tempdir(), "instrument.csv")
  write.csv(data_dictionary, file = temp_csv, row.names = FALSE, na = "")

  utils::zip(
    zipfile = absolute_file_path,
    files = temp_csv,
    extras = c("-q", "-j")
  )

  file.remove(temp_csv)

  cli::cli_alert_success("Instrument successfully zipped to {.file {file}}")

  invisible(file)
}
