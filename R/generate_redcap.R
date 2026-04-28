#' Generate a REDCap Instrument ZIP File for the HiTOP-BR
#'
#' @description Generates a REDCap-compatible data dictionary for the
#'   Hierarchical Taxonomy of Psychopathology - Brief Report (HiTOP-BR) and
#'   packages it into an Instrument ZIP file for easy uploading.
#'
#' @details **Importing (Uploading) Instruments into a REDCap Project:**
#' 1. Log in to your REDCap account and navigate to the desired project.
#' 2. Click on the "Designer" link in the left menu bar under "Project Home and
#'   Design".
#' 3. In the main page, under "Data Collection Instruments", look for the
#'   "Upload instrument ZIP" option and click the "Upload" button.
#' 4. Click "Choose File", navigate to where you have the measure saved as a ZIP
#'   folder, and select the ZIP folder containing your instrument.
#' 5. Click "Upload instrument ZIP" button to begin the import process.
#' 6. Find the imported instrument in your list of measures and review for
#'   accuracy.
#' 7. Test the instrument to ensure proper functionality within your project.
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
#' @details **Importing (Uploading) Instruments into a REDCap Project:**
#' 1. Log in to your REDCap account and navigate to the desired project.
#' 2. Click on the "Designer" link in the left menu bar under "Project Home and
#'   Design".
#' 3. In the main page, under "Data Collection Instruments", look for the
#'   "Upload instrument ZIP" option and click the "Upload" button.
#' 4. Click "Choose File", navigate to where you have the measure saved as a ZIP
#'   folder, and select the ZIP folder containing your instrument.
#' 5. Click "Upload instrument ZIP" button to begin the import process.
#' 6. Find the imported instrument in your list of measures and review for
#'   accuracy.
#' 7. Test the instrument to ensure proper functionality within your project.
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
