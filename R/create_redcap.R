create_redcap_csv <- function(
  items_df,
  instructions_df,
  form_name = NULL,
  field_type = "radio",
  required_field = TRUE,
  file_path = NULL
) {
  instrument <- colnames(items_df)[[1]]
  if (is.null(form_name)) {
    form_name <- paste0(tolower(instrument), "_questionnaire")
  }
  if (is.null(file_path)) {
    file_path <- paste0(tolower(instrument), "_redcap.csv")
  }
  # 1. Format the REDCap choices string from instructions
  choice_pairs <- paste(
    instructions_df$options$value,
    instructions_df$options$label,
    sep = ", "
  )
  formatted_choices <- paste(choice_pairs, collapse = " | ")

  # 2. Determine the padding width automatically
  max_w <- max(nchar(items_df[[1]]), na.rm = TRUE)

  # 3. Create the zero-padded format string and apply it
  fmt <- sprintf("%%0%dd", max_w)
  padded_num <- sprintf(fmt, items_df[[1]])

  # 4. Build the complete Data Dictionary data frame
  item_rows <- data.frame(
    `Variable / Field Name` = paste0(tolower(instrument), "_", padded_num),
    `Form Name` = form_name,
    `Section Header` = NA_character_,
    `Field Type` = field_type,
    `Field Label` = items_df$Text,
    `Choices, Calculations, OR Slider Labels` = formatted_choices,
    `Field Note` = NA_character_,
    `Text Validation Type OR Show Slider Number` = NA_character_,
    `Text Validation Min` = NA_character_,
    `Text Validation Max` = NA_character_,
    `Identifier?` = NA_character_,
    `Branching Logic (Show field only if...)` = NA_character_,
    `Required Field?` = ifelse(required_field, "y", "n"),
    `Custom Alignment` = NA_character_,
    `Question Number (surveys only)` = NA_character_,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  instruction_text <- instructions_df$start[1]
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

  # 5. Export to CSV automatically
  if (!is.null(file_path)) {
    write.csv(data_dictionary, file = file_path, row.names = FALSE, na = "")
    cli::cli_inform("Data dictionary successfully written to: {file_path}")
  }

  # Return the data frame invisibly
  invisible(data_dictionary)
}
