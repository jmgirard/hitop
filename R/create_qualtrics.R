generate_qualtrics_txt <- function(
  items_df,
  instructions_list,
  file_path = "qualtrics_import.txt"
) {
  # 1. Initialize the file with the Advanced Format tag
  out <- c("[[AdvancedFormat]]", "")

  instrument <- colnames(items_df)[[1]]
  max_w <- max(nchar(nrow(items_df)))
  fmt <- sprintf("[[ID:%s_%%0%dd]]", instrument, max_w)

  # 2. Add the starting instructions as a Descriptive Block (DB)
  out <- c(
    out,
    "[[Question:DB]]",
    "[[ID:start_instructions]]",
    instructions_list$start,
    ""
  )

  # 3. Format choices using AdvancedChoices to ensure exact recode values
  choices_block <- c("[[AdvancedChoices]]")

  # Loop through options to set exact numeric values and labels
  for (i in seq_len(nrow(instructions_list$options))) {
    val <- instructions_list$options$value[i]
    lab <- instructions_list$options$label[i]
    choices_block <- c(choices_block, paste0("[[Choice:", val, "]]"), lab)
  }

  # 4. Loop through the items dataframe and append each question
  for (i in seq_len(nrow(items_df))) {
    item_num <- items_df[[i, 1]]
    item_text <- items_df$Text[i]

    q_block <- c(
      "[[Question:MC:SingleAnswer]]",
      sprintf(fmt, item_num),
      item_text,
      choices_block,
      ""
    )

    out <- c(out, q_block)
  }

  # 5. Write everything to the specified file
  writeLines(out, con = file_path)
  cli::cli_inform("Successfully created Qualtrics import file at: {file_path}")
}
