#' Generate a Qualtrics Advanced Format Import File
#'
#' Creates a text file formatted for the Qualtrics Advanced Format import tool.
#' It takes a data frame of survey items and a list of instructions, combining
#' them into a structured text file with specific block and question tags. This
#' function is designed to work seamlessly with the package's built-in datasets.
#'
#' @param items A data frame containing the survey items. The first column must
#'   contain the item identifiers (e.g., item numbers), and a column named
#'   `Text` must contain the question text.
#' @param instructions A list containing the survey instructions and response
#'   options. It must include a `start` character string for the initial
#'   descriptive block, and an `options` data frame containing `value` and
#'   `label` columns for the multiple-choice options.
#' @param file_path A character string specifying the output file path. Defaults
#'   to `"qualtrics_import.txt"`.
#'
#' @return Invisibly returns `NULL`. The function creates a text file at the
#'   specified `file_path` and prints a success message to the console.
#'
#' @importFrom cli cli_inform
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate a Qualtrics text file using the package's built-in datasets
#' generate_qualtrics_txt(
#'   items = hitopbr_items,
#'   instructions = hitopbr_instructions,
#'   file_path = "hitopbr_survey.txt"
#' )
#' }
generate_qualtrics_txt <- function(
  items,
  instructions,
  file_path = "qualtrics_import.txt"
) {
  # 1. Initialize the file with the Advanced Format tag
  out <- c("[[AdvancedFormat]]", "")

  instrument <- colnames(items)[[1]]
  max_w <- max(nchar(nrow(items)))
  fmt <- sprintf("[[ID:%s_%%0%dd]]", instrument, max_w)

  # 2. Add the starting instructions as a Descriptive Block (DB)
  out <- c(
    out,
    "[[Question:DB]]",
    "[[ID:start_instructions]]",
    instructions$start,
    ""
  )

  # 3. Format choices using AdvancedChoices to ensure exact recode values
  choices_block <- c("[[AdvancedChoices]]")

  # Loop through options to set exact numeric values and labels
  for (i in seq_len(nrow(instructions$options))) {
    val <- instructions$options$value[i]
    lab <- instructions$options$label[i]
    choices_block <- c(choices_block, paste0("[[Choice:", val, "]]"), lab)
  }

  # 4. Loop through the items dataframe and append each question
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
  }

  # 5. Write everything to the specified file
  writeLines(out, con = file_path)
  cli::cli_inform("Successfully created Qualtrics import file at: {file_path}")
}
