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
