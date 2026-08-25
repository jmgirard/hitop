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
#' @examples
#' # Write a HiTOP-BR Qualtrics import file to a temporary location
#' generate_qualtrics_hitopbr(file = tempfile(fileext = ".txt"))
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
#' @param module An optional [hitop_module()] object restricting the file to the
#'   items of the chosen scales, keeping their original HiTOP-SR item numbers.
#'   This is deliberately unlike [generate_docx_hitopsr()], whose module forms
#'   are numbered `1` to `n`: here an item number names a collected data
#'   column, so renumbering would rename variables in dictionaries already in
#'   the field. (default = `NULL`)
#' @param descriptor An optional path to write a module descriptor to, beside
#'   the instrument file. The saved file records which scales the form covers
#'   and which instrument items they draw on, so [read_module()] hands the
#'   module straight back to [score_hitopsr()] at scoring time. A call passing
#'   no `module` writes a descriptor naming every scale, describing the full
#'   administration. Written before the instrument file, so an unwritable path
#'   is reported before any form is produced; if the instrument file then
#'   cannot be written, the descriptor is removed again, a file that was
#'   already at that path included.
#'   It must name a path of its own: an empty string, or the same path as
#'   `file`, is refused rather than leaving you with no descriptor and no
#'   error. (default = `NULL`)
#' @param subset Deprecated. The former name of `module`; supplying it warns.
#'   Supplying both `module` and `subset` is an error. (default = `NULL`)
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @seealso [write_module()] and [read_module()] for the descriptor file.
#'
#' @examples
#' # Write a HiTOP-SR Qualtrics import file to a temporary location
#' generate_qualtrics_hitopsr(file = tempfile(fileext = ".txt"))
#'
#' # A two-scale module, original numbering preserved (unlike the Word form)
#' generate_qualtrics_hitopsr(
#'   file = tempfile(fileext = ".txt"),
#'   module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
#' )
#'
#' @export
generate_qualtrics_hitopsr <- function(
  file = "hitopsr_qualtrics.txt",
  block_name = "HiTOP-SR",
  id_prefix = "HSR",
  include_instructions = TRUE,
  breaks = 15,
  module = NULL,
  descriptor = NULL,
  subset = NULL
) {
  module <- resolve_module_arg(module, subset)
  validate_string(descriptor, "descriptor", allow_null = TRUE)
  validate_descriptor_target(descriptor, file)
  reduced <- apply_module(hitopsr_items, NULL, module, "HSR")

  # Written BEFORE the export, so an unwritable `descriptor` path is reported
  # while no instrument file exists yet. No `item_order`: this export never
  # shuffles, so the items are in instrument order and there is no printed
  # order to record.
  built <- FALSE
  if (!is.null(descriptor)) {
    write_descriptor_sidecar(descriptor, module, "hitopsr")
    # A descriptor with no form beside it describes a form that was never
    # written, so it goes again if the build below fails.
    # file.remove() on the literal path, never unlink(), which would treat a
    # descriptor path holding `*`, `?` or `[` as a wildcard and delete every
    # file it matched.
    on.exit(
      if (!built && file.exists(descriptor)) file.remove(descriptor),
      add = TRUE
    )
  }

  out <- build_qualtrics_txt(
    items = reduced$items,
    instructions = hitopsr_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
  built <- TRUE
  invisible(out)
}

#' Generate a Qualtrics Import File for the PID-5 (Full)
#'
#' @param file Character string specifying the output file path.
#' @param block_name Character string specifying the name of the block in Qualtrics.
#' @param id_prefix Character string specifying the prefix for the question IDs.
#' @param include_instructions Logical. If `TRUE`, includes instructions block.
#' @param breaks Integer or `NULL`. The number of items to display before a page break.
#'
#' @examples
#' # Write a PID-5 (full) Qualtrics import file to a temporary location
#' generate_qualtrics_pid5(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5 <- function(
  file = "pid5_qualtrics.txt",
  block_name = "PID-5",
  id_prefix = "PID5",
  include_instructions = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$FULL), ]
  items <- items[order(items$FULL), ]
  items <- items[, c("FULL", setdiff(names(items), "FULL"))]

  build_qualtrics_txt(
    items = items,
    instructions = pid_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the PID-5-SF
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @examples
#' # Write a PID-5-SF Qualtrics import file to a temporary location
#' generate_qualtrics_pid5sf(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5sf <- function(
  file = "pid5sf_qualtrics.txt",
  block_name = "PID-5-SF",
  id_prefix = "PID5SF",
  include_instructions = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$SF), ]
  items <- items[order(items$SF), ]
  items <- items[, c("SF", setdiff(names(items), "SF"))]

  build_qualtrics_txt(
    items = items,
    instructions = pid_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the PID-5-BF
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @examples
#' # Write a PID-5-BF Qualtrics import file to a temporary location
#' generate_qualtrics_pid5bf(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5bf <- function(
  file = "pid5bf_qualtrics.txt",
  block_name = "PID-5-BF",
  id_prefix = "PID5BF",
  include_instructions = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$BF), ]
  items <- items[order(items$BF), ]
  items <- items[, c("BF", setdiff(names(items), "BF"))]

  build_qualtrics_txt(
    items = items,
    instructions = pid_instructions,
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
  breaks,
  call = rlang::caller_env()
) {
  # The guards live here rather than in each generate_qualtrics_*() wrapper so
  # every instrument gets them from one place; `call` defaults to the wrapper
  # that called this, so the abort blames the exported function the user wrote,
  # not this internal (the score_engine() convention).
  #
  # `block_name` allows NULL because the block-line branch below already treats
  # NULL and "" alike as "write no block line"; guarding it as a plain string
  # would turn a call that works today into an error.
  validate_string(block_name, arg = "block_name", allow_null = TRUE, call = call)
  validate_string(id_prefix, arg = "id_prefix", call = call)
  validate_flag(include_instructions, arg = "include_instructions", call = call)
  validate_count(breaks, arg = "breaks", min = 0, allow_null = TRUE, call = call)

  # 1. Initialize the file with the Advanced Format tag
  out <- c("[[AdvancedFormat]]", "")

  # 2. Add Block Name if provided
  if (!is.null(block_name) && nchar(block_name) > 0) {
    out <- c(out, paste0("[[Block:", block_name, "]]"), "")
  }

  # Determine padding width automatically for question IDs. The width comes
  # from the largest item NUMBER, not the row count: a module keeps original
  # numbering, so its row count under-pads (item 7 as `_07` beside `_312`).
  # For a full instrument the two agree, so existing output is unchanged.
  max_w <- nchar(as.character(max(items[[1]])))
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
