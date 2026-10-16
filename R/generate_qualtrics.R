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
    max_n = max(hitopbr_items$HBR),
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
#'   the field. The zero-padding is the full instrument's for the same reason:
#'   item 4 is `HSR_004` in a module file as in the complete one, never
#'   `HSR_04`. A module whose `items`, `nItems`, `camelCase` or `instrument` differ from a
#'   fresh [hitop_module()] build of its scales, or that cannot be rebuilt
#'   from its `instrument` and `scales`, is refused with an error of class
#'   `hitop_module_mismatch`, before any file is written.
#'   (default = `NULL`)
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
#'   error. Once both files are on disk the descriptor's path is announced on
#'   the console, after the message naming the import file.
#'   The descriptor's `columns` field holds the names that the generated file
#'   assigns to the items: the questions' `[[ID:]]` values, such as `HSR_066`
#'   (built from `id_prefix`). A Qualtrics data export with the item variable
#'   names as column headers uses these names. Exported as numeric values
#'   rather than choice text, it can be scored by [score_hitopsr()] and
#'   [reliability_hitopsr()] from the module [read_module()] returns, with
#'   `items` omitted. Pass `items` for data whose columns carry
#'   other names, such as an export made with "Use internal IDs in header" or
#'   a question renamed after import. (default = `NULL`)
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
# Source for the `descriptor` help's column-name claim: a first-hand Qualtrics
# export of one generated two-scale module with the default `id_prefix`,
# `cairn/references/qualtrics2026exportheader.md`.
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
    # The item questions' `[[ID:]]` values, which Qualtrics uses as the data
    # export's column names. build_qualtrics_txt() writes the same IDs below.
    # `id_prefix` is used here before the builder checks it, so it is checked
    # here first, with the same check the builder makes.
    validate_string(id_prefix, arg = "id_prefix")
    write_descriptor_sidecar(
      descriptor,
      module,
      "hitopsr",
      columns = qualtrics_item_ids(
        id_prefix,
        reduced$items[[1]],
        max(hitopsr_items$HSR)
      )
    )
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
    max_n = max(hitopsr_items$HSR),
    instructions = hitopsr_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
  built <- TRUE

  # Said only once the form is on disk. The sidecar was written before the
  # build and is removed again if the build fails, so an alert any earlier
  # could name a descriptor that no longer exists when the call returns.
  if (!is.null(descriptor)) {
    cli::cli_alert_success(
      "Module descriptor successfully written to {.file {descriptor}}"
    )
  }

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
    max_n = max(pid_items$FULL, na.rm = TRUE),
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
    max_n = max(pid_items$SF, na.rm = TRUE),
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
    max_n = max(pid_items$BF, na.rm = TRUE),
    instructions = pid_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the PID5BF+M
#'
#' The 36 items of the modified brief form of the PID-5 (PID5BF+M), in BF+M
#' order with their PID-5 text, and the instructions and response options of
#' the other PID-5 forms. Score the export with
#' `score_pid5(version = "BFPM")`.
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @examples
#' # Write a PID5BF+M Qualtrics import file to a temporary location
#' generate_qualtrics_pid5bfpm(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5bfpm <- function(
  file = "pid5bfpm_qualtrics.txt",
  block_name = "PID5BF+M",
  id_prefix = "PID5BFPM",
  include_instructions = TRUE,
  breaks = 15
) {
  items <- pid_items[!is.na(pid_items$BFPM), ]
  items <- items[order(items$BFPM), ]
  items <- items[, c("BFPM", setdiff(names(items), "BFPM"))]

  build_qualtrics_txt(
    items = items,
    max_n = max(pid_items$BFPM, na.rm = TRUE),
    instructions = pid_instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the PID-5 Informant Form
#'
#' The 218 items of the PID-5 Informant Form (PID-5-IRF), in the form's order
#' with the informant wording (`pid_items$TextIRF`). The instructions block
#' ends with the form's rating prompt and the stem "He or she…" that each
#' item completes, and the response options are the form's 0 to 3 labels.
#' With `breaks`, a descriptive block restating the prompt and stem opens
#' every page after the first. The
#' question IDs are `PID5IRF_001` to `PID5IRF_218`. Score the export with
#' `score_pid5(version = "IRF")`.
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @examples
#' # Write a PID-5 Informant Form Qualtrics import file to a temporary location
#' generate_qualtrics_pid5irf(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5irf <- function(
  file = "pid5irf_qualtrics.txt",
  block_name = "PID-5-IRF",
  id_prefix = "PID5IRF",
  include_instructions = TRUE,
  breaks = 15
) {
  form <- pid_irf_form()

  build_qualtrics_txt(
    items = form$items,
    max_n = max(pid_items$IRF, na.rm = TRUE),
    instructions = form$instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks,
    page_header = form$page_header
  )
}

#' Generate a Qualtrics Import File for the PID-5 Child Form (Ages 11 to 17)
#'
#' The 220 items of the PID-5 child form for ages 11 to 17, which are the
#' adult full form's items in the same order with the same keying, and the
#' child form's instructions and response options. The question IDs are the
#' adult form's, `PID5_001` to `PID5_220`, so the export scores with
#' `score_pid5(version = "FULL")` as an adult export does.
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2013). *The Personality Inventory for DSM-5
#'   (PID-5)—Child Age 11–17*. American Psychiatric Association.
#'
#' @examples
#' # Write a PID-5 child form Qualtrics import file to a temporary location
#' generate_qualtrics_pid5child(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5child <- function(
  file = "pid5child_qualtrics.txt",
  block_name = "PID-5 Child",
  id_prefix = "PID5",
  include_instructions = TRUE,
  breaks = 15
) {
  form <- pid_child_form("FULL")

  build_qualtrics_txt(
    items = form$items,
    max_n = max(pid_items$FULL, na.rm = TRUE),
    instructions = form$instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the PID-5-BF Child Form (Ages 11 to 17)
#'
#' The 25 items of the PID-5-BF child form for ages 11 to 17, which are the
#' adult brief form's items in the same order with the same keying, and the
#' child form's instructions and response options. The question IDs are the
#' adult form's, `PID5BF_01` to `PID5BF_25`, so the export scores with
#' `score_pid5(version = "BF")` as an adult export does.
#'
#' @inheritParams generate_qualtrics_pid5
#'
#' @references Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
#'   Skodol, A. E. (2013). *The Personality Inventory for DSM-5—Brief Form
#'   (PID-5-BF)—Child Age 11–17*. American Psychiatric Association.
#'
#' @examples
#' # Write a PID-5-BF child form Qualtrics import file to a temporary location
#' generate_qualtrics_pid5bfchild(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_pid5bfchild <- function(
  file = "pid5bfchild_qualtrics.txt",
  block_name = "PID-5-BF Child",
  id_prefix = "PID5BF",
  include_instructions = TRUE,
  breaks = 15
) {
  form <- pid_child_form("BF")

  build_qualtrics_txt(
    items = form$items,
    max_n = max(pid_items$BF, na.rm = TRUE),
    instructions = form$instructions,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks
  )
}

#' Generate a Qualtrics Import File for the HiTOP-DAT
#'
#' @description Creates a text file formatted for the Qualtrics Advanced Format
#'   import tool containing the 382 items of the HiTOP Digital Assessment and
#'   Tracker (HiTOP-DAT) battery. The item questions are `hdat_001` to
#'   `hdat_382` in battery order (`hitopdat_items$Item`), each a single-answer
#'   multiple-choice question whose choices are the values and labels of the
#'   item's own answer set (`hitopdat_choices`). Each answer carries its
#'   scoring value from `hitopdat_choices$Value` as its Qualtrics recode
#'   value, written in the file as the answer's `[[Choice:<value>]]` tag.
#'   Each of the seven measures opens with a descriptive text question holding its instruction text,
#'   except the CAPE, which has none. Its ID is
#'   `<id_prefix>_<measure>_instructions`, where `<measure>` is the measure's
#'   name in lower case with each run of other characters made `_`
#'   (`hdat_whodas_instructions`, `hdat_idas_ii_instructions`). Each measure
#'   after the first starts a new page.
#'
#' @seealso Step-by-step import instructions for Qualtrics and REDCap:
#'   \url{https://jmgirard.github.io/hitop/articles/import-instructions.html}
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitopdat_qualtrics.txt"`.
#' @param block_name Character string specifying the name of the block in
#'   Qualtrics. Defaults to `"HiTOP-DAT"`.
#' @param id_prefix Character string specifying the prefix for the question IDs.
#'   Defaults to `"hdat"`.
#' @param include_instructions Logical. If `TRUE` (default), includes each
#'   measure's instruction text as a descriptive text question.
#' @param breaks Integer or `NULL`. The number of items of a measure to
#'   display before inserting a page break, counted from the start of each
#'   measure. Set to `0` or `NULL` to break pages only between measures.
#'   Defaults to `15`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' # Write a HiTOP-DAT Qualtrics import file to a temporary location
#' generate_qualtrics_hitopdat(file = tempfile(fileext = ".txt"))
#'
#' @export
generate_qualtrics_hitopdat <- function(
  file = "hitopdat_qualtrics.txt",
  block_name = "HiTOP-DAT",
  id_prefix = "hdat",
  include_instructions = TRUE,
  breaks = 15
) {
  # build_qualtrics_txt() checks the other arguments. `file` is checked here,
  # and `id_prefix` before the instruction IDs are built from it.
  validate_string(file, "file")
  validate_string(id_prefix, "id_prefix")

  items <- hitopdat_items[order(hitopdat_items$Item), ]

  # One (value, label) data frame per answer set, then one per item from its
  # set, in the table's order.
  set_choices <- lapply(
    split(hitopdat_choices, hitopdat_choices$Choice_Set),
    function(x) data.frame(value = x$Value, label = x$Label)
  )

  first <- !duplicated(items$Measure)
  measures <- items$Measure[first]

  build_qualtrics_txt(
    items = items,
    max_n = max(hitopdat_items$Item),
    instructions = NULL,
    file = file,
    block_name = block_name,
    id_prefix = id_prefix,
    include_instructions = include_instructions,
    breaks = breaks,
    choices = unname(set_choices[items$Choice_Set]),
    sections = list(
      row = which(first),
      id = paste0(
        id_prefix,
        "_",
        gsub("[^a-z0-9]+", "_", tolower(measures)),
        "_instructions"
      ),
      text = unname(hitopdat_instructions$start[measures])
    )
  )
}

# Internal Helper: the item questions' `[[ID:]]` values
#
# One place for the IDs build_qualtrics_txt() writes, so the HiTOP-SR
# generator can record the same names in its descriptor's `columns` before the
# text file is built.
qualtrics_item_ids <- function(id_prefix, n, max_n) {
  item_names(paste0(id_prefix, "_"), n, max_n = max_n)
}

# Internal Helper: Build the Qualtrics text file
build_qualtrics_txt <- function(
  items,
  max_n,
  instructions,
  file,
  block_name,
  id_prefix,
  include_instructions,
  breaks,
  page_header = NULL,
  choices = NULL,
  sections = NULL,
  call = rlang::caller_env()
) {
  # `page_header`, when given, is a descriptive block repeated at the top of
  # every page after the first: the PID-5 Informant Form's items complete a
  # stem ("He or she..."), so each page restates the rating prompt and stem,
  # as the printed form does (M160 review). NULL writes no such block, so
  # every other export is unchanged.
  #
  # `choices` and `sections` serve a battery whose measures differ (the
  # HiTOP-DAT); NULL for both keeps every other export unchanged.
  # `choices` is one data frame of `value` and `label` per item, in place of
  # `instructions$options` for all items. `sections` is
  # list(row, id, text): each `row` starts a section, which opens with a
  # descriptive block of that `id` and `text` (none where `text` is NA) in
  # place of the single start block. Each section after the first starts a
  # new page, and `breaks` counts the items from the start of each section.
  #
  # The guards live here rather than in each generate_qualtrics_*() wrapper so
  # every instrument gets them from one place; `call` defaults to the wrapper
  # that called this, so the abort blames the exported function the user wrote,
  # not this internal (the score_engine() convention).
  #
  # `block_name` allows NULL because the block-line branch below already treats
  # NULL and "" alike as "write no block line"; guarding it as a plain string
  # would turn a call that works today into an error.
  # `max_n` is the instrument's largest item number, not this export's: a
  # module export keeps the full instrument's item-name width, so it has no
  # default to fall back on and every wrapper must state it.
  rlang::check_required(max_n, call = call)
  validate_count(max_n, arg = "max_n", min = 1, call = call)

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

  # Question IDs, padded to the width of the INSTRUMENT's largest item number
  # (`max_n`), not of the items this call exports: a module keeps its original
  # numbering, so padding to the exported items would write item 7 as `_07`
  # where the full instrument writes `_007`. For a full-instrument export the
  # two agree, so existing output is unchanged.
  question_ids <- qualtrics_item_ids(id_prefix, items[[1]], max_n)

  # 3. Add the starting instructions as a Descriptive Block (DB)
  if (include_instructions && is.null(sections)) {
    out <- c(
      out,
      "[[Question:DB]]",
      "[[ID:start_instructions]]",
      instructions$start,
      ""
    )
  }

  # 4. Format choices using AdvancedChoices to ensure exact recode values
  choices_block <- function(options) {
    block <- c("[[AdvancedChoices]]")
    # Loop through options to set exact numeric values and labels
    for (i in seq_len(nrow(options))) {
      val <- options$value[i]
      lab <- options$label[i]
      block <- c(block, paste0("[[Choice:", val, "]]"), lab)
    }
    block
  }
  if (is.null(choices)) {
    item_choices <- rep(list(choices_block(instructions$options)), nrow(items))
  } else {
    item_choices <- lapply(choices, choices_block)
  }

  # 5. Loop through the items dataframe and append each question
  section_rows <- if (is.null(sections)) integer(0) else sections$row
  # Items since the current section began; with no sections, every item.
  n_in_section <- 0L
  for (i in seq_len(nrow(items))) {
    s <- match(i, section_rows)
    if (!is.na(s)) {
      if (s > 1L) out <- c(out, "[[PageBreak]]", "")
      if (include_instructions && !is.na(sections$text[s])) {
        out <- c(
          out,
          "[[Question:DB]]",
          paste0("[[ID:", sections$id[s], "]]"),
          sections$text[s],
          ""
        )
      }
      n_in_section <- 0L
    }
    n_in_section <- n_in_section + 1L

    item_text <- items$Text[i]

    q_block <- c(
      "[[Question:MC:SingleAnswer]]",
      paste0("[[ID:", question_ids[i], "]]"),
      item_text,
      item_choices[[i]],
      ""
    )

    out <- c(out, q_block)

    # Insert Page Break based on the 'breaks' argument. A break the next
    # section's own page break already makes is not written twice.
    if (
      !is.null(breaks) && breaks > 0 && (n_in_section %% breaks == 0) &&
        (i != nrow(items)) && !((i + 1L) %in% section_rows)
    ) {
      out <- c(out, "[[PageBreak]]", "")
      if (!is.null(page_header)) {
        out <- c(
          out,
          "[[Question:DB]]",
          paste0("[[ID:page_header_", i %/% breaks + 1L, "]]"),
          page_header,
          ""
        )
      }
    }
  }

  # 6. Write everything to the specified file
  # A binary connection, not the path: `writeLines()` on a path opens a text
  # connection, which on Windows translates every "\n" into "\r\n". The
  # distributed artifacts in inst/extdata are LF, so a text connection would
  # make the same call emit different bytes on different platforms.
  con <- base::file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(out), con = con, useBytes = TRUE)
  cli::cli_alert_success(
    "Qualtrics import file successfully created at {.file {file}}"
  )
  invisible(file)
}
