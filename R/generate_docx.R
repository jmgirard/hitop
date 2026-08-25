#' Generate a Word Document for the HiTOP-BR Assessment
#'
#' @description Creates a formatted Microsoft Word document containing the
#'   Hierarchical Taxonomy of Psychopathology - Brief Report (HiTOP-BR) items,
#'   instructions, and optional scoring keys.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitopbr.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be
#'   one of `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.
#' @param title Character string for the document header title. Defaults to
#'   `"HiTOP-BR (v1.0)"`.
#' @param include_scoring Logical. If `TRUE` (default), appends a page break and
#'   the scoring instructions table.
#' @param font_size Numeric value specifying the base font size in points.
#'   Defaults to `10`.
#' @param font_family Character string specifying the font family to be used.
#'   Defaults to `"Times New Roman"`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' \donttest{
#' # Write a HiTOP-BR paper form to a temporary Word document
#' generate_docx_hitopbr(file = tempfile(fileext = ".docx"))
#' }
#'
#' @export
generate_docx_hitopbr <- function(
  file = "hitopbr.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-BR (v1.0)",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  t1 <- make_items_table(
    hitopbr_items,
    "HBR",
    hitopbr_instructions$options,
    dims$pw,
    font_size,
    font_family
  )

  t2 <- NULL
  if (include_scoring) {
    t2 <- make_scoring_table(
      hitopbr_scales,
      "HBR",
      dims$pw,
      font_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. No items are reverse-scored."

  build_hitop_doc(
    file,
    title,
    hitopbr_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    font_size,
    font_family
  )
}

#' Generate a Word Document for the HiTOP-SR Assessment
#'
#' @description Creates a formatted Microsoft Word document containing the
#'   Hierarchical Taxonomy of Psychopathology - Self-Report (HiTOP-SR) items,
#'   instructions, and optional scoring keys. The 405 items are formatted into a
#'   single continuous table.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitopsr.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be
#'   one of `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.
#' @param title Character string for the document header title, printed
#'   verbatim. The default (`NULL`) resolves by what the form contains:
#'   `"HiTOP-SR Module (v1.0)"` when `module` is supplied and
#'   `"HiTOP-SR (v1.0)"` otherwise, so a form built from a few scales is not
#'   headed as the full 405-item instrument. (default = `NULL`)
#' @param include_scoring Logical. If `TRUE` (default), appends a page break and
#'   the scoring instructions table.
#' @param include_subscales Logical. If `TRUE`, appends optional subscales to
#'   the scoring instructions table. Defaults to `FALSE`.
#' @param font_size Numeric value specifying the base font size in points.
#'   Defaults to `10`.
#' @param font_family Character string specifying the font family to be used.
#'   Defaults to `"Times New Roman"`.
#' @param module An optional [hitop_module()] object restricting the form to the
#'   items of the chosen scales. Cannot be combined with
#'   `include_subscales = TRUE`. (default = `NULL`)
#' @param renumber Logical. If `TRUE` (default), the printed items are numbered
#'   `1` to `n` down the page, so a module form does not show the full
#'   instrument's gapped numbers. Set to `FALSE` to print each item's original
#'   HiTOP-SR number instead. The scoring page always uses whichever numbers
#'   are printed. This differs from [generate_qualtrics_hitopsr()] and
#'   [generate_redcap_hitopsr()], which never renumber, because there an item
#'   number names a collected data column.
#' @param randomize Logical. If `TRUE`, the items are printed in a random
#'   order. On a renumbered module form the document also carries a crosswalk
#'   from each printed number back to its original HiTOP-SR number, so the form
#'   is scoreable from the paper alone; that crosswalk is printed whether or
#'   not `include_scoring` appends the key. It is *not* printed when `module`
#'   is `NULL` (405 pairs would be one dense paragraph) or when
#'   `renumber = FALSE` (the printed numbers are already the original ones) —
#'   in both cases read the order from the `item_order` attribute described
#'   under Value. There is no `seed` argument: call [set.seed()] before this
#'   function to make an order reproducible. (default = `FALSE`)
#'
#'   **Scoring data collected on a shuffled form.** [score_hitopsr()] addresses
#'   a module's items by their position in `module$items`, which is ascending
#'   original order — not the order a shuffled form prints them in. Reorder the
#'   collected columns through `item_order` before scoring:
#'   `collected[order(attr(out, "item_order"))]`. Scoring printed-order columns
#'   directly returns wrong scale scores and raises no error --- or pass
#'   `descriptor` and let the saved file carry the order for you.
#' @param descriptor An optional path to write a module descriptor to, beside
#'   the Word file. The saved file records which scales the form covers and
#'   which instrument items they draw on, so [read_module()] hands the module
#'   straight back to [score_hitopsr()] at scoring time. A call passing no
#'   `module` writes a descriptor naming every scale, describing the full
#'   administration. With `randomize = TRUE` it also records the printed order,
#'   returned on the read module's `item_order` attribute --- the record a
#'   shuffled whole-instrument form otherwise leaves nowhere, since no
#'   crosswalk is printed for one. Written before the Word file, so an
#'   unwritable path is reported before any form is produced; if the Word file
#'   then cannot be written, the descriptor is removed again, a file that was
#'   already at that path included.
#'   It must name a path of its own: an empty string, or the same path as
#'   `file`, is refused rather than leaving you with no descriptor and no
#'   error. (default = `NULL`)
#' @param subset Deprecated. The former name of `module`; supplying it warns.
#'   Supplying both `module` and `subset` is an error. (default = `NULL`)
#'
#' @return Invisibly returns the path to the created file (`file`), carrying an
#'   `item_order` attribute: the original HiTOP-SR item numbers in the order
#'   they were printed. It is present on every call, and is simply ascending
#'   unless `randomize = TRUE`.
#'
#' @examples
#' \donttest{
#' # Write a HiTOP-SR paper form to a temporary Word document
#' generate_docx_hitopsr(file = tempfile(fileext = ".docx"))
#'
#' # A module containing only two scales, printed as items 1 to 8
#' generate_docx_hitopsr(
#'   file = tempfile(fileext = ".docx"),
#'   module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
#' )
#'
#' # The same module keeping the full instrument's own item numbers
#' generate_docx_hitopsr(
#'   file = tempfile(fileext = ".docx"),
#'   module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
#'   renumber = FALSE
#' )
#'
#' # A shuffled form; the scoring page carries the crosswalk back
#' set.seed(1)
#' out <- generate_docx_hitopsr(
#'   file = tempfile(fileext = ".docx"),
#'   module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
#'   randomize = TRUE
#' )
#' attr(out, "item_order")
#'
#' # The same form with a descriptor saved beside it; the descriptor carries
#' # the printed order, so the collected columns can be put back in order
#' # without keeping a note by hand
#' f <- tempfile(fileext = ".json")
#' generate_docx_hitopsr(
#'   file = tempfile(fileext = ".docx"),
#'   module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
#'   randomize = TRUE,
#'   descriptor = f
#' )
#' attr(read_module(f), "item_order")
#' }
#'
#' @seealso [write_module()] and [read_module()] for the descriptor file.
#'
#' @export
generate_docx_hitopsr <- function(
  file = "hitopsr.docx",
  papersize = c("us", "a4"),
  title = NULL,
  include_scoring = TRUE,
  include_subscales = FALSE,
  font_size = 10,
  font_family = "Times New Roman",
  module = NULL,
  renumber = TRUE,
  randomize = FALSE,
  descriptor = NULL,
  subset = NULL
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)
  module <- resolve_module_arg(module, subset)
  validate_string(title, "title", allow_null = TRUE)
  validate_string(descriptor, "descriptor", allow_null = TRUE)
  validate_descriptor_target(descriptor, file)
  validate_flag(renumber, "renumber")
  validate_flag(randomize, "randomize")

  # Resolved AFTER resolve_module_arg(), so a caller still passing the
  # deprecated `subset =` gets the module header too. The sentinel is what
  # keeps an explicit `title` untouched: only `NULL` is replaced, so no
  # caller's header moves without asking (D-037).
  if (is.null(title)) {
    title <- if (is.null(module)) "HiTOP-SR (v1.0)" else "HiTOP-SR Module (v1.0)"
  }

  # Truthiness must match the consumer below (`if (include_subscales)`), or a
  # truthy non-TRUE value slips the guard and still adds the subscale rows.
  if (!is.null(module) && include_subscales) {
    cli::cli_abort(c(
      "{.arg include_subscales} cannot be combined with {.arg module}.",
      i = "A subscale may draw items from scales outside the module, so its
           scoring row would list items the form does not contain.",
      i = "Set {.code include_subscales = FALSE} to generate the module form."
    ))
  }

  reduced <- apply_module(hitopsr_items, hitopsr_scales, module, "HSR")

  # ONE printed-order map drives the items table, the scoring table, the
  # subscale rows, the crosswalk, and the returned attribute. Building it once
  # here is what keeps them from disagreeing: `item_order` is the original HSR
  # numbers in the order they are printed, and `printed_of()` sends an original
  # number to the number printed beside it.
  # sample.int(), not sample(): sample() on a length-one vector samples from
  # 1:x instead of permuting it, which is the classic foot-gun here.
  slot <- seq_len(nrow(reduced$items))
  if (randomize) slot <- sample.int(nrow(reduced$items))
  # as.integer(): `hitopsr_items$HSR` is a double, and an item number is a
  # count. Without this the returned attribute is a double and
  # `identical(attr(out, "item_order"), 1:405)` is FALSE.
  item_order <- as.integer(reduced$items$HSR[slot])
  printed <- if (renumber) seq_along(item_order) else item_order
  printed_of <- function(x) printed[match(x, item_order)]

  items_printed <- reduced$items[slot, , drop = FALSE]
  items_printed$HSR <- printed

  # Build the items table
  t1 <- make_items_table(
    items_printed,
    "HSR",
    hitopsr_instructions$options,
    dims$pw,
    font_size,
    font_family
  )

  t2 <- NULL
  if (include_scoring) {
    # Extract only the necessary columns from the main scales
    scales_to_score <- reduced$scales[, c("Scale", "itemdata")]
    scales_to_score$itemdata <- lapply(
      scales_to_score$itemdata,
      remap_itemdata,
      printed_of = printed_of
    )

    # If requested, prepare and append the subscales
    if (include_subscales) {
      subscales_to_score <- hitopsr_subscales[, c("Subscale", "itemdata")]
      subscales_to_score$itemdata <- lapply(
        subscales_to_score$itemdata,
        remap_itemdata,
        printed_of = printed_of
      )

      # Rename 'Subscale' to 'Scale' to match the main dataframe
      names(subscales_to_score)[
        names(subscales_to_score) == "Subscale"
      ] <- "Scale"

      # Optional: Add a visual indicator that these are subscales
      subscales_to_score$Scale <- paste0(
        subscales_to_score$Scale,
        " (Subscale)"
      )

      # Bind them together
      scales_to_score <- rbind(scales_to_score, subscales_to_score)
    }

    # Pass the combined data to the helper function
    t2 <- make_scoring_table(
      scales_to_score,
      "HSR",
      dims$pw,
      font_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. Reverse-scored items are indicated with (R)."

  # A shuffled form is scoreable from the paper alone only if the paper says
  # where each printed item came from. Three conditions gate the crosswalk.
  # Nothing shuffled: it would just restate the numbering. `renumber = FALSE`:
  # the printed numbers ARE the original ones, so every row would read
  # "42 -> 42". No module: a shuffled full instrument would print 405 pairs as
  # one dense paragraph on a participant-facing page, and D-036 signed off the
  # crosswalk for module forms only -- so the whole-instrument caller reads the
  # order from the returned `item_order` instead (maintainer decision, 2026-08-22
  # M046 review gate).
  crosswalk_msg <- NULL
  if (randomize && renumber && !is.null(module)) {
    crosswalk_msg <- paste(
      paste(printed, "\u2192", item_order),
      collapse = ", "
    )
  }

  # Written BEFORE the document, so an unwritable `descriptor` path is reported
  # while no Word file exists yet. A printed order is recorded only where there
  # is one to record: without `randomize` the form prints the instrument's own
  # order, which the module's ascending `items` already states.
  built <- FALSE
  if (!is.null(descriptor)) {
    write_descriptor_sidecar(
      descriptor,
      module,
      "hitopsr",
      item_order = if (randomize) item_order else NULL
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

  out <- build_hitop_doc(
    file,
    title,
    hitopsr_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    font_size,
    font_family,
    crosswalk_msg
  )
  built <- TRUE

  invisible(structure(out, item_order = item_order))
}

# Internal Helper: remap one scale's itemdata frame into printed numbering
#
# `printed_of()` closes over the single printed-order map built in
# generate_docx_hitopsr(), so a scoring row can never drift from the items
# table. Rows are re-sorted by their PRINTED number: on a shuffled form the
# original ascending order would print as a scattered list.
#
# A subscale may draw items from outside a module, which is why
# `include_subscales` and `module` cannot be combined; with `module = NULL`
# every item is present, so `match()` here never yields NA.
remap_itemdata <- function(x, printed_of) {
  x$HSR <- printed_of(x$HSR)
  x[order(x$HSR), , drop = FALSE]
}

# Internal Helper: Build the shared document footer (build stamp + copyright)
build_docx_footer <- function(font_size, font_family) {
  footer_prop <- officer::fp_text(
    color = "grey",
    font.size = max(6, font_size - 2),
    font.family = font_family
  )
  stamp <- paste0(
    "Generated ",
    format(Sys.Date(), "%Y-%m-%d"),
    " \u00b7 hitop ",
    utils::packageVersion("hitop")
  )
  footer_text <- officer::fpar(
    officer::ftext(stamp, prop = footer_prop),
    officer::ftext(
      " \u00b7 Copyright 2024 \u00a9 Hierarchical Taxonomy of Psychopathology Society",
      prop = footer_prop
    ),
    fp_p = officer::fp_par(text.align = "right")
  )
  officer::block_list(footer_text)
}

# Internal Helper: Get page dimensions based on paper size
get_page_dims <- function(papersize) {
  if (papersize == "us") {
    list(w = 8.5, h = 11.0, pw = 8.5 - 1.5)
  } else {
    list(w = 8.27, h = 11.69, pw = 8.27 - 1.5)
  }
}

# Internal Helper: Build the items flextable
make_items_table <- function(
  items_df,
  item_col,
  opts,
  printable_w,
  font_size,
  font_family,
  opts_per_line = nrow(opts)
) {
  num_opts <- nrow(opts)
  # The legend prints `opts_per_line` response options per header line. The
  # default puts them all on one line -- Word then breaks that line wherever
  # the column ends, which splits an option phrase mid-phrase once the labels
  # are long (the PID legend is 126 characters against the SR/BR's 58), so the
  # PID generators pass 2. Layout only: the pairs and their separator are
  # unchanged, and no line carries a trailing separator.
  pairs <- paste(opts$value, opts$label, sep = " = ")
  starts <- seq(1, length(pairs), by = opts_per_line)
  legend_text <- vapply(
    starts,
    function(i) {
      paste(pairs[i:min(i + opts_per_line - 1L, length(pairs))], collapse = " \u2022 ")
    },
    character(1)
  )

  table_data <- data.frame(
    Text = paste0(items_df[[item_col]], ".  ", items_df$Text),
    stringsAsFactors = FALSE
  )

  opt_cols <- paste0("opt_", seq_len(num_opts))
  for (i in seq_len(num_opts)) {
    table_data[[opt_cols[i]]] <- as.character(opts$value[i])
  }

  # which(), not seq(2, n, by = 2): a one-item table (a single-item module
  # form) makes that seq() count backwards and abort with "wrong sign in 'by'".
  even_rows <- which(seq_len(nrow(table_data)) %% 2 == 0)
  std_border <- officer::fp_border(color = "black", width = 1.5)

  opt_col_width <- 0.4
  text_col_width <- printable_w - (num_opts * opt_col_width)

  table_data |>
    flextable::flextable() |>
    flextable::delete_part(part = "header") |>
    flextable::add_header_lines(values = legend_text) |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = "Text", align = "left", part = "body") |>
    flextable::align(j = opt_cols, align = "center", part = "body") |>
    flextable::valign(valign = "center", part = "body") |>
    flextable::width(j = "Text", width = text_col_width) |>
    flextable::width(j = opt_cols, width = opt_col_width) |>
    flextable::padding(padding = 5, part = "all") |>
    flextable::hline_top(border = std_border, part = "header") |>
    flextable::hline_bottom(border = std_border, part = "header") |>
    flextable::bg(i = even_rows, bg = "#f2f2f2", part = "body") |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::set_table_properties(layout = "fixed", align = "left")
}

# Internal Helper: Build the scoring flextable
make_scoring_table <- function(
  scales_df,
  item_col,
  printable_w,
  font_size,
  font_family
) {
  scales_sorted <- scales_df[order(scales_df$Scale), ]

  scoring_data <- data.frame(
    Scale = scales_sorted$Scale,
    Items = vapply(
      scales_sorted$itemdata,
      function(x) {
        item_labels <- ifelse(
          x$Reverse,
          paste0(x[[item_col]], "(R)"),
          as.character(x[[item_col]])
        )
        paste(item_labels, collapse = ", ")
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  n_rows <- ceiling(nrow(scoring_data) / 2)

  if (nrow(scoring_data) %% 2 != 0) {
    scoring_data <- rbind(
      scoring_data,
      data.frame(
        Scale = NA_character_,
        Items = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }

  col1 <- scoring_data[1:n_rows, , drop = FALSE]
  col2 <- scoring_data[(n_rows + 1):nrow(scoring_data), , drop = FALSE]
  names(col2) <- c("Scale_2", "Items_2")

  table_2_data <- cbind(col1, col2)

  scale_col_width <- 1.25
  items_col_width <- (printable_w / 2) - scale_col_width
  std_border <- officer::fp_border(color = "black", width = 1.5)

  table_2_data |>
    flextable::flextable() |>
    flextable::set_header_labels(
      Scale = "Scale",
      Items = "Items",
      Scale_2 = "Scale",
      Items_2 = "Items"
    ) |>
    flextable::align(align = "left", part = "all") |>
    flextable::valign(valign = "top", part = "body") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::fontsize(size = max(6, font_size - 1), part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::width(j = c("Scale", "Scale_2"), width = scale_col_width) |>
    flextable::width(j = c("Items", "Items_2"), width = items_col_width) |>
    flextable::colformat_char(na_str = "") |>
    flextable::hline_top(part = "header", border = std_border) |>
    flextable::hline_bottom(part = "header", border = std_border) |>
    flextable::set_table_properties(layout = "fixed", align = "left")
}

# Internal Helper: Assemble the actual Word Document
build_hitop_doc <- function(
  file,
  title,
  instr_text,
  scoring_msg,
  table_1,
  table_2,
  include_scoring,
  dims,
  font_size,
  font_family,
  crosswalk_msg = NULL
) {
  inst_prop <- officer::fp_text(
    font.size = font_size,
    font.family = font_family
  )
  inst_prop_bold <- officer::fp_text(
    bold = TRUE,
    font.size = font_size,
    font.family = font_family
  )
  inst_par_prop <- officer::fp_par(padding.bottom = 24)

  # Header
  header_text <- officer::fpar(
    officer::ftext(title, prop = inst_prop_bold),
    fp_p = officer::fp_par(text.align = "left")
  )

  # Footer
  my_header <- officer::block_list(header_text)
  my_footer <- build_docx_footer(font_size, font_family)

  my_doc <- officer::read_docx() |>
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(instr_text, prop = inst_prop),
        fp_p = inst_par_prop
      )
    ) |>
    flextable::body_add_flextable(value = table_1)

  scoring_page <- include_scoring && !is.null(table_2)

  # The crosswalk is NOT gated on the scoring key. A shuffled form has to be
  # scoreable from the paper alone, and `include_scoring = FALSE` asks for a
  # participant-facing form with no key -- which the crosswalk is not: it maps
  # printed numbers to original ones and reveals neither scale membership nor
  # reverse-keying. Gating it on the key wrote a shuffled form nobody could
  # score, which is what this milestone's review returned.
  if (scoring_page || !is.null(crosswalk_msg)) {
    my_doc <- my_doc |> officer::body_add_break()
  }

  if (scoring_page) {
    my_doc <- my_doc |>
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Scoring Instructions: ", prop = inst_prop_bold),
          officer::ftext(scoring_msg, prop = inst_prop),
          fp_p = inst_par_prop
        )
      )
  }

  # Ahead of the scoring table, never after it: the table is the last thing on
  # the page, and a reader looking up a printed number wants the map before
  # the key rather than past it.
  if (!is.null(crosswalk_msg)) {
    my_doc <- my_doc |>
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(
            "Item Number Crosswalk (printed number \u2192 original HiTOP-SR number): ",
            prop = inst_prop_bold
          ),
          officer::ftext(crosswalk_msg, prop = inst_prop),
          fp_p = inst_par_prop
        )
      )
  }

  if (scoring_page) {
    my_doc <- my_doc |>
      flextable::body_add_flextable(value = table_2)
  }

  my_doc <- my_doc |>
    officer::body_set_default_section(
      value = officer::prop_section(
        page_size = officer::page_size(
          width = dims$w,
          height = dims$h,
          orient = "portrait"
        ),
        page_margins = officer::page_mar(
          bottom = 0.75,
          top = 0.75,
          right = 0.75,
          left = 0.75
        ),
        header_default = my_header,
        footer_default = my_footer
      )
    )

  print(my_doc, target = file)
  cli::cli_alert_success("Document successfully created at {.file {file}}")
  invisible(file)
}

#' Generate a Word Document for the HiTOP-HSUM Assessment Overview
#'
#' @description Creates a formatted Microsoft Word document containing the
#'   Hierarchical Taxonomy of Psychopathology - Substance Use Module (HSUM)
#'   overview, branching logic, and items.
#'
#' @param file Character string specifying the output file path. Defaults to
#'   `"hitophsum_overview.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be
#'   one of `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.
#' @param title Character string for the document header title. Defaults to
#'   `"HiTOP-HSUM (v1.0) Overview"`.
#' @param font_size Numeric value specifying the base font size in points.
#'   Defaults to `10`.
#' @param font_family Character string specifying the font family to be used.
#'   Defaults to `"Times New Roman"`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @examples
#' \donttest{
#' # Write a HiTOP-HSUM paper form to a temporary Word document
#' generate_docx_hitophsum(file = tempfile(fileext = ".docx"))
#' }
#'
#' @export
generate_docx_hitophsum <- function(
  file = "hitophsum_overview.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-HSUM (v1.0) Overview",
  font_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  # --- 1. Define Data for Step 1 ---
  sud1_options <- paste(
    "Options:",
    "\u2022 Alcohol",
    "\u2022 Cannabis (marijuana, pot, grass, hash, etc.)",
    "\u2022 Nicotine (cigarettes, vaping/e-cigarettes, nicotine pouches, over-the-counter nicotine gum, patch, lozenge)",
    "\u2022 Cocaine (coke, crack, etc.)",
    "\u2022 Prescription stimulants (Ritalin, Concerta, Dexedrine, Adderall, diet pills, etc.)",
    "\u2022 Methamphetamine (speed, crystal meth, ice, etc.)",
    "\u2022 Inhalants (nitrous oxide, glue, gas, paint thinner, etc.)",
    "\u2022 Sedatives or sleeping pills (Valium, Serepax, Ativan, Xanax, Librium, Rohypnol, GHB, etc.)",
    "\u2022 Hallucinogens (LSD, acid, mushrooms, PCP, Special K, ecstasy, etc.)",
    "\u2022 Street opioids (heroin, opium, fentanyl, etc.)",
    "\u2022 Prescription opioids (oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)",
    "\u2022 Other - specify:",
    sep = "\n"
  )

  sud2_options <- paste(
    "Options:",
    "\u2022 smoking cigarettes",
    "\u2022 vaping or e-cigarettes",
    "\u2022 cigars, cigarillos, or filtered cigars filled only with tobacco",
    "\u2022 nicotine gum or patches",
    "\u2022 chewing tobacco or pouches/dip",
    "\u2022 Other - specify:",
    sep = "\n"
  )

  step1_data <- data.frame(
    Item = c("SUDscreen1", "Logic", "SUDscreen2", "Logic", "SUDscreen3"),
    Content = c(
      paste0(
        "Please select which of the following substance(s) you have used in the past 12 months. Please consider only substances that were NOT prescribed to you by a medical professional or that you used in a manner that was NOT prescribed.\n\n",
        sud1_options
      ),
      "[If nicotine is selected in SUDscreen1, then ask SUDscreen2]",
      paste0(
        "In what forms did you use nicotine over the past 12 months? (Select all that apply)\n\n",
        sud2_options
      ),
      "[If more than one form is selected in SUDscreen2, then ask SUDscreen3]",
      "In what form did you MOST OFTEN use nicotine over the past 12 months?\n(Options match SUDscreen2)"
    ),
    stringsAsFactors = FALSE
  )

  # --- 2. Define Data for Step 2 ---
  freq_opts <- "Options: 1-5 days total, 6-11 days total, about once a month, 2-3 days per month, 1-2 days per week, 3-4 days per week, 5-6 days per week, everyday, Prefer not to say"
  heavy_opts <- "Options: Never drank this many drinks on a single occasion, 1-5 times, 6-11 times, about once a month, 2-3 times per month, 1-2 times per week, 3-4 times per week, 5-6 times per week, everyday, Prefer not to say"

  step2_data <- data.frame(
    Topic = c(
      "frequency of use",
      "frequency of intoxication",
      "use per day",
      "frequency of heavy use"
    ),
    Item = c("SUDscreen4", "SUDscreen5", "SUDscreen6", "SUDscreen7"),
    Alcohol = c(
      paste0(
        "How often did you drink alcohol over the past 12 months?\n\n",
        freq_opts
      ),
      paste0(
        "Over the past 12 months, how often did you get intoxicated/drunk while drinking alcohol?\n\nOptions: didn't get drunk or intoxicated..., ",
        gsub("Options: ", "", freq_opts)
      ),
      "On days when you drank alcohol, how many drinks did you typically consume on a single occasion?\n\nOptions: Drop-down menu with 1 to 20+ drinks (and prefer not to say)",
      paste0(
        "Over the past 12 months, how often did you have more than 4 drinks (for women) or 5 drinks (for men) on a single occasion?\n\n",
        heavy_opts
      )
    ),
    Nicotine = c(
      paste0(
        "How often did you use nicotine (in any form) over the past 12 months?\n\n",
        freq_opts
      ),
      "not assessed for nicotine",
      "Loops through all endorsed forms from SUDscreen2:\n- Cigarettes/Cigars: On days when you smoked [form], how many did you typically smoke? (Drop-down 1 to 60+)\n- Other forms: On days when you used nicotine, how much did you typically use? (Open response)",
      "not assessed for nicotine"
    ),
    Other = c(
      paste0(
        "How often did you use [substance] over the past 12 months?\n\n",
        freq_opts
      ),
      paste0(
        "Over the past 12 months, how often did you get intoxicated/high while using [substance]?\n\nOptions: didn't get high or intoxicated..., ",
        gsub("Options: ", "", freq_opts)
      ),
      "On days when you used [substance], how much did you typically use?\n\nOptions: Open response field for typical use",
      "not assessed for other"
    ),
    stringsAsFactors = FALSE
  )

  # --- 3A. Define Data for Step 3 (SUD Items) ---
  step3_sud_matrix <- rbind(
    c(
      "craving",
      "SUD01",
      "Reminders of alcohol gave me a strong urge to drink.",
      "Reminders of using nicotine gave me a strong urge to smoke or take nicotine.",
      "Reminders of [substance] gave me a strong urge to use [substance]."
    ),
    c(
      "craving",
      "SUD02",
      "I craved a drink of alcohol.",
      "I craved nicotine.",
      "I craved [substance]."
    ),
    c(
      "craving",
      "SUD03",
      "I had strong desires for alcohol.",
      "I had strong desires to use nicotine.",
      "I had strong desires for [substance]."
    ),

    c(
      "hazardous use",
      "SUD04",
      "I drove a car while intoxicated.",
      "not assessed for nicotine",
      "I drove a car while under the influence of [substance]."
    ),
    c(
      "hazardous use",
      "SUD05",
      "When I was under the influence of alcohol, I was in a situation where I could have gotten hurt (e.g., when riding a bicycle, swimming, operating machinery).",
      "not assessed for nicotine",
      "When I was under the influence of [substance], I was in a situation where I could have gotten hurt (e.g., when riding a bicycle, swimming, operating machinery)."
    ),
    c(
      "hazardous use",
      "SUD06",
      "I damaged something valuable while under the influence of alcohol.",
      "not assessed for nicotine",
      "I damaged something valuable while using [substance]."
    ),
    c(
      "hazardous use",
      "SUD07",
      "I did risky or dangerous things while drinking alcohol that I would not have done sober.",
      "not assessed for nicotine",
      "I did risky or dangerous things while using [substance] that I would not have done sober."
    ),

    c(
      "impaired control",
      "SUD08",
      "I was not able to cut back drinking when I wanted to.",
      "I was not able to cut back on using nicotine when I wanted to.",
      "I was not able to cut back on using [substance] when I wanted to."
    ),
    c(
      "impaired control",
      "SUD09",
      "I drank too much.",
      "I used nicotine too much.",
      "I used too much [substance]."
    ),
    c(
      "impaired control",
      "SUD10",
      "I tried to drink only at certain times, but it did not work for long.",
      "I tried to use nicotine only at certain times, but it did not work for long.",
      "I tried to use [substance] only at certain times, but it did not work for long."
    ),
    c(
      "impaired control",
      "SUD11",
      "I got drunk despite previously deciding not to.",
      "I used nicotine despite previously deciding not to.",
      "I got high on [substance] despite previously deciding not to."
    ),

    c(
      "role interference",
      "SUD12",
      "I continued to drink even though it made my performance at work suffer.",
      "I continued to use nicotine even though it made my performance at work suffer.",
      "I continued to use [substance] even though it made my performance at work suffer."
    ),
    c(
      "role interference",
      "SUD13",
      "Drinking or being sick from drinking got in the way of me taking care of myself, my home, or my family.",
      "Using nicotine got in the way of me taking care of myself, my home, or my family.",
      "Using [substance] or being sick from using [substance] got in the way of me taking care of myself, my home, or my family."
    ),
    c(
      "role interference",
      "SUD14",
      "I missed work, school, or other obligations because I was getting over the effects of drinking (e.g., feeling hungover).",
      "Using nicotine interfered with my health.",
      "I missed work, school, or other obligations because I was getting over the effects of using [substance]."
    ),

    c(
      "tolerance",
      "SUD15",
      "Over time, I needed to drink more alcohol in order to get the same effect.",
      "Over time, I needed to use more nicotine to get the same effect.",
      "Over time, I needed to use more [substance] to get the same effect."
    ),
    c(
      "tolerance",
      "SUD16",
      "I consumed a good amount of alcohol before others realized I had been drinking.",
      "I've noticed that using nicotine does not have the same effect that it used to have.",
      "I used a good amount of [substance] before others realized I had been using it."
    ),
    c(
      "tolerance",
      "SUD17",
      "I drank a lot more than others before feeling drunk.",
      "My use of nicotine has increased over time.",
      "I used [substance] a lot more than others before feeling high."
    )
  )
  step3_sud_data <- as.data.frame(step3_sud_matrix, stringsAsFactors = FALSE)
  colnames(step3_sud_data) <- c(
    "Subscale",
    "Item",
    "Alcohol",
    "Nicotine",
    "Other"
  )

  # --- 3B. Define Data for Step 3 (WITH Items) ---
  step3_with_matrix <- rbind(
    c("... changes in bodily sensations", ""),
    c("WITH01", "Goose bumps"),
    c("WITH02", "Heart racing"),
    c("WITH03", "Muscle aches"),
    c("WITH04", "Physical shakes"),
    c("WITH05", "Seizures"),
    c("WITH06", "Sweating"),

    c("... changes in digestive problems", ""),
    c("WITH07", "Constipation"),
    c("WITH08", "Diarrhea"),
    c("WITH09", "Nausea or Vomiting"),

    c("... changes in or on my face or head", ""),
    c("WITH10", "Coughing"),
    c("WITH11", "Dilated pupils"),
    c("WITH12", "Dry mouth"),
    c("WITH13", "Fever"),
    c("WITH14", "Headaches or Migraines"),
    c("WITH15", "Mouth sores"),
    c("WITH16", "Runny nose"),
    c("WITH17", "Teary eyes"),
    c("WITH18", "Yawning"),

    c("... changes in how I think, feel, or behave", ""),
    c("WITH19", "Anxiety or Nervousness"),
    c("WITH20", "Depression or Sadness"),
    c("WITH21", "Trouble enjoying things"),
    c("WITH22", "Anger or Irritability"),
    c("WITH23", "Trouble concentrating"),
    c("WITH24", "Trouble paying attention"),
    c("WITH25", "Trouble thinking clearly"),
    c("WITH26", "Restlessness"),
    c("WITH27", "Seeing things that weren't really there"),
    c("WITH28", "Feeling things that weren't really there"),
    c("WITH29", "Hearing things that weren't really there"),

    c("... changes in appetite or sleeping", ""),
    c("WITH30", "Decreased appetite"),
    c("WITH31", "Increased appetite"),
    c("WITH32", "Sleeping too much"),
    c("WITH33", "Insomnia or trouble sleeping")
  )
  step3_with_data <- as.data.frame(step3_with_matrix, stringsAsFactors = FALSE)
  colnames(step3_with_data) <- c("Item", "Content")

  # --- Formatting Variables ---
  even_rows_step1 <- which(seq_len(nrow(step1_data)) %% 2 == 0)
  even_rows_step2 <- which(seq_len(nrow(step2_data)) %% 2 == 0)
  even_rows_step3_sud <- which(seq_len(nrow(step3_sud_data)) %% 2 == 0)
  even_rows_step3_with <- which(seq_len(nrow(step3_with_data)) %% 2 == 0)

  italic_step2_nicotine <- which(grepl("not assessed", step2_data$Nicotine))
  italic_step2_other <- which(grepl("not assessed", step2_data$Other))
  italic_step3_sud <- which(grepl("not assessed", step3_sud_data$Nicotine))

  with_header_rows <- which(step3_with_data$Content == "")

  std_border <- officer::fp_border(color = "black", width = 1.5)
  inst_prop <- officer::fp_text(
    font.size = font_size,
    font.family = font_family
  )
  inst_prop_bold <- officer::fp_text(
    bold = TRUE,
    font.size = font_size,
    font.family = font_family
  )
  inst_par_prop <- officer::fp_par(padding.bottom = 12)

  # --- Header and Footer ---
  header_text <- officer::fpar(
    officer::ftext(title, prop = inst_prop_bold),
    fp_p = officer::fp_par(text.align = "left")
  )
  my_header <- officer::block_list(header_text)

  my_footer <- build_docx_footer(font_size, font_family)

  # --- Build Flextables ---
  ft_step1 <- flextable::flextable(step1_data) |>
    flextable::width(j = "Item", width = 1.25) |>
    flextable::width(j = "Content", width = dims$pw - 1.25) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::hline_top(border = std_border, part = "header") |>
    flextable::hline_bottom(border = std_border, part = "header") |>
    flextable::bg(i = even_rows_step1, bg = "#f2f2f2", part = "body") |>
    flextable::valign(valign = "top", part = "body")

  ft_step2 <- flextable::flextable(step2_data) |>
    flextable::width(j = "Topic", width = 0.9) |>
    flextable::width(j = "Item", width = 0.8) |>
    flextable::width(
      j = c("Alcohol", "Nicotine", "Other"),
      width = (dims$pw - 1.7) / 3
    ) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::hline_top(border = std_border, part = "header") |>
    flextable::hline_bottom(border = std_border, part = "header") |>
    flextable::bg(i = even_rows_step2, bg = "#f2f2f2", part = "body") |>
    flextable::italic(
      i = italic_step2_nicotine,
      j = "Nicotine",
      part = "body"
    ) |>
    flextable::italic(i = italic_step2_other, j = "Other", part = "body") |>
    flextable::valign(valign = "top", part = "body")

  ft_step3_sud <- flextable::flextable(step3_sud_data) |>
    flextable::width(j = "Subscale", width = 0.9) |>
    flextable::width(j = "Item", width = 0.7) |>
    flextable::width(
      j = c("Alcohol", "Nicotine", "Other"),
      width = (dims$pw - 1.6) / 3
    ) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::hline_top(border = std_border, part = "header") |>
    flextable::hline_bottom(border = std_border, part = "header") |>
    flextable::bg(i = even_rows_step3_sud, bg = "#f2f2f2", part = "body") |>
    flextable::italic(i = italic_step3_sud, j = "Nicotine", part = "body") |>
    flextable::valign(valign = "top", part = "body")

  ft_step3_with <- flextable::flextable(step3_with_data) |>
    flextable::width(j = "Item", width = 1.0) |>
    flextable::width(j = "Content", width = dims$pw - 1.0) |>
    flextable::theme_vanilla() |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::hline_top(border = std_border, part = "header") |>
    flextable::hline_bottom(border = std_border, part = "header") |>
    flextable::bg(i = even_rows_step3_with, bg = "#f2f2f2", part = "body") |>
    flextable::bg(i = with_header_rows, bg = "#d9d9d9", part = "body") |>
    flextable::bold(i = with_header_rows, part = "body") |>
    flextable::keep_with_next(i = with_header_rows, part = "body", value = TRUE)

  # Merge category spanning rows in the WITH table
  for (row_idx in with_header_rows) {
    ft_step3_with <- flextable::merge_at(
      ft_step3_with,
      i = row_idx,
      j = c("Item", "Content"),
      part = "body"
    )
  }

  ft_step3_with <- flextable::valign(
    ft_step3_with,
    valign = "top",
    part = "body"
  )

  # --- Assemble Document ---
  my_doc <- officer::read_docx() |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "In this next section, we will ask you about your use of alcohol and other substances over the past 12 months.",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )) |>

    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Step 1: Determine which substances have been used in past 12 months",
        prop = inst_prop_bold
      ),
      fp_p = inst_par_prop
    )) |>
    flextable::body_add_flextable(value = ft_step1) |>
    officer::body_add_break() |>

    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Step 2: Get frequency and quantity of use for each substance in Step 1",
        prop = inst_prop_bold
      ),
      fp_p = inst_par_prop
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "[Loop through frequency items for all substances endorsed in Step 1]",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )) |>
    flextable::body_add_flextable(value = ft_step2) |>
    officer::body_add_break() |>

    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Step 3: Loop through the SUD items",
        prop = inst_prop_bold
      ),
      fp_p = inst_par_prop
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Looping algorithm based on frequency data in Step 2: 1. administer alcohol items IF alcohol use is at least monthly. 2. administer nicotine items IF nicotine use is at least 1-2 times per week. 3. administer other drug items for the most frequently used other drug that is used at least monthly.",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Response options for all items: (not at all, a little, moderately, a lot)",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )) |>
    flextable::body_add_flextable(value = ft_step3_sud) |>
    officer::body_add_fpar(officer::fpar(fp_p = inst_par_prop)) |>

    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "As a result of stopping, quitting, or cutting down on my use of [substance], I experienced...",
        prop = inst_prop_bold
      ),
      fp_p = inst_par_prop
    )) |>
    officer::body_add_fpar(officer::fpar(
      officer::ftext(
        "Response options for all items: (not at all, a little, moderately, a lot)",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )) |>
    flextable::body_add_flextable(value = ft_step3_with) |>

    officer::body_set_default_section(
      value = officer::prop_section(
        page_size = officer::page_size(
          width = dims$w,
          height = dims$h,
          orient = "portrait"
        ),
        page_margins = officer::page_mar(
          bottom = 0.75,
          top = 0.75,
          right = 0.75,
          left = 0.75
        ),
        header_default = my_header,
        footer_default = my_footer
      )
    )

  print(my_doc, target = file)
  cli::cli_alert_success("Document successfully created at {.file {file}}")
  invisible(file)
}

#' Generate a Word Document for the PID-5 (Full)
#'
#' @param file Character string specifying the output file path.
#' @param papersize Character string specifying the paper dimensions. Must be
#'   one of `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.
#' @param title Character string for the document header title.
#' @param include_scoring Logical. If `TRUE`, appends a page break and scoring instructions.
#' @param font_size Numeric value specifying the base font size in points.
#' @param font_family Character string specifying the font family to be used.
#'
#' @examples
#' \donttest{
#' # Write a PID-5 (full) paper form to a temporary Word document
#' generate_docx_pid5(file = tempfile(fileext = ".docx"))
#' }
#'
#' @export
generate_docx_pid5 <- function(
  file = "pid5.docx",
  papersize = c("us", "a4"),
  title = "PID-5 (Full)",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  items <- pid_items[!is.na(pid_items$FULL), ]
  items <- items[order(items$FULL), ]

  t1 <- make_items_table(
    items,
    "FULL",
    pid_instructions$options,
    dims$pw,
    font_size,
    font_family,
    opts_per_line = 2
  )

  t2 <- NULL
  if (include_scoring) {
    scales_to_score <- pid_scales$FULL
    names(scales_to_score)[names(scales_to_score) == "Facet"] <- "Scale"

    t2 <- make_scoring_table(
      scales_to_score,
      "FULL",
      dims$pw,
      font_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. Reverse-scored items are indicated with (R)."

  build_hitop_doc(
    file,
    title,
    pid_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    font_size,
    font_family
  )
}

#' Generate a Word Document for the PID-5-SF
#'
#' @inheritParams generate_docx_pid5
#'
#' @examples
#' \donttest{
#' # Write a PID-5-SF paper form to a temporary Word document
#' generate_docx_pid5sf(file = tempfile(fileext = ".docx"))
#' }
#'
#' @export
generate_docx_pid5sf <- function(
  file = "pid5sf.docx",
  papersize = c("us", "a4"),
  title = "PID-5-SF",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  items <- pid_items[!is.na(pid_items$SF), ]
  items <- items[order(items$SF), ]

  t1 <- make_items_table(
    items,
    "SF",
    pid_instructions$options,
    dims$pw,
    font_size,
    font_family,
    opts_per_line = 2
  )

  t2 <- NULL
  if (include_scoring) {
    scales_to_score <- pid_scales$SF
    names(scales_to_score)[names(scales_to_score) == "Facet"] <- "Scale"

    t2 <- make_scoring_table(
      scales_to_score,
      "SF",
      dims$pw,
      font_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. Reverse-scored items are indicated with (R)."

  build_hitop_doc(
    file,
    title,
    pid_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    font_size,
    font_family
  )
}

#' Generate a Word Document for the PID-5-BF
#'
#' @inheritParams generate_docx_pid5
#'
#' @examples
#' \donttest{
#' # Write a PID-5-BF paper form to a temporary Word document
#' generate_docx_pid5bf(file = tempfile(fileext = ".docx"))
#' }
#'
#' @export
generate_docx_pid5bf <- function(
  file = "pid5bf.docx",
  papersize = c("us", "a4"),
  title = "PID-5-BF",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  items <- pid_items[!is.na(pid_items$BF), ]
  items <- items[order(items$BF), ]

  t1 <- make_items_table(
    items,
    "BF",
    pid_instructions$options,
    dims$pw,
    font_size,
    font_family,
    opts_per_line = 2
  )

  t2 <- NULL
  if (include_scoring) {
    scales_to_score <- pid_scales$BF
    names(scales_to_score)[names(scales_to_score) == "Domain"] <- "Scale"

    t2 <- make_scoring_table(
      scales_to_score,
      "BF",
      dims$pw,
      font_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. Reverse-scored items are indicated with (R)."

  build_hitop_doc(
    file,
    title,
    pid_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    font_size,
    font_family
  )
}
