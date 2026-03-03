#' Generate a Word Document for the HiTOP-BR Assessment
#'
#' @description
#' Creates a formatted Microsoft Word document containing the Hierarchical
#' Taxonomy of Psychopathology - Brief Report (HiTOP-BR) items, instructions,
#' and optional scoring keys.
#'
#' @param file Character string specifying the output file path. Defaults to `"hitopbr_1.0.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be one of `"letter"` or `"a4"`. Defaults to `"letter"`.
#' @param title Character string for the document header title. Defaults to `"HiTOP-BR (v1.0)"`.
#' @param include_scoring Logical. If `TRUE` (default), appends a page break and the scoring instructions table.
#' @param base_size Numeric value specifying the base font size in points. Defaults to `10`.
#' @param font_family Character string specifying the font family to be used. Defaults to `"Times New Roman"`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#' @export
generate_docx_hitopbr <- function(
  file = "hitopbr_1.0.docx",
  papersize = c("letter", "a4"),
  title = "HiTOP-BR (v1.0)",
  include_scoring = TRUE,
  base_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  t1 <- make_items_table(
    hitopbr_items,
    "HBR",
    hitopbr_instructions$options,
    dims$pw,
    base_size,
    font_family
  )

  t2 <- NULL
  if (include_scoring) {
    t2 <- make_scoring_table(
      hitopbr_scales,
      "HBR",
      dims$pw,
      base_size,
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
    base_size,
    font_family
  )
}

#' Generate a Word Document for the HiTOP-SR Assessment
#'
#' @description
#' Creates a formatted Microsoft Word document containing the Hierarchical
#' Taxonomy of Psychopathology - Self-Report (HiTOP-SR) items, instructions,
#' and optional scoring keys. The 405 items are formatted into a single continuous table.
#'
#' @param file Character string specifying the output file path. Defaults to `"hitopsr_1.0.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be one of `"letter"` or `"a4"`. Defaults to `"letter"`.
#' @param title Character string for the document header title. Defaults to `"HiTOP-SR (v1.0)"`.
#' @param include_scoring Logical. If `TRUE` (default), appends a page break and the scoring instructions table.
#' @param include_subscales Logical. If `TRUE`, appends optional subscales to the scoring instructions table. Defaults to `FALSE`.
#' @param base_size Numeric value specifying the base font size in points. Defaults to `10`.
#' @param font_family Character string specifying the font family to be used. Defaults to `"Times New Roman"`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#' @export
generate_docx_hitopsr <- function(
  file = "hitopsr_1.0.docx",
  papersize = c("letter", "a4"),
  title = "HiTOP-SR (v1.0)",
  include_scoring = TRUE,
  include_subscales = FALSE,
  base_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)
  dims <- get_page_dims(papersize)

  # Build the items table
  t1 <- make_items_table(
    hitopsr_items,
    "HSR",
    hitopsr_instructions$options,
    dims$pw,
    base_size,
    font_family
  )

  t2 <- NULL
  if (include_scoring) {
    # Extract only the necessary columns from the main scales
    scales_to_score <- hitopsr_scales[, c("Scale", "itemdata")]

    # If requested, prepare and append the subscales
    if (include_subscales) {
      subscales_to_score <- hitopsr_subscales[, c("Subscale", "itemdata")]

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
      base_size,
      font_family
    )
  }

  scoring_msg <- "Average the responses for the following item numbers. Reverse-scored items are indicated with (R)."

  build_hitop_doc(
    file,
    title,
    hitopsr_instructions$start,
    scoring_msg,
    t1,
    t2,
    include_scoring,
    dims,
    base_size,
    font_family
  )
}

# Internal Helper: Get page dimensions based on paper size
get_page_dims <- function(papersize) {
  if (papersize == "letter") {
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
  base_size,
  font_family
) {
  num_opts <- nrow(opts)
  legend_text <- paste(
    opts$value,
    opts$label,
    sep = " = ",
    collapse = " \u2022 "
  )

  table_data <- data.frame(
    Text = paste0(items_df[[item_col]], ".  ", items_df$Text),
    stringsAsFactors = FALSE
  )

  opt_cols <- paste0("opt_", seq_len(num_opts))
  for (i in seq_len(num_opts)) {
    table_data[[opt_cols[i]]] <- as.character(opts$value[i])
  }

  even_rows <- seq(2, nrow(table_data), by = 2)
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
    flextable::fontsize(size = base_size, part = "all") |>
    flextable::font(fontname = font_family, part = "all") |>
    flextable::set_table_properties(layout = "fixed", align = "left")
}

# Internal Helper: Build the scoring flextable
make_scoring_table <- function(
  scales_df,
  item_col,
  printable_w,
  base_size,
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
    flextable::fontsize(size = max(6, base_size - 1), part = "all") |>
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
  base_size,
  font_family
) {
  inst_prop <- officer::fp_text(
    font.size = base_size,
    font.family = font_family
  )
  inst_prop_bold <- officer::fp_text(
    bold = TRUE,
    font.size = base_size,
    font.family = font_family
  )
  inst_par_prop <- officer::fp_par(padding.bottom = 24)

  # Header
  header_text <- officer::fpar(
    officer::ftext(title, prop = inst_prop_bold),
    fp_p = officer::fp_par(text.align = "left")
  )

  # Footer
  footer_text <- officer::fpar(
    officer::ftext(
      "Copyright 2024 \u00A9 Hierarchical Taxonomy of Psychopathology Society",
      prop = officer::fp_text(
        color = "grey",
        font.size = max(6, base_size - 2),
        font.family = font_family
      )
    ),
    fp_p = officer::fp_par(text.align = "right")
  )

  my_header <- officer::block_list(header_text)
  my_footer <- officer::block_list(footer_text)

  my_doc <- officer::read_docx() |>
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(instr_text, prop = inst_prop),
        fp_p = inst_par_prop
      )
    ) |>
    flextable::body_add_flextable(value = table_1)

  if (include_scoring && !is.null(table_2)) {
    my_doc <- my_doc |>
      officer::body_add_break() |>
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Scoring Instructions: ", prop = inst_prop_bold),
          officer::ftext(scoring_msg, prop = inst_prop),
          fp_p = inst_par_prop
        )
      ) |>
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
