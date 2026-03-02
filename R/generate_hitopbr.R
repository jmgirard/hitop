#' Generate a Word Document for the HiTOP-BR Assessment
#'
#' @description
#' Creates a formatted Microsoft Word document containing the Hierarchical
#' Taxonomy of Psychopathology - Brief Report (HiTOP-BR) items, instructions,
#' and optional scoring keys. The function dynamically adjusts table widths
#' to ensure a perfect fit within standard page margins.
#'
#' @param file Character string specifying the output file path. Defaults to `"hitopbr_1.0.docx"`.
#' @param papersize Character string specifying the paper dimensions. Must be one of `"letter"` (8.5 x 11 inches) or `"a4"` (8.27 x 11.69 inches). Defaults to `"letter"`.
#' @param title Character string for the document header title. Defaults to `"HiTOP-BR (v1.0)"`.
#' @param include_scoring Logical. If `TRUE` (default), appends a page break and the scoring instructions table to the end of the document.
#' @param base_size Numeric value specifying the base font size in points. Defaults to `10`.
#' @param font_family Character string specifying the font family to be used throughout the document. Defaults to `"Times New Roman"`.
#'
#' @return Invisibly returns the path to the created file (`file`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate standard version
#' generate_hitopbr_doc()
#'
#' # Generate a customized version for A4 paper without the scoring key
#' generate_hitopbr_doc(
#'   file = "hitopbr_a4_noscoring.docx",
#'   papersize = "a4",
#'   title = "HiTOP-BR - Patient Copy",
#'   include_scoring = FALSE,
#'   base_size = 12,
#'   font_family = "Arial"
#' )
#' }
generate_hitopbr_doc <- function(
  file = "hitopbr_1.0.docx",
  papersize = c("letter", "a4"),
  title = "HiTOP-BR (v1.0)",
  include_scoring = TRUE,
  base_size = 10,
  font_family = "Times New Roman"
) {
  papersize <- match.arg(papersize)

  if (papersize == "letter") {
    page_w <- 8.5
    page_h <- 11.0
  } else {
    page_w <- 8.27
    page_h <- 11.69
  }

  printable_w <- page_w - 1.5

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

  header_text <- officer::fpar(
    officer::ftext(title, prop = inst_prop_bold),
    fp_p = officer::fp_par(text.align = "left")
  )

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

  opts <- hitopbr_instructions$options
  num_opts <- nrow(opts)

  legend_text <- paste(
    opts$value,
    opts$label,
    sep = " = ",
    collapse = " \u2022 "
  )

  table_data <- data.frame(
    Text = paste0(hitopbr_items$HBR, ".  ", hitopbr_items$Text),
    stringsAsFactors = FALSE
  )

  opt_cols <- paste0("opt_", seq_len(num_opts))

  for (i in seq_len(num_opts)) {
    col_name <- opt_cols[i]
    table_data[[col_name]] <- as.character(opts$value[i])
  }

  even_rows <- seq(2, nrow(table_data), by = 2)
  std_border <- officer::fp_border(color = "black", width = 1.5)

  opt_col_width <- 0.4
  text_col_width <- printable_w - (num_opts * opt_col_width)

  table_1 <- table_data |>
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

  # Process Scoring Data only if requested
  if (include_scoring) {
    hitopbr_scales_sorted <- hitopbr_scales[order(hitopbr_scales$Scale), ]

    scoring_data <- data.frame(
      Scale = hitopbr_scales_sorted$Scale,
      Items = vapply(
        hitopbr_scales_sorted$itemdata,
        function(x) {
          item_labels <- ifelse(
            x$Reverse,
            paste0(x$HBR, "(R)"),
            as.character(x$HBR)
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

    table_2 <-
      table_2_data |>
      flextable::flextable() |>
      flextable::set_header_labels(
        Scale = "Scale",
        Items = "Items",
        Scale_2 = "Scale",
        Items_2 = "Items"
      ) |>
      flextable::align(align = "left", part = "all") |>
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

  # Assemble document
  my_doc <-
    officer::read_docx() |>
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(hitopbr_instructions$start, prop = inst_prop),
        fp_p = inst_par_prop
      )
    ) |>
    flextable::body_add_flextable(value = table_1)

  # Conditionally add scoring instructions
  if (include_scoring) {
    my_doc <- my_doc |>
      officer::body_add_break() |>
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Scoring Instructions: ", prop = inst_prop_bold),
          officer::ftext(
            "Average the responses for the following item numbers. No items are reverse-scored.",
            prop = inst_prop
          ),
          fp_p = inst_par_prop
        )
      ) |>
      flextable::body_add_flextable(value = table_2)
  }

  # Apply universal formatting
  my_doc <- my_doc |>
    officer::body_set_default_section(
      value = officer::prop_section(
        page_size = officer::page_size(
          width = page_w,
          height = page_h,
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
