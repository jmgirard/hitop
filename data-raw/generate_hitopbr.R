library(flextable)
library(officer)

# 1. Define Text Properties, Header, and Footer
# Define text properties for the instructions
inst_prop <- fp_text(font.size = 10, font.family = "Times New Roman")
inst_prop_bold <- fp_text(
  bold = TRUE,
  font.size = 10,
  font.family = "Times New Roman"
)
inst_par_prop <- fp_par(padding.bottom = 24)

# Define header and footer
header_text <- fpar(
  ftext("HiTOP-BR (v1.0)", prop = fp_text(bold = TRUE)),
  fp_p = fp_par(text.align = "left")
)

footer_text <- fpar(
  ftext(
    "Copyright 2024 \u00A9 Hierarchical Taxonomy of Psychopathology Society",
    prop = fp_text(color = "grey")
  ),
  fp_p = fp_par(text.align = "right")
)

my_header <- block_list(header_text)
my_footer <- block_list(footer_text)

# 2. Build the Items Table
opts <- hitopbr_instructions$options

legend_text <- paste(
  opts$value,
  opts$label,
  sep = " = ",
  collapse = " \u2022 "
)

table_data <- hitopbr_items |>
  select(HBR, Text) |>
  mutate(Text = paste0(HBR, ".  ", Text)) |>
  select(-HBR)

opt_cols <- paste0("opt_", seq_len(nrow(opts)))

for (i in seq_len(nrow(opts))) {
  col_name <- opt_cols[i]
  table_data[[col_name]] <- as.character(opts$value[i])
}

even_rows <- seq(2, nrow(table_data), by = 2)
std_border <- fp_border(color = "black", width = 1.5)

table_1 <- table_data |>
  flextable() |>
  delete_part(part = "header") |>
  add_header_lines(values = legend_text) |>
  align(align = "center", part = "header") |>
  align(j = "Text", align = "left", part = "body") |>
  align(j = opt_cols, align = "center", part = "body") |>
  valign(valign = "center", part = "body") |>
  width(j = "Text", width = 5.4) |>
  width(j = opt_cols, width = 0.4) |>
  padding(padding = 5, part = "all") |>
  hline_top(border = std_border, part = "header") |>
  hline_bottom(border = std_border, part = "header") |>
  bg(i = even_rows, bg = "#f2f2f2", part = "body") |>
  fontsize(size = 10, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  set_table_properties(layout = "fixed", align = "left")


# 3. Build the Scoring Instructions Table
scoring_data <- hitopbr_scales |>
  arrange(Scale) |>
  mutate(
    Items = map_chr(
      itemdata,
      ~ {
        .x |>
          mutate(
            item_label = if_else(Reverse, paste0(HBR, "(R)"), as.character(HBR))
          ) |>
          pull(item_label) |>
          paste(collapse = ", ")
      }
    )
  ) |>
  select(Scale, Items)

n_rows <- ceiling(nrow(scoring_data) / 2)

if (nrow(scoring_data) %% 2 != 0) {
  scoring_data <- bind_rows(
    scoring_data,
    tibble(Scale = NA_character_, Items = NA_character_)
  )
}

col1 <- scoring_data[1:n_rows, ]
col2 <- scoring_data[(n_rows + 1):nrow(scoring_data), ] |>
  rename(Scale_2 = Scale, Items_2 = Items)

table_2 <- bind_cols(col1, col2) |>
  flextable() |>
  set_header_labels(
    Scale = "Scale",
    Items = "Items",
    Scale_2 = "Scale",
    Items_2 = "Items"
  ) |>
  align(align = "left", part = "all") |>
  padding(padding = 3, part = "all") |>
  fontsize(size = 9, part = "all") |>
  font(fontname = "Times New Roman", part = "all") |>
  width(j = c("Scale", "Scale_2"), width = 1.25) |>
  width(j = c("Items", "Items_2"), width = 2.25) |>
  colformat_char(na_str = "") |>
  hline_top(part = "header", border = std_border) |>
  hline_bottom(part = "header", border = std_border) |>
  set_table_properties(layout = "fixed", align = "left")


# 4. Assemble the Document
my_doc <- read_docx() |>
  # Add the main instructional text with custom font
  body_add_fpar(
    fpar(
      ftext(hitopbr_instructions$start, prop = inst_prop),
      fp_p = inst_par_prop
    )
  ) |>

  # Add the item options table
  body_add_flextable(value = table_1) |>

  # Add a page break
  body_add_break() |>

  # Add the scoring instructions text with custom font
  body_add_fpar(
    fpar(
      ftext("Scoring Instructions: ", prop = inst_prop_bold),
      ftext(
        "Average the responses for the following item numbers. No items are reverse-scored.",
        prop = inst_prop
      ),
      fp_p = inst_par_prop
    )
  ) |>

  # Add the scoring table
  body_add_flextable(value = table_2) |>

  # Apply the margins, header, and footer to the entire document block
  body_set_default_section(
    value = prop_section(
      page_size = page_size(width = 8.5, height = 11, orient = "portrait"),
      page_margins = page_mar(
        bottom = 0.75,
        top = 0.75,
        right = 0.75,
        left = 0.75
      ),
      header_default = my_header,
      footer_default = my_footer
    )
  )

# 5. Save the output
print(my_doc, target = "inst/extdata/hitopbr_1.0.docx")
