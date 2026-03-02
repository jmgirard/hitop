# HiTOP-BR
quarto::quarto_render(
  input = "data-raw/hitopbr.qmd",
  output_format = "docx",
  quarto_args = c("--output-dir", "../inst/extdata")
)

# HiTOP-SR
quarto::quarto_render(
  input = "data-raw/hitopsr.qmd",
  output_format = "docx",
  quarto_args = c("--output-dir", "../inst/extdata")
)
