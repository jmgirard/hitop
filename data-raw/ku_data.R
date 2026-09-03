## HiTOP-SR

## Response columns are read as integers, never guessed and coerced afterwards,
## so the `spec` attribute the reader stores stays a truthful record of the
## types. The item-text lookup below carries no response column and needs none.
integer_columns <- function(names) {
  do.call(
    readr::cols,
    setNames(rep(list(readr::col_integer()), length(names)), names)
  )
}

## The export's lookup carries the item texts as they were collected, before
## the trailing periods `hitopsr_items` standardized onto seven of them, so the
## texts are matched with any terminal period removed. The join is declared
## one-to-one so that an ambiguous match errors here rather than silently
## mis-mapping an item's responses.
without_terminal_period <- function(x) sub("\\.$", "", x)

ku_items <-
  readr::read_csv(
    "Y:/VIDAS/Study1/study1_items.csv",
    show_col_types = FALSE
  ) |>
  dplyr::mutate(TextKey = without_terminal_period(Text)) |>
  dplyr::inner_join(
    dplyr::mutate(hitopsr_items, TextKey = without_terminal_period(Text)),
    by = "TextKey",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(name = sprintf("hsr_%03d", HSR)) |>
  dplyr::pull(Old, name)
stopifnot(length(ku_items) == 405L)

## Which of the export's columns hold responses is decided by the rule the
## `select()` below uses -- a name starting with "hitop" -- read off the file's
## own header rather than pasted out as 405 literal names.
ku_hitopsr_file <- "Y:/VIDAS/Study1/qualtrics_2026-02-26.csv"
ku_hitopsr_responses <- grep(
  "^hitop",
  names(readr::read_csv(ku_hitopsr_file, n_max = 0, show_col_types = FALSE)),
  value = TRUE
)

ku_hitopsr <-
  readr::read_csv(
    ku_hitopsr_file,
    col_types = integer_columns(ku_hitopsr_responses)
  ) |>
  dplyr::filter(
    Finished == 1, # complete the study
    consent == 12, # consent to participate
    data_share == 1, # allow us to share data
    use_data == 1, # recommend that we use their data
    att_hitop == 2 | att_hitop2 == 4 # get at least one attention check right
  ) |>
  dplyr::mutate(
    participant = sprintf("P%03d", as.integer(factor(ResponseId))),
    biosex = factor(demo_biosex, levels = 0:1, labels = c("male", "female"))
  ) |>
  dplyr::select(participant, biosex, starts_with("hitop")) |>
  dplyr::rename(dplyr::any_of(ku_items)) |>
  dplyr::arrange(participant) |>
  dplyr::select(
    participant,
    biosex,
    dplyr::all_of(sprintf("hsr_%03d", 1:405))
  )
usethis::use_data(ku_hitopsr, overwrite = TRUE)

# ------------------------------------------------------------------------------

## HiTOP-BR
item_conversion <-
  dplyr::full_join(
    hitopsr_items,
    hitopbr_items,
    by = c("Text", "HSR", "Original")
  ) |>
  dplyr::select(HSR, HBR) |>
  tidyr::drop_na()

ku_hitopbr <-
  ku_hitopsr |>
  dplyr::select(
    participant,
    biosex,
    sprintf("hsr_%03d", item_conversion$HSR)
  ) |>
  setNames(c(
    "participant",
    "biosex",
    sprintf("hbr_%02d", item_conversion$HBR)
  )) |>
  dplyr::select(
    participant,
    biosex,
    dplyr::all_of(sprintf("hbr_%02d", 1:45))
  )
usethis::use_data(ku_hitopbr, overwrite = TRUE)

# ------------------------------------------------------------------------------

## PID-5-SF
ku_pid5sf <- readr::read_csv(
  "data-raw/ku_pid5sf.csv",
  col_types = integer_columns(sprintf("pid5sf_%03d", 1:100))
)
usethis::use_data(ku_pid5sf, overwrite = TRUE)
