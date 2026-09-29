# Multi-instrument hitop-form response fixtures (M139).
#
# Writes five files to tests/testthat/fixtures/ in the shape D-080 gives a
# file from a study link that fields several instruments in one session: the
# five lead columns, one group of item columns per instrument in link order,
# then any `q_` answer columns. The page does not write this shape yet, so
# these files are written by rule here, not saved by the page.
#
# Run from the package root: Rscript data-raw/form_multi_fixtures.R
#
# The rule, restated in test-read_form_responses.R:
#   * study `fixture`, participant `p001`, submitted `2026-09-28T12:00:00Z`;
#   * `instrument` holds the stems in file order joined by single spaces, and
#     `form_build` one date per stem in that order: hitopbr 2026-09-20,
#     pid5bf 2026-09-18, pid5sf 2026-09-19;
#   * each instrument's items are answered by its own pattern, repeating down
#     its own columns from its first item: hitopbr 4, 3, 2, 1; pid5bf 0, 1,
#     2, 3; pid5sf 2, 0, 3, 1;
#   * the `-questions` files add `q_age` = `34`, `q_group` = `2` and an empty
#     `q_more` after the item columns;
#   * the `-shuffled` file adds `item_order` sixth, one group per stem joined
#     by " | ", each group a permutation of that stem's item numbers drawn
#     with seed 139. The item values follow the column rule above, not the
#     shown order.

instruments <- list(
  hitopbr = list(n = 45L, width = 2L, build = "2026-09-20", pattern = c(4L, 3L, 2L, 1L)),
  pid5bf = list(n = 25L, width = 2L, build = "2026-09-18", pattern = c(0L, 1L, 2L, 3L)),
  pid5sf = list(n = 100L, width = 3L, build = "2026-09-19", pattern = c(2L, 0L, 3L, 1L))
)

write_multi <- function(name, stems, questions = FALSE, shuffle = FALSE) {
  lead <- c("study", "participant", "instrument", "form_build", "submitted")
  header <- lead
  row <- c("fixture", "p001", paste(stems, collapse = " "),
           paste(vapply(instruments[stems], `[[`, "", "build"), collapse = " "),
           "2026-09-28T12:00:00Z")
  if (shuffle) {
    set.seed(139)
    groups <- vapply(stems, function(s) {
      paste(sample(instruments[[s]]$n), collapse = " ")
    }, character(1))
    header <- c(header, "item_order")
    row <- c(row, paste(groups, collapse = " | "))
  }
  for (s in stems) {
    inst <- instruments[[s]]
    header <- c(header, sprintf(paste0("%s_%0", inst$width, "d"), s, seq_len(inst$n)))
    row <- c(row, rep_len(inst$pattern, inst$n))
  }
  if (questions) {
    header <- c(header, "q_age", "q_group", "q_more")
    row <- c(row, "34", "2", "")
  }
  path <- file.path("tests", "testthat", "fixtures", name)
  writeLines(c(paste(header, collapse = ","), paste(row, collapse = ",")), path)
  invisible(path)
}

two <- c("hitopbr", "pid5bf")
three <- c("hitopbr", "pid5bf", "pid5sf")
write_multi("responses-multi-two.csv", two)
write_multi("responses-multi-two-questions.csv", two, questions = TRUE)
write_multi("responses-multi-three.csv", three)
write_multi("responses-multi-three-questions.csv", three, questions = TRUE)
write_multi("responses-multi-two-shuffled.csv", two, shuffle = TRUE)
