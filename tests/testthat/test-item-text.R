# Formatting invariants on instrument item text.
#
# The HiTOP-SR source export left 7 of 405 items (and 1 matching HiTOP-BR item)
# without a trailing period; these were standardized with maintainer sign-off
# on 2026-07-16 (see project/SOURCES.md, "Note on HiTOP-SR/BR item-text
# punctuation"). These tests pin that invariant so a future CSV edit or
# instrument update cannot silently reintroduce the inconsistency.
#
# Note: pid_items$Text intentionally uses period-free sentence fragments
# (0/220), so it is *not* covered here.

test_that("every hitopsr_items text ends in a period", {
  expect_true(all(grepl("\\.$", hitopsr_items$Text)))
})

test_that("every hitopbr_items text ends in a period", {
  expect_true(all(grepl("\\.$", hitopbr_items$Text)))
})

test_that("SR/BR item text has no stray whitespace", {
  for (txt in list(hitopsr_items$Text, hitopbr_items$Text)) {
    expect_false(any(grepl("^[[:space:]]|[[:space:]]$", txt)))
    expect_false(any(grepl("  ", txt, fixed = TRUE)))
  }
})
