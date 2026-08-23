# No-regression and round-trip invariants for the `module` argument (M024).
#
# Two properties, checked across all three HiTOP-SR generators:
#   (1) module = NULL produces the same artifact as omitting the argument;
#   (2) a module naming ALL 76 scales reproduces the full instrument.
# (2) is the stronger check: it exercises the module code path end to end and
# asserts it is the identity when nothing is actually dropped.
#
# DOCX and REDCap zips are not byte-deterministic (zip member mtimes), so those
# comparisons parse the container; the flat Qualtrics .txt is compared by md5.

all_sr_scales <- function() hitop_module("hitopsr", hitopsr_scales$camelCase)

test_that("Qualtrics: module = NULL and an all-scales module match the default", {
  f_default <- withr::local_tempfile(fileext = ".txt")
  f_null <- withr::local_tempfile(fileext = ".txt")
  f_all <- withr::local_tempfile(fileext = ".txt")

  suppressMessages(generate_qualtrics_hitopsr(file = f_default))
  suppressMessages(generate_qualtrics_hitopsr(file = f_null, module = NULL))
  suppressMessages(generate_qualtrics_hitopsr(file = f_all, module = all_sr_scales()))

  md5 <- unname(tools::md5sum(c(f_default, f_null, f_all)))
  expect_equal(md5[[2]], md5[[1]])
  expect_equal(md5[[3]], md5[[1]])

  # And the default file really is the whole instrument.
  parsed <- read_qualtrics(f_default)
  expect_equal(parsed$questions$num, hitopsr_items$HSR)
})

test_that("REDCap: module = NULL and an all-scales module match the default", {
  f_default <- withr::local_tempfile(fileext = ".zip")
  f_null <- withr::local_tempfile(fileext = ".zip")
  f_all <- withr::local_tempfile(fileext = ".zip")

  suppressMessages(generate_redcap_hitopsr(file = f_default))
  suppressMessages(generate_redcap_hitopsr(file = f_null, module = NULL))
  suppressMessages(generate_redcap_hitopsr(file = f_all, module = all_sr_scales()))

  dd_default <- read_redcap_csv(f_default)
  expect_equal(read_redcap_csv(f_null), dd_default)
  expect_equal(read_redcap_csv(f_all), dd_default)

  items <- dd_default[dd_default$`Field Type` == "radio", ]
  expect_equal(nrow(items), nrow(hitopsr_items))
  expect_equal(items$`Field Label`, hitopsr_items$Text)
})

test_that("DOCX: module = NULL and an all-scales module match the default", {
  skip_if_no_docx()
  f_default <- withr::local_tempfile(fileext = ".docx")
  f_null <- withr::local_tempfile(fileext = ".docx")
  f_all <- withr::local_tempfile(fileext = ".docx")

  suppressMessages(generate_docx_hitopsr(file = f_default))
  suppressMessages(generate_docx_hitopsr(file = f_null, module = NULL))
  suppressMessages(generate_docx_hitopsr(file = f_all, module = all_sr_scales()))

  xml_default <- read_docx_xml(f_default)
  expect_equal(read_docx_xml(f_null), xml_default)
  expect_equal(read_docx_xml(f_all), xml_default)

  # The default document really carries every item and every scale.
  expect_true(grepl(paste0("405.  ", hitopsr_items$Text[405]), xml_default, fixed = TRUE))
  expect_true(grepl("Antisocial Behavior", xml_default, fixed = TRUE))
})

test_that("an all-scales module resolves to the whole instrument", {
  s <- all_sr_scales()
  expect_equal(s$items, hitopsr_items$HSR)
  expect_equal(s$nItems, 405)
  expect_equal(length(s$scales), 76)
})
