# The printed header of a HiTOP-SR Word form (M048).
#
# `generate_docx_hitopsr(title = )` defaults to NULL and resolves by what the
# form contains: a module form is headed "HiTOP-SR Module (v1.0)", a full
# instrument "HiTOP-SR (v1.0)" (D-037). The header lands in word/header*.xml,
# not word/document.xml, so `docx_header_title()` (helper-generators.R) reads
# it back out of the written file rather than trusting the argument.

m_title <- hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))

# ---- AC1: the two defaults --------------------------------------------------

test_that("a module Word form is headed as a module by default", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, module = m_title))

  expect_equal(docx_header_title(f), "HiTOP-SR Module (v1.0)")
})

test_that("a full-instrument Word form keeps the plain instrument header", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f))

  expect_equal(docx_header_title(f), "HiTOP-SR (v1.0)")
})

test_that("module = NULL is the same case as supplying no module at all", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(file = f, module = NULL))

  expect_equal(docx_header_title(f), "HiTOP-SR (v1.0)")
})

test_that("the deprecated subset = argument also earns the module header", {
  skip_if_no_docx()
  # The sentinel resolves AFTER resolve_module_arg(), so a caller still on the
  # old argument name gets the same header a `module =` caller gets. Resolving
  # it before that call would silently head their form as the full instrument.
  f <- withr::local_tempfile(fileext = ".docx")
  suppressWarnings(suppressMessages(
    generate_docx_hitopsr(file = f, subset = m_title)
  ))

  expect_equal(docx_header_title(f), "HiTOP-SR Module (v1.0)")
})

# ---- AC2: an explicit title is printed verbatim -----------------------------

test_that("an explicit title overrides the module default", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(
    file = f,
    module = m_title,
    title = "Anxiety Screener, Wave 2"
  ))

  expect_equal(docx_header_title(f), "Anxiety Screener, Wave 2")
})

test_that("an explicit title overrides the full-instrument default", {
  skip_if_no_docx()
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(
    file = f,
    title = "Anxiety Screener, Wave 2"
  ))

  expect_equal(docx_header_title(f), "Anxiety Screener, Wave 2")
})

test_that("an explicit title that equals the other default is still honored", {
  skip_if_no_docx()
  # The discriminating case: a caller who deliberately wants a module form
  # headed as the plain instrument gets exactly that. A resolver keyed on the
  # header's *text* rather than on `is.null(title)` would overwrite this.
  f <- withr::local_tempfile(fileext = ".docx")
  suppressMessages(generate_docx_hitopsr(
    file = f,
    module = m_title,
    title = "HiTOP-SR (v1.0)"
  ))

  expect_equal(docx_header_title(f), "HiTOP-SR (v1.0)")
})

test_that("title must be a single string or NULL", {
  f <- withr::local_tempfile(fileext = ".docx")
  expect_error(generate_docx_hitopsr(file = f, title = 1), "title")
  expect_error(generate_docx_hitopsr(file = f, title = c("a", "b")), "title")
  expect_error(generate_docx_hitopsr(file = f, title = NA), "title")
})

# ---- AC3: the committed full-instrument forms are unmoved -------------------

test_that("a default rebuild reproduces both committed HiTOP-SR forms", {
  skip_if_no_docx()
  # The two files `data-raw/artifacts.R` builds from this generator are the
  # oracle for the claim that the new default is conditional: an unconditional
  # rename would move the header on both, and any drift in the items table
  # would show here too. Committed bytes, not a freshly built comparand.
  committed <- c(
    us = system.file("extdata", "hitopsr_US.docx", package = "hitop"),
    a4 = system.file("extdata", "hitopsr_A4.docx", package = "hitop")
  )

  for (paper in names(committed)) {
    skip_if(committed[[paper]] == "")
    f <- withr::local_tempfile(fileext = ".docx")
    suppressMessages(generate_docx_hitopsr(file = f, papersize = paper))

    expect_equal(
      docx_header_title(f),
      docx_header_title(committed[[paper]]),
      info = paper
    )
    expect_equal(docx_header_title(f), "HiTOP-SR (v1.0)", info = paper)
    expect_equal(docx_item_rows(f), docx_item_rows(committed[[paper]]), info = paper)
  }
})
