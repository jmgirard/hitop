# Locks the overview instrument page (vignettes/articles/overview.Rmd): it
# exists and links exactly the three main HiTOP instrument download pages
# (SR, BR, HSUM). Fails if a card link goes stale or the instrument set changes.

test_that("overview page links exactly the three main HiTOP instruments", {
  # Source-checkout only: vignettes/articles is not installed.
  articles <- testthat::test_path("..", "..", "vignettes", "articles")
  skip_if(!dir.exists(articles), "vignettes/articles not available")

  overview <- file.path(articles, "overview.Rmd")
  expect_true(file.exists(overview))
  text <- paste(readLines(overview, warn = FALSE), collapse = "\n")

  # Every instrument download-page link the overview points at.
  linked <- unique(regmatches(
    text,
    gregexpr("download-[a-z0-9]+\\.html", text)
  )[[1]])

  expect_setequal(
    linked,
    c(
      "download-hitopsr.html",
      "download-hitopbr.html",
      "download-hitophsum.html"
    )
  )

  # Each linked page resolves to a real article source (no broken card links).
  for (page in linked) {
    expect_true(
      file.exists(file.path(articles, sub("\\.html$", ".Rmd", page))),
      info = page
    )
  }
})

test_that("overview describes HiTOP-BR scales at their true hierarchy levels", {
  # Source-checkout only: vignettes/articles is not installed.
  articles <- testthat::test_path("..", "..", "vignettes", "articles")
  skip_if(!dir.exists(articles), "vignettes/articles not available")

  lines <- readLines(file.path(articles, "overview.Rmd"), warn = FALSE)
  br_start <- grep("HiTOP Brief Report \\(BR\\)", lines)
  expect_length(br_start, 1)
  br_block <- paste(lines[br_start:(br_start + 8)], collapse = " ")

  # The eight HiTOP-BR scales span three hierarchy levels: six spectra, the
  # Externalizing superspectrum, and the general p-factor. The card must not
  # equate all eight with spectra (the bug this test locks against).
  expect_false(
    grepl("eight[^.]*spectra", br_block, ignore.case = TRUE),
    info = "HiTOP-BR card must not call all 8 scales spectra"
  )
  # It must name the higher-order levels it actually covers.
  expect_match(br_block, "superspectrum", ignore.case = TRUE)
  expect_match(br_block, "p-factor", ignore.case = TRUE)
})
