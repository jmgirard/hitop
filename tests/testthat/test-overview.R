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
