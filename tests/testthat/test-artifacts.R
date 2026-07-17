# Locks the committed inst/extdata/ artifacts to the hitop_artifacts
# manifest (D-016): no distributed artifact may change without a new
# manifest row (a build-date version bump). Regenerate via
# data-raw/artifacts.R, which appends the new rows.

# The latest manifest row per file describes the currently distributed file.
latest_manifest <- function() {
  m <- hitop_artifacts[order(hitop_artifacts$file, hitop_artifacts$build_date), ]
  m[!duplicated(m$file, fromLast = TRUE), ]
}

test_that("hitop_artifacts has the documented structure", {
  expect_s3_class(hitop_artifacts, "tbl_df")
  expect_named(
    hitop_artifacts,
    c(
      "file",
      "instrument",
      "format",
      "instrument_version",
      "build_date",
      "md5",
      "changes"
    )
  )
  expect_s3_class(hitop_artifacts$build_date, "Date")
  expect_true(all(
    hitop_artifacts$format %in% c("docx_us", "docx_a4", "qualtrics", "redcap")
  ))
  expect_true(all(grepl("^[0-9a-f]{32}$", hitop_artifacts$md5)))
  expect_false(any(is.na(hitop_artifacts)))
})

test_that("every committed artifact has a current manifest row", {
  extdata <- system.file("extdata", package = "hitop")
  files <- list.files(extdata, pattern = "\\.(docx|txt|qsf|zip)$")
  expect_setequal(files, latest_manifest()$file)
})

test_that("no artifact changed without a manifest update (md5 lock)", {
  extdata <- system.file("extdata", package = "hitop")
  m <- latest_manifest()
  actual <- unname(tools::md5sum(file.path(extdata, m$file)))
  expect_equal(actual, m$md5, info = "Rebuild via data-raw/artifacts.R")
})

test_that("artifact file names carry no instrument version", {
  expect_false(any(grepl("[0-9]+\\.[0-9]+", latest_manifest()$file)))
})

test_that("committed DOCX footers carry the manifest build stamp", {
  skip_if_not_installed("officer")
  extdata <- system.file("extdata", package = "hitop")
  m <- latest_manifest()
  m <- m[grepl("^docx", m$format), ]
  for (i in seq_len(nrow(m))) {
    footer <- read_docx_footer(file.path(extdata, m$file[i]))
    expect_match(
      footer,
      paste0("Generated ", format(m$build_date[i], "%Y-%m-%d")),
      info = m$file[i]
    )
    expect_match(footer, "Copyright", info = m$file[i])
  }
})

test_that("the committed QSF SurveyName carries the manifest build date", {
  skip_if_not_installed("jsonlite")
  qsf_path <- system.file("extdata", "hitophsum_qualtrics.qsf", package = "hitop")
  qsf <- jsonlite::fromJSON(qsf_path, simplifyVector = FALSE)
  m <- latest_manifest()
  row <- m[m$file == "hitophsum_qualtrics.qsf", ]
  expect_match(
    qsf$SurveyEntry$SurveyName,
    format(row$build_date, "%Y-%m-%d")
  )
})

test_that("download-page links point at committed artifacts", {
  # Source-checkout only: vignettes/articles is not installed.
  articles <- testthat::test_path("..", "..", "vignettes", "articles")
  skip_if(!dir.exists(articles), "vignettes/articles not available")
  extdata <- system.file("extdata", package = "hitop")
  committed <- list.files(extdata, pattern = "\\.(docx|txt|qsf|zip)$")
  pages <- list.files(articles, pattern = "^download-.*\\.Rmd$", full.names = TRUE)
  expect_length(pages, 6)
  for (page in pages) {
    text <- paste(readLines(page, warn = FALSE), collapse = "\n")
    hrefs <- regmatches(
      text,
      gregexpr("inst/extdata/[A-Za-z0-9._-]+", text)
    )[[1]]
    expect_gt(length(hrefs), 0, label = basename(page))
    linked <- unique(basename(hrefs))
    expect_true(
      all(linked %in% committed),
      info = paste0(
        basename(page),
        ": stale link(s) ",
        paste(setdiff(linked, committed), collapse = ", ")
      )
    )
  }
})

test_that("download pages link the centralized import-instructions article", {
  # Source-checkout only: vignettes/articles is not installed.
  articles <- testthat::test_path("..", "..", "vignettes", "articles")
  skip_if(!dir.exists(articles), "vignettes/articles not available")

  # The article exists and carries the three anchored sections the pages target.
  article <- file.path(articles, "import-instructions.Rmd")
  expect_true(file.exists(article))
  atext <- paste(readLines(article, warn = FALSE), collapse = "\n")
  for (anchor in c("{#qualtrics-qsf}", "{#qualtrics-txt}", "{#redcap-zip}")) {
    expect_match(atext, anchor, fixed = TRUE)
  }

  pages <- list.files(articles, pattern = "^download-.*\\.Rmd$", full.names = TRUE)
  expect_length(pages, 6)
  for (page in pages) {
    text <- paste(readLines(page, warn = FALSE), collapse = "\n")
    # REDCap card links the article's REDCap section.
    expect_match(
      text, "articles/import-instructions.html#redcap-zip",
      fixed = TRUE, info = basename(page)
    )
    # Qualtrics card links the right section: HSUM ships a .qsf, the rest .txt.
    q_anchor <- if (basename(page) == "download-hitophsum.Rmd") {
      "articles/import-instructions.html#qualtrics-qsf"
    } else {
      "articles/import-instructions.html#qualtrics-txt"
    }
    expect_match(text, q_anchor, fixed = TRUE, info = basename(page))
    # The per-generator #details links are fully superseded.
    expect_no_match(
      text, "generate_redcap_[a-z0-9]+\\.html#details",
      info = basename(page)
    )
  }
})
