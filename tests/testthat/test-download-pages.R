# Locks what the instrument download pages render (vignettes/articles/
# download-*.Rmd): three cards in the row, an online-form strip under it on
# the five pages with a JSON export and none on the HSUM page, and an anchor
# to every manifest file. Each page's `downloads` chunk is extracted with
# knitr::purl() and evaluated against the sourced helpers, so the checks read
# rendered HTML rather than page source. Also locks the Instruments menu of
# `_pkgdown.yml` and the overview page's step list. Source-checkout only:
# neither vignettes/articles nor _pkgdown.yml is installed.

articles_dir <- function() {
  testthat::test_path("..", "..", "vignettes", "articles")
}

download_pages <- function() {
  list.files(articles_dir(), pattern = "^download-.*\\.Rmd$", full.names = TRUE)
}

page_stem <- function(page) {
  sub("^download-(.*)\\.Rmd$", "\\1", basename(page))
}

# Evaluate one page's `downloads` chunk with the helpers sourced and return
# the emitted `{=html}` fences' contents, one string per .emit_html() call,
# in output order.
render_downloads_chunk <- function(page) {
  tangled <- tempfile(fileext = ".R")
  on.exit(unlink(tangled), add = TRUE)
  knitr::purl(page, output = tangled, documentation = 1, quiet = TRUE)
  lines <- readLines(tangled, warn = FALSE)
  headers <- grep("^## ----", lines)
  begin <- grep("^## ----downloads[,-]", lines)
  if (length(begin) != 1) {
    stop("no single `downloads` chunk in ", basename(page))
  }
  end <- min(c(headers[headers > begin], length(lines) + 1)) - 1
  env <- new.env()
  sys.source(file.path(articles_dir(), "_download-helpers.R"), envir = env)
  out <- utils::capture.output(
    eval(parse(text = lines[(begin + 1):end]), envir = env)
  )
  text <- paste(out, collapse = "\n")
  fences <- regmatches(
    text,
    gregexpr("(?s)```\\{=html\\}\n.*?\n```", text, perl = TRUE)
  )[[1]]
  sub("\n```$", "", sub("^```\\{=html\\}\n", "", fences))
}

# hitop-form's decodeConfig(): restore the standard alphabet and padding,
# then read the JSON.
decode_c <- function(p) {
  b64 <- chartr("-_", "+/", p)
  pad <- (4 - nchar(b64) %% 4) %% 4
  rawToChar(jsonlite::base64_dec(paste0(b64, strrep("=", pad))))
}

test_that("each download page renders exactly three cards in its row", {
  skip_if(!dir.exists(articles_dir()), "vignettes/articles not available")
  skip_if_not_installed("knitr")
  pages <- download_pages()
  expect_length(pages, 6)
  for (page in pages) {
    fences <- render_downloads_chunk(page)
    row <- fences[startsWith(fences, '<div class="row mt-4 hitop-downloads">')]
    expect_length(row, 1)
    cards <- gregexpr('<div class="col-md-4 mb-4">', row, fixed = TRUE)[[1]]
    expect_equal(sum(cards > 0), 3, info = basename(page))
  }
})

test_that("the five form-backed pages render one online strip and the HSUM page none", {
  skip_if(!dir.exists(articles_dir()), "vignettes/articles not available")
  skip_if_not_installed("knitr")
  skip_if_not_installed("jsonlite")
  for (page in download_pages()) {
    stem <- page_stem(page)
    fences <- render_downloads_chunk(page)
    is_strip <- startsWith(fences, '<div class="hitop-online">')
    if (stem == "hitophsum") {
      expect_equal(sum(is_strip), 0, info = basename(page))
      expect_no_match(paste(fences, collapse = "\n"), "hitop-online", fixed = TRUE)
      next
    }
    expect_equal(sum(is_strip), 1, info = basename(page))
    # The strip sits under the row, not inside or before it.
    expect_gt(which(is_strip), which(startsWith(fences, '<div class="row')))
    strip <- fences[is_strip]

    # The link builder with the instrument filled in: `c` is the unpadded
    # base64url of {"instrument":"<stem>"}, the text JSON.stringify() writes.
    params <- regmatches(
      strip,
      gregexpr(
        'href="https://jmgirard\\.github\\.io/hitop-form/link\\.html\\?c=([^"]*)"',
        strip
      )
    )[[1]]
    expect_length(params, 1)
    p <- sub('.*\\?c=([^"]*)"$', "\\1", params)
    expect_match(p, "^[A-Za-z0-9_-]+$")
    expect_equal(decode_c(p), sprintf('{"instrument":"%s"}', stem))

    expect_match(
      strip, '<a href="../articles/online-collection.html"', fixed = TRUE
    )

    # The JSON export is a same-origin download button with a build badge.
    json <- regmatches(
      strip,
      gregexpr(
        sprintf('(?s)<a href="\\.\\./downloads/%s\\.json"[^>]*>.*?</a>', stem),
        strip,
        perl = TRUE
      )
    )[[1]]
    expect_length(json, 1)
    expect_match(json, sprintf('download="%s.json"', stem), fixed = TRUE)
    expect_match(json, 'class="hitop-build-badge"', fixed = TRUE)
  }
})

test_that("each page's rendered chunk links every file in its manifest rows and no other", {
  skip_if(!dir.exists(articles_dir()), "vignettes/articles not available")
  skip_if_not_installed("knitr")
  m <- latest_manifest()
  for (page in download_pages()) {
    stem <- page_stem(page)
    files <- m$file[sub("[_.].*$", "", m$file) == stem]
    expect_gt(length(files), 0, label = basename(page))
    html <- paste(render_downloads_chunk(page), collapse = "\n")
    linked <- regmatches(
      html, gregexpr('href="\\.\\./downloads/[^"]+"', html)
    )[[1]]
    linked <- sub('^href="\\.\\./downloads/([^"]+)"$', "\\1", linked)
    expect_setequal(linked, files)
  }
})

test_that("the Instruments menu reaches the link builder right after the module builder", {
  config <- testthat::test_path("..", "..", "_pkgdown.yml")
  skip_if(!file.exists(config), "_pkgdown.yml not available")
  skip_if_not_installed("yaml")
  menu <- yaml::read_yaml(config)$navbar$components$downloads$menu
  texts <- vapply(menu, function(e) if (is.null(e$text)) "" else e$text, "")
  i <- which(texts == "Build a HiTOP-SR Module")
  expect_length(i, 1)
  # A module-builder entry that closes the menu has nothing after it; an
  # empty entry then fails the two expectations below instead of erroring.
  following <- if (length(i) == 1 && i < length(menu)) menu[[i + 1]] else list()
  expect_equal(following$text, "Make a study link")
  expect_equal(
    following$href,
    "https://jmgirard.github.io/hitop-form/link.html"
  )
})

test_that("the overview page lists the four steps from instrument to scores with links that resolve", {
  skip_if(!dir.exists(articles_dir()), "vignettes/articles not available")
  text <- paste(
    readLines(file.path(articles_dir(), "overview.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  lists <- regmatches(
    text, gregexpr("(?s)<ol[^>]*>.*?</ol>", text, perl = TRUE)
  )[[1]]
  expect_length(lists, 1)
  items <- regmatches(
    lists, gregexpr("(?s)<li>.*?</li>", lists, perl = TRUE)
  )[[1]]
  expect_length(items, 4)
  steps <- c(
    "Choose an instrument", "Choose a route", "Collect responses",
    "Read and score"
  )
  for (k in seq_along(steps)) {
    expect_match(items[k], steps[k], fixed = TRUE)
  }

  hrefs <- regmatches(lists, gregexpr('href="[^"]+"', lists))[[1]]
  hrefs <- sub('^href="([^"]+)"$', "\\1", hrefs)
  hrefs <- hrefs[!grepl("^https?://", hrefs)]
  expect_gt(length(hrefs), 0)
  root <- testthat::test_path("..", "..")
  for (href in hrefs) {
    path <- sub("#.*$", "", href)
    name <- sub("\\.html$", "", basename(path))
    candidates <- if (grepl("^\\.\\./reference/", path)) {
      file.path(root, "man", paste0(name, ".Rd"))
    } else if (grepl("^\\.\\./articles/", path)) {
      c(
        file.path(root, "vignettes", "articles", paste0(name, ".Rmd")),
        file.path(root, "vignettes", paste0(name, ".Rmd"))
      )
    } else {
      character(0)
    }
    expect_true(any(file.exists(candidates)), info = href)
  }
})
