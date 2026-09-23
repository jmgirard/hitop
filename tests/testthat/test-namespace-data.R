# The package's functions read its datasets by bare name (`hitopsr_scales`,
# `pid_items`, ...). An installed package keeps its datasets in a lazy-load
# store that only the attached package exposes, so without the bindings
# `.onLoad()` adds, a call through `hitop::` with the package not attached
# fails with "object 'hitopsr_scales' not found".
#
# `devtools::load_all()` places the datasets inside the namespace itself, so
# under it the bug cannot occur and the installed copy on the library path may
# be an older build. Both tests therefore run only against an installed
# package, as in `R CMD check`.

skip_if_dev_load <- function() {
  testthat::skip_if(
    exists(".__DEVTOOLS__", envir = asNamespace("hitop"), inherits = FALSE),
    "the package is loaded by pkgload, which puts the datasets in the namespace"
  )
}

test_that("every dataset is bound in the namespace itself", {
  skip_if_dev_load()
  ns <- asNamespace("hitop")
  datasets <- utils::data(package = "hitop")$results[, "Item"]
  # A domain that emptied would make the check below vacuous.
  expect_true(length(datasets) > 0L)
  for (nm in datasets) {
    expect_true(exists(nm, envir = ns, inherits = FALSE), info = nm)
    # Each binding reads its own dataset, not the one bound last.
    if (exists(nm, envir = ns, inherits = FALSE)) {
      expect_identical(
        get(nm, envir = ns, inherits = FALSE),
        getExportedValue("hitop", nm),
        info = nm
      )
    }
  }
})

test_that("exported functions work through hitop:: without attaching the package", {
  skip_if_dev_load()
  skip_on_cran()
  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    'stopifnot(!"package:hitop" %in% search())',
    'm <- hitop::hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))',
    'cat("items:", m$nItems, "\\n")',
    's <- hitop::score_hitopsr(hitop::sim_hitopsr, items = 1:405, append = FALSE)',
    'cat("scales:", ncol(s), "\\n")',
    'p <- hitop::score_pid5(hitop::sim_pid5, items = 1:220, append = FALSE)',
    'cat("pid:", ncol(p), "\\n")'
  ), script)
  out <- withr::with_envvar(
    c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)),
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  status <- attr(out, "status")
  expect_true(is.null(status) || status == 0L, info = paste(out, collapse = "\n"))
  expect_true(any(out == "items: 8 "), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("^scales: [0-9]+ $", out)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("^pid: [0-9]+ $", out)), info = paste(out, collapse = "\n"))
})
