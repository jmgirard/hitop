# The HiTOP-SR scale formerly abbreviated `NSSI` carries its full name
# everywhere the package prints or returns it (M058; D-041; provenance and the
# source's own disagreement in cairn/SOURCES.md, "HiTOP-SR scale names" and
# OQ-3).
#
# These assertions run against shipped objects only, so they run anywhere. The
# checks that need the source PDF or the merge-base build live in
# data-raw/verify_hitopsr_scale_name.R and data-raw/verify_hitopsr_rename.R,
# which are maintainer-run.

## The adopted name, written out once. Everything below compares against this
## rather than re-deriving it, and nothing here calls
## snakecase::to_any_case() -- that is the function the rename itself used, so
## re-deriving the stem would assert the deliverable against itself (IP2).
adopted_name <- "Non-suicidal Self-injury"
adopted_stem <- "nonSuicidalSelfInjury"

test_that("the scale carries its full name in every keying table", {
  expect_true(adopted_name %in% hitopsr_items$Scale)
  expect_identical(sum(hitopsr_items$Scale == adopted_name), 6L)

  expect_true(adopted_name %in% hitopsr_scales$Scale)
  expect_identical(
    hitopsr_scales$camelCase[hitopsr_scales$Scale == adopted_name],
    adopted_stem
  )
  expect_identical(
    hitopsr_scales$itemNumbers[[adopted_stem]],
    hitopsr_items$HSR[hitopsr_items$Scale == adopted_name]
  )

  ## The definitions table keys on the same stem, which is what
  ## available_scales() joins on.
  expect_true(adopted_stem %in% hitopsr_definitions$camelCase)
  expect_true(adopted_name %in% hitopsr_definitions$Scale)
})

## Every character vector reachable inside a dataset, named by its path.
## Recursion matters: hitopsr_scales$itemdata is a list-column of tibbles, so a
## sweep of the top-level columns alone would miss the item text and the
## subscale labels inside it.
character_leaves <- function(x, path = "") {
  if (is.character(x)) {
    out <- list(x)
    names(out) <- path
    return(out)
  }
  if (is.factor(x)) {
    out <- list(as.character(x))
    names(out) <- path
    return(out)
  }
  if (!is.list(x)) return(list())
  nms <- names(x)
  ## A list's own names are a naming surface too: hitopsr_scales$itemNumbers is
  ## a list keyed by scale stem, so the old spelling can survive there with
  ## every value clean. Sweeping values alone would miss it.
  own <- list()
  if (!is.null(nms)) {
    own <- list(nms)
    names(own) <- if (nzchar(path)) paste0(path, " [names]") else "[names]"
  }
  if (is.null(nms)) nms <- paste0("[[", seq_along(x), "]]")
  c(own, do.call(c, lapply(seq_along(x), function(i) {
    sub <- if (nzchar(path)) paste0(path, "$", nms[[i]]) else nms[[i]]
    character_leaves(x[[i]], sub)
  })))
}

test_that("no exported dataset still carries the old abbreviation", {
  ## Datasets are enumerated from the package rather than hand-listed, so a
  ## dataset added later is swept without this test being edited.
  datasets <- utils::data(package = "hitop")$results[, "Item"]
  expect_gt(length(datasets), 0L)

  offenders <- character(0)
  for (nm in datasets) {
    obj <- get(nm, envir = asNamespace("hitop"))
    ## Every character column, including those nested inside list-columns --
    ## hitopsr_scales$itemdata holds one tibble per scale.
    leaves <- character_leaves(obj)
    if (!length(leaves)) next
    hits <- vapply(leaves, function(x) any(grepl("nssi", x, ignore.case = TRUE)), logical(1))
    if (any(hits)) offenders <- c(offenders, paste0(nm, "$", names(hits)[hits]))
  }
  expect_identical(offenders, character(0))
})

test_that("scored output names the scale by its stem", {
  scored <- score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE)

  ## Written literally, never re-derived: these are the two columns D-041
  ## renames, and the point of the test is that the deliverable produced them.
  expect_true("hsr_nonSuicidalSelfInjury" %in% names(scored))
  expect_true("hsr_nonSuicidalSelfInjury_se" %in% names(scored))
  expect_false(any(grepl("nssi", names(scored), ignore.case = TRUE)))
})
