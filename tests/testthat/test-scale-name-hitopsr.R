# Two HiTOP-SR scales carry the name the introduction paper's Table 1 prints,
# everywhere the package prints or returns it: the scale formerly abbreviated
# `NSSI` (M058) and the scale formerly called `Body Focus` (M059). Provenance
# and the source's own disagreement over the first of the two are in
# cairn/SOURCES.md, "HiTOP-SR scale names" and OQ-3.
#
# The assertions here run against shipped objects, the repository's own git
# history, and the committed Word artifacts. Whatever needs the source PDF
# lives in data-raw/verify_hitopsr_scale_name.R and
# data-raw/verify_hitopsr_names.R, which are maintainer-run.

## The adopted names, written out once. Everything below compares against these
## rather than re-deriving them, and nothing here calls
## snakecase::to_any_case() -- that is the function the renames themselves
## used, so re-deriving a stem would assert the deliverable against itself
## (IP2). `retired` is the spelling each rename replaced, swept for below.
adopted <- list(
  list(
    name = "Non-suicidal Self-injury",
    stem = "nonSuicidalSelfInjury",
    items = c(46L, 215L, 235L, 298L, 387L, 404L),
    retired = "nssi"
  ),
  list(
    name = "Appearance Focus",
    stem = "appearanceFocus",
    items = c(16L, 79L, 201L, 335L, 350L),
    retired = "body ?focus"
  )
)

test_that("each scale carries its adopted name in every keying table", {
  for (a in adopted) {
    expect_true(a$name %in% hitopsr_items$Scale, info = a$name)
    expect_identical(
      sum(hitopsr_items$Scale == a$name),
      length(a$items),
      info = a$name
    )

    expect_true(a$name %in% hitopsr_scales$Scale, info = a$name)
    expect_identical(
      hitopsr_scales$camelCase[hitopsr_scales$Scale == a$name],
      a$stem,
      info = a$name
    )
    expect_identical(
      hitopsr_scales$itemNumbers[[a$stem]],
      a$items,
      info = a$name
    )

    ## The definitions table keys on the same stem, which is what
    ## available_scales() joins on.
    expect_true(a$stem %in% hitopsr_definitions$camelCase, info = a$name)
    expect_true(a$name %in% hitopsr_definitions$Scale, info = a$name)
  }
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
  ## a list keyed by scale stem, so an old spelling can survive there with
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

test_that("no exported dataset still carries a retired name", {
  ## Datasets are enumerated from the package rather than hand-listed, so a
  ## dataset added later is swept without this test being edited.
  datasets <- utils::data(package = "hitop")$results[, "Item"]
  expect_gt(length(datasets), 0L)

  for (a in adopted) {
    offenders <- character(0)
    for (nm in datasets) {
      obj <- get(nm, envir = asNamespace("hitop"))
      leaves <- character_leaves(obj)
      if (!length(leaves)) next
      hits <- vapply(
        leaves,
        function(x) any(grepl(a$retired, x, ignore.case = TRUE)),
        logical(1)
      )
      if (any(hits)) offenders <- c(offenders, paste0(nm, "$", names(hits)[hits]))
    }
    expect_identical(offenders, character(0), info = a$retired)
  }
})

## The sweep above is only as good as its ability to see a violation. Planting
## one proves the walk reaches a list-column's element names, the surface a
## top-level column sweep would miss.
test_that("the dataset sweep can see a retired name where one hides", {
  planted <- hitopsr_scales
  names(planted$itemNumbers)[names(planted$itemNumbers) == "appearanceFocus"] <-
    "bodyFocus"
  leaves <- character_leaves(planted)
  hits <- vapply(
    leaves,
    function(x) any(grepl("body ?focus", x, ignore.case = TRUE)),
    logical(1)
  )
  expect_true(any(hits))
  expect_match(names(hits)[hits], "\\[names\\]", all = TRUE)
})

test_that("scored output names each scale by its stem", {
  scored <- hush_se(score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE))

  ## Written literally, never re-derived: these are the four columns the two
  ## renames produce, and the point of the test is that the deliverable
  ## produced them.
  expect_true("hsr_nonSuicidalSelfInjury" %in% names(scored))
  expect_true("hsr_nonSuicidalSelfInjury_se" %in% names(scored))
  expect_true("hsr_appearanceFocus" %in% names(scored))
  expect_true("hsr_appearanceFocus_se" %in% names(scored))
  expect_false(any(grepl("nssi", names(scored), ignore.case = TRUE)))
  expect_false(any(grepl("body ?focus", names(scored), ignore.case = TRUE)))
})

# ---- M059 AC1: the renamed columns over a probe set -------------------------

test_that("the renamed scale scores over every probe, matching a hand mean", {
  items <- c(16L, 79L, 201L, 335L, 350L)

  ## The five items are scored straight, not reverse-coded. Stated here rather
  ## than read from the table the scorer also reads, then checked against it:
  ## the recomputation below is only independent if its keying is.
  expect_false(any(hitopsr_items$Reverse[hitopsr_items$HSR %in% items]))

  ## An NA-injected copy, with a different pattern per respondent so that
  ## "available" and "complete" cannot agree by accident: respondent 1 loses
  ## one item of the scale, respondent 2 loses two, respondent 3 loses all
  ## five, and respondent 4 keeps every item.
  holes <- sim_hitopsr
  holes[1, sprintf("hsr_%03d", items[1])] <- NA
  holes[2, sprintf("hsr_%03d", items[1:2])] <- NA
  holes[3, sprintf("hsr_%03d", items)] <- NA

  for (dat in list(whole = sim_hitopsr, holed = holes)) {
    raw <- as.matrix(dat[sprintf("hsr_%03d", items)])
    for (miss in c("available", "complete")) {
      for (se in c(FALSE, TRUE)) {
        scored <- hush_se(score_hitopsr(
          dat,
          items = 1:405,
          missing = miss,
          calc_se = se,
          append = FALSE
        ))
        expect_true("hsr_appearanceFocus" %in% names(scored))
        expect_false(any(grepl("body ?focus", names(scored), ignore.case = TRUE)))

        ## The hand recomputation: a plain row mean of the five item columns,
        ## with na.rm following the `missing` setting the call used.
        expected <- rowMeans(raw, na.rm = identical(miss, "available"))
        expected[is.nan(expected)] <- NA_real_
        expect_equal(
          scored$hsr_appearanceFocus,
          unname(expected),
          info = paste(miss, se)
        )

        expect_identical(
          "hsr_appearanceFocus_se" %in% names(scored),
          se,
          info = paste(miss, se)
        )
      }
    }
  }
})

# ---- M059 AC2: only the name moved ------------------------------------------

test_that("the keying tables differ from the merge base only in the name", {
  base <- skip_without_rename_base()
  old_name <- "Body Focus"
  new_name <- "Appearance Focus"
  old_stem <- "bodyFocus"
  new_stem <- "appearanceFocus"

  ## hitopsr_items, joined on the item number.
  old_items <- merge_base_object("hitopsr_items", base)
  expect_setequal(names(old_items), names(hitopsr_items))
  expect_identical(sort(old_items$HSR), sort(hitopsr_items$HSR))
  ord_old <- order(old_items$HSR)
  ord_new <- order(hitopsr_items$HSR)
  for (col in names(hitopsr_items)) {
    a <- old_items[[col]][ord_old]
    b <- hitopsr_items[[col]][ord_new]
    if (identical(col, "Scale")) {
      expect_identical(which(a != b), which(b == new_name), info = col)
      expect_true(all(a[a != b] == old_name), info = col)
    } else {
      expect_identical(a, b, info = col)
    }
  }

  ## hitopsr_scales, joined on the renamed scale: the old table's name is
  ## rewritten to the new one and the two are compared row for row, which also
  ## checks the row order the alphabetical sort induces.
  old_scales <- merge_base_object("hitopsr_scales", base)
  expect_true(old_name %in% old_scales$Scale)
  keyed <- old_scales
  keyed$Scale[keyed$Scale == old_name] <- new_name
  keyed$camelCase[keyed$camelCase == old_stem] <- new_stem
  names(keyed$itemNumbers)[names(keyed$itemNumbers) == old_stem] <- new_stem
  keyed <- keyed[order(keyed$Scale), ]
  expect_identical(keyed$Scale, hitopsr_scales$Scale)
  expect_identical(keyed$camelCase, hitopsr_scales$camelCase)
  expect_identical(keyed$nItems, hitopsr_scales$nItems)
  ## The list-column's element names are keyed by the stem the rename changes,
  ## so they are compared as well as its values.
  expect_identical(names(keyed$itemNumbers), names(hitopsr_scales$itemNumbers))
  expect_identical(unname(keyed$itemNumbers), unname(hitopsr_scales$itemNumbers))
  for (i in seq_len(nrow(keyed))) {
    expect_equal(
      as.data.frame(keyed$itemdata[[i]]),
      as.data.frame(hitopsr_scales$itemdata[[i]]),
      info = hitopsr_scales$Scale[[i]]
    )
  }

  ## hitopsr_definitions, keyed the same way.
  old_defs <- merge_base_object("hitopsr_definitions", base)
  keyed_defs <- old_defs
  keyed_defs$Scale[keyed_defs$Scale == old_name] <- new_name
  keyed_defs$camelCase[keyed_defs$camelCase == old_stem] <- new_stem
  keyed_defs <- keyed_defs[order(keyed_defs$camelCase), ]
  current_defs <- hitopsr_definitions[order(hitopsr_definitions$camelCase), ]
  for (col in names(hitopsr_definitions)) {
    expect_identical(keyed_defs[[col]], current_defs[[col]], info = col)
  }

  ## hitopsr_subscales does not join on the renamed scale and must not have
  ## moved at all.
  expect_identical(merge_base_object("hitopsr_subscales", base), hitopsr_subscales)
})

test_that("the keyed diff can see a change outside the name", {
  base <- skip_without_rename_base()
  old_items <- merge_base_object("hitopsr_items", base)
  ## Planted: an item's reverse flag flipped. The comparison above asserts
  ## every non-Scale column identical, so this must be visible.
  planted <- old_items
  planted$Reverse[[1]] <- !planted$Reverse[[1]]
  ord <- order(planted$HSR)
  expect_false(identical(
    planted$Reverse[ord],
    hitopsr_items$Reverse[order(hitopsr_items$HSR)]
  ))
})

# ---- M059 AC5: exactly the two Word files and their staged copies -----------

test_that("the rebuild touched only the two Word forms and their copies", {
  base <- skip_without_rename_base()
  changed <- git_run(
    "diff", "--name-only", base, "--",
    "inst/extdata", "pkgdown/assets/downloads"
  )
  skip_if(is.null(changed), "git diff unavailable")
  expect_setequal(
    changed,
    c(
      "inst/extdata/hitopsr_A4.docx",
      "inst/extdata/hitopsr_US.docx",
      "pkgdown/assets/downloads/hitopsr_A4.docx",
      "pkgdown/assets/downloads/hitopsr_US.docx"
    )
  )

  ## The manifest gains one row per rebuilt artifact. It keys on inst/extdata
  ## basenames, so the two staged copies do not have rows of their own; they
  ## are locked to these rows by the md5 check in test-artifacts.R.
  old_manifest <- merge_base_object("hitop_artifacts", base)
  expect_identical(nrow(hitop_artifacts), nrow(old_manifest) + 2L)
  added <- hitop_artifacts[!Reduce(`|`, lapply(
    seq_len(nrow(old_manifest)),
    function(i) {
      Reduce(`&`, lapply(names(hitop_artifacts), function(col) {
        hitop_artifacts[[col]] == old_manifest[[col]][[i]]
      }))
    }
  )), ]
  expect_setequal(added$file, c("hitopsr_A4.docx", "hitopsr_US.docx"))
})

# ---- M059 AC6: the built Word forms carry no retired name -------------------

## All the text a .docx renders: the body, plus the headers and footers, which
## live in their own parts and which read_docx_xml() cannot see.
docx_all_text <- function(file) {
  exdir <- tempfile("docx")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  parts <- grep(
    "^word/(document|header[0-9]*|footer[0-9]*)\\.xml$",
    utils::unzip(file, list = TRUE)$Name,
    value = TRUE
  )
  if (!length(parts)) return(character(0))
  utils::unzip(file, files = parts, exdir = exdir)
  xml <- paste(
    unlist(lapply(file.path(exdir, parts), readLines, warn = FALSE,
                  encoding = "UTF-8")),
    collapse = "\n"
  )
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  gsub("<[^>]+>", "", runs)
}

test_that("the rebuilt Word forms print the adopted names and no retired one", {
  installed <- file.path(
    system.file("extdata", package = "hitop"),
    c("hitopsr_US.docx", "hitopsr_A4.docx")
  )
  staged <- file.path(
    repo_root(), "pkgdown", "assets", "downloads",
    c("hitopsr_US.docx", "hitopsr_A4.docx")
  )
  ## The staged copies are dropped when they are not there, so this sweep can
  ## run from a built tarball that carries only the installed pair. That the
  ## directory exists at all in a checkout is asserted once, in
  ## test-artifacts.R.
  files <- c(installed, staged[file.exists(staged)])
  expect_gte(length(files), 2L)

  for (f in files) {
    text <- docx_all_text(f)
    ## The scoring page prints every scale name, so a form whose text does not
    ## contain the adopted names is not a form this sweep can speak about.
    expect_true("Appearance Focus" %in% text, info = basename(f))
    expect_true("Non-suicidal Self-injury" %in% text, info = basename(f))
    for (a in adopted) {
      expect_false(
        any(grepl(a$retired, text, ignore.case = TRUE)),
        info = paste(basename(f), a$retired)
      )
    }
  }
})

# ---- The changelog's claims, enforced ---------------------------------------

test_that("the renamed columns sit where NEWS says they do", {
  scored <- hush_se(score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE))
  ## NEWS tells a reader selecting scored columns by position where the two
  ## renamed scales moved to. Written as literals, since the point is that
  ## these are the positions the release notes name.
  expect_identical(which(names(scored) == "hsr_nonSuicidalSelfInjury"), 451L)
  expect_identical(which(names(scored) == "hsr_nonSuicidalSelfInjury_se"), 527L)
  expect_identical(which(names(scored) == "hsr_appearanceFocus"), 408L)
  expect_identical(which(names(scored) == "hsr_appearanceFocus_se"), 484L)
})

test_that("a module cannot be built from a retired scale name", {
  for (a in adopted) {
    expect_no_error(hitop_module("hitopsr", scales = a$name))
  }
  ## Which failure, not merely a failure: an unrecognized scale name, not a
  ## missing argument or an unsupported instrument.
  expect_error(
    hitop_module("hitopsr", scales = "NSSI"),
    'Unknown scale name: "NSSI"',
    fixed = TRUE
  )
  expect_error(
    hitop_module("hitopsr", scales = "Body Focus"),
    'Unknown scale name: "Body Focus"',
    fixed = TRUE
  )
})

test_that("a saved descriptor recording a retired name is rejected on read", {
  skip_if_not_installed("jsonlite")
  path <- withr::local_tempfile(fileext = ".json")
  m <- hitop_module("hitopsr", scales = "Appearance Focus")
  write_module(m, path)
  ## Rewrite the saved scale name to the retired spelling, leaving the item
  ## numbers it records alone, so the rejection is about the name.
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  raw$scales <- list("Body Focus")
  jsonlite::write_json(raw, path, auto_unbox = TRUE)
  expect_error(read_module(path), 'Unknown scale name: "Body Focus"', fixed = TRUE)
})
