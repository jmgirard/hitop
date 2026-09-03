# Retyping the response values moved nothing a user can see. Two surfaces are
# checked here: the distributed artifacts, which are built from the internal
# instruction objects whose `options$value` moved, and the scored results, which
# are computed from the response columns that moved.
#
# The artifact half runs anywhere. The scoring half compares against the merge
# base's copy of each dataset, so it runs on the branch that made the change and
# skips elsewhere (see helper-merge-base.R).

# ---- the distributed artifacts ----------------------------------------------

# The latest manifest row per file, as test-artifacts.R defines it: the row that
# describes the currently distributed file.
current_manifest <- function() {
  m <- hitop_artifacts[order(hitop_artifacts$file, hitop_artifacts$build_date), ]
  m[!duplicated(m$file, fromLast = TRUE), ]
}

# The package's own builders for the flat-text formats, keyed by the file they
# write. `hitophsum_qualtrics.qsf` is deliberately absent: it is a Qualtrics API
# export produced by devel/qualtrics_hitophsum.R, not built here (see the header
# of data-raw/artifacts.R), so the md5 lock in test-artifacts.R is its whole
# check.
flat_text_builders <- list(
  pid5_qualtrics.txt = function(file) generate_qualtrics_pid5(file = file),
  pid5sf_qualtrics.txt = function(file) generate_qualtrics_pid5sf(file = file),
  pid5bf_qualtrics.txt = function(file) generate_qualtrics_pid5bf(file = file),
  hitopsr_qualtrics.txt = function(file) generate_qualtrics_hitopsr(file = file),
  hitopbr_qualtrics.txt = function(file) generate_qualtrics_hitopbr(file = file),
  pid5_redcap.zip = function(file) generate_redcap_pid5(file = file),
  pid5sf_redcap.zip = function(file) generate_redcap_pid5sf(file = file),
  pid5bf_redcap.zip = function(file) generate_redcap_pid5bf(file = file),
  hitopsr_redcap.zip = function(file) generate_redcap_hitopsr(file = file),
  hitopbr_redcap.zip = function(file) generate_redcap_hitopbr(file = file),
  hitophsum_redcap.zip = function(file) generate_redcap_hitophsum(file = file)
)

test_that("every flat-text manifest row but the API-built QSF has a builder here", {
  # Without this the comparison below could quietly cover fewer files than the
  # manifest distributes -- a new export, or a QSF that gains a generator, would
  # simply not be compared. The exemption is named, so it has to be revisited
  # rather than inherited.
  rows <- current_manifest()
  flat_text <- rows$file[rows$format %in% c("qualtrics", "redcap")]
  expect_setequal(
    setdiff(flat_text, names(flat_text_builders)),
    "hitophsum_qualtrics.qsf"
  )
  expect_setequal(names(flat_text_builders), setdiff(flat_text, "hitophsum_qualtrics.qsf"))
})

# A REDCap export is a zip, and a zip is not byte-reproducible: its members
# carry mtimes (LESSONS M020). The data dictionary inside it is the flat text
# the generator actually wrote, so that is what is compared.
redcap_dictionary <- function(path) {
  members <- utils::unzip(path, list = TRUE)$Name
  csv <- grep("\\.csv$", members, value = TRUE)
  expect_length(csv, 1L)
  dir <- withr::local_tempdir()
  utils::unzip(path, files = csv, exdir = dir)
  readBin(file.path(dir, csv), "raw", file.size(file.path(dir, csv)))
}

file_bytes <- function(path) readBin(path, "raw", file.size(path))

# What this comparison can and cannot catch. It is a no-regression lock over
# content: a value, label or item that moved would change the text. It is not a
# probe of the type axis -- the generators render a response value through
# `as.character()`, and `as.character(1L)` and `as.character(1)` are the same
# string, so a build from internal data with the option values coerced back to
# double reproduces the committed `hitopsr_qualtrics.txt` and the
# `hitopsr_redcap.zip` dictionary byte for byte (checked by hand, 2026-09-03).
test_that("a fresh build reproduces every committed flat-text artifact", {
  extdata <- system.file("extdata", package = "hitop")
  expect_gt(length(flat_text_builders), 0L)
  for (name in names(flat_text_builders)) {
    fresh <- withr::local_tempfile(fileext = paste0(".", tools::file_ext(name)))
    suppressMessages(flat_text_builders[[name]](fresh))
    committed <- file.path(extdata, name)
    if (identical(tools::file_ext(name), "zip")) {
      expect_identical(redcap_dictionary(fresh), redcap_dictionary(committed), info = name)
    } else {
      expect_identical(file_bytes(fresh), file_bytes(committed), info = name)
    }
  }
})

# ---- the scored results -----------------------------------------------------

# One call per scoring entry point each dataset admits, in the shape the
# instrument's vignette uses, with `append = FALSE` wherever the argument
# exists. `data` is supplied by the caller so the same expressions run against
# the committed dataset and against the merge base's copy of it.
hitopsr_items_arg <- sprintf("hsr_%03d", 1:405)
hitopbr_items_arg <- sprintf("hbr_%02d", 1:45)
pid5sf_items_arg <- sprintf("pid5sf_%03d", 1:100)

scoring_calls <- list(
  ku_hitopsr = list(
    score = function(d) {
      score_hitopsr(d, items = hitopsr_items_arg, append = FALSE)
    },
    reliability = function(d) {
      reliability_hitopsr(d, items = hitopsr_items_arg, omega = FALSE)
    },
    interval = function(d) {
      scored <- score_hitopsr(d, items = hitopsr_items_arg, append = FALSE)
      interval_hitopsr(scored, scores = names(scored), append = FALSE)
    }
  ),
  ku_hitopbr = list(
    score = function(d) {
      score_hitopbr(d, items = hitopbr_items_arg, append = FALSE)
    },
    reliability = function(d) {
      reliability_hitopbr(d, items = hitopbr_items_arg, omega = FALSE)
    },
    interval = function(d) {
      scored <- score_hitopbr(d, items = hitopbr_items_arg, append = FALSE)
      interval_hitopbr(scored, scores = names(scored), append = FALSE)
    }
  ),
  ku_pid5sf = list(
    score = function(d) {
      score_pid5(d, items = pid5sf_items_arg, version = "SF", append = FALSE)
    },
    validity = function(d) {
      validity_pid5(d, items = pid5sf_items_arg, version = "SF", append = FALSE)
    },
    reliability = function(d) {
      reliability_pid5(d, items = pid5sf_items_arg, version = "SF", omega = FALSE)
    },
    norm = function(d) {
      scored <- score_pid5(d, items = pid5sf_items_arg, version = "SF")
      scored <- validity_pid5(scored, items = pid5sf_items_arg, version = "SF")
      norm_pid5(
        scored,
        scores = paste0("pid_", c(
          "negativeAffectivity", "detachment", "antagonism",
          "disinhibition", "psychoticism", "INCS"
        )),
        version = "SF",
        append = FALSE
      )
    }
  )
)

test_that("the scoring comparison covers every entry point each dataset admits", {
  # The named sets are the criterion's own domain. Asserted rather than derived
  # so a call quietly dropped from the list above fails here.
  expect_setequal(names(scoring_calls), c("ku_hitopsr", "ku_hitopbr", "ku_pid5sf"))
  expect_setequal(names(scoring_calls$ku_hitopsr), c("score", "reliability", "interval"))
  expect_setequal(names(scoring_calls$ku_hitopbr), c("score", "reliability", "interval"))
  expect_setequal(
    names(scoring_calls$ku_pid5sf),
    c("score", "validity", "reliability", "norm")
  )
})

test_that("no scored value moved when the response columns became integers", {
  base <- skip_without_merge_base()
  for (name in names(scoring_calls)) {
    old <- merge_base_object(name, base)
    # Vacuous unless the merge base really stores the responses as doubles.
    responses <- setdiff(names(old), c("participant", "biosex"))
    testthat::skip_if(
      all(vapply(old[responses], is.integer, logical(1))),
      paste0("the merge base already stores ", name, "'s responses as integer")
    )
    for (call in names(scoring_calls[[name]])) {
      fn <- scoring_calls[[name]][[call]]
      expect_identical(
        suppressWarnings(suppressMessages(fn(getExportedValue("hitop", name)))),
        suppressWarnings(suppressMessages(fn(old))),
        info = paste(name, call)
      )
    }
  }
})
