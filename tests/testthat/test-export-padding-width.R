# The online exports pad item numbers to the INSTRUMENT's width, not to the
# width of whichever items one call exports (M079).
#
# `build_qualtrics_txt()` and `build_redcap_zip()` derived the padding width
# from `max(items[[1]])`, so a module whose items were all below 100 would have
# been written as `hsr_07` -- a name `label_hitopsr()` cannot match. Both now
# take the instrument's largest item number as a required `max_n` and pass it
# to `item_names()`.
#
# The direct-call tests below hand the builders a hand-cut two-item frame
# precisely because no module buildable from today's keying reaches the
# defect: the smallest per-scale largest HiTOP-SR item number is 151, so every
# real module is already three digits wide.

sr_probe_items <- function() {
  hitopsr_items[hitopsr_items$HSR %in% c(7L, 45L), ]
}

# ---- AC1: the width comes from `max_n`, not from the exported items ---------

test_that("Qualtrics question ids pad to the instrument width, not the export's", {
  f <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(build_qualtrics_txt(
    items = sr_probe_items(),
    max_n = max(hitopsr_items$HSR),
    instructions = hitopsr_instructions,
    file = f,
    block_name = NULL,
    id_prefix = "HSR",
    include_instructions = FALSE,
    breaks = NULL
  ))

  expect_identical(read_qualtrics(f)$questions$id, c("HSR_007", "HSR_045"))
})

test_that("REDCap variable names pad to the instrument width, not the export's", {
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(build_redcap_zip(
    items = sr_probe_items(),
    max_n = max(hitopsr_items$HSR),
    instructions = hitopsr_instructions,
    file = f,
    instrument = "HSR",
    form_name = "probe_form",
    required = TRUE,
    breaks = NULL
  ))

  # The dictionary opens with the descriptive instructions field, then the
  # items; both are asserted so nothing is quietly dropped from the comparison.
  expect_identical(
    read_redcap_csv(f)[["Variable / Field Name"]],
    c("hsr_instructions", "hsr_007", "hsr_045")
  )
})

# ---- AC2: the width is required, with no fallback to the exported items -----

test_that("build_qualtrics_txt() aborts when no instrument width is given", {
  err <- expect_error(
    build_qualtrics_txt(
      items = sr_probe_items(),
      instructions = hitopsr_instructions,
      file = withr::local_tempfile(fileext = ".txt"),
      block_name = NULL,
      id_prefix = "HSR",
      include_instructions = FALSE,
      breaks = NULL
    ),
    class = "rlang_error"
  )
  expect_match(conditionMessage(err), "max_n.+absent but must be supplied")
})

test_that("build_redcap_zip() aborts when no instrument width is given", {
  err <- expect_error(
    build_redcap_zip(
      items = sr_probe_items(),
      instructions = hitopsr_instructions,
      file = withr::local_tempfile(fileext = ".zip"),
      instrument = "HSR",
      form_name = "probe_form",
      required = TRUE,
      breaks = NULL
    ),
    class = "rlang_error"
  )
  expect_match(conditionMessage(err), "max_n.+absent but must be supplied")
})

test_that("both builders reject a width that is not a single whole number", {
  expect_error(
    build_qualtrics_txt(
      items = sr_probe_items(),
      max_n = "405",
      instructions = hitopsr_instructions,
      file = withr::local_tempfile(fileext = ".txt"),
      block_name = NULL,
      id_prefix = "HSR",
      include_instructions = FALSE,
      breaks = NULL
    ),
    "`max_n` argument must be a single whole number"
  )
  expect_error(
    build_redcap_zip(
      items = sr_probe_items(),
      max_n = c(45, 405),
      instructions = hitopsr_instructions,
      file = withr::local_tempfile(fileext = ".zip"),
      instrument = "HSR",
      form_name = "probe_form",
      required = TRUE,
      breaks = NULL
    ),
    "`max_n` argument must be a single whole number"
  )
})

# ---- T3: a real module keeps the full instrument's width end to end ---------

# The three HiTOP-SR scales with the smallest largest item numbers, so the
# module's own largest number (192) is well below the instrument's 405. Both
# are still three digits wide -- no module buildable from today's keying is
# narrower than that -- so this is an end-to-end check that the wrappers hand
# the instrument's width down, not a discriminator; the synthetic-frame tests
# above are what tell the two width sources apart.
sr_probe_scales <- c(
  "difficultiesReachingOrgasm",
  "workaholism",
  "trichotillomania"
)

sr_probe_module <- function() hitop_module("hitopsr", sr_probe_scales)

test_that("a HiTOP-SR module export names items at the full instrument width", {
  mod <- sr_probe_module()
  nums <- unlist(hitopsr_scales$itemNumbers[
    hitopsr_scales$camelCase %in% sr_probe_scales
  ])
  nums <- sort(unique(as.integer(nums)))
  expect_lt(max(nums), max(hitopsr_items$HSR))

  f_q <- withr::local_tempfile(fileext = ".txt")
  suppressMessages(generate_qualtrics_hitopsr(file = f_q, module = mod))
  expect_identical(
    read_qualtrics(f_q)$questions$id,
    item_names("HSR_", nums, max_n = max(hitopsr_items$HSR))
  )

  f_r <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopsr(file = f_r, module = mod))
  expect_identical(
    read_redcap_csv(f_r)[["Variable / Field Name"]],
    c("hsr_instructions", item_names("hsr_", nums, max_n = max(hitopsr_items$HSR)))
  )
})

# ---- AC3: no shipped online export's item names move -----------------------

# The generator that builds a manifest file, derived from the file's own name:
# `<stem>_<format>.<ext>` is built by `generate_<format>_<stem>()`.
manifest_generator <- function(file) {
  stem <- sub("\\.[^.]+$", "", file)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
  paste0("generate_", parts[[2]], "_", parts[[1]])
}

# Whether this package EXPORTS that generator -- the question AC3's domain is
# stated in, asked of the namespace's export list rather than of the search
# path, as `test-export-arg-guards.R` asks it.
has_manifest_generator <- function(file) {
  manifest_generator(file) %in% getNamespaceExports("hitop")
}

manifest_generator_fn <- function(file) {
  getExportedValue("hitop", manifest_generator(file))
}

# Item variable names as the committed or freshly built file carries them.
# Routed on the extension explicitly: an unknown container aborts here rather
# than reaching whichever reader the `else` branch happened to name.
export_item_names <- function(file) {
  switch(
    tools::file_ext(file),
    txt = read_qualtrics(file)$questions$id,
    zip = read_redcap_csv(file)[["Variable / Field Name"]],
    stop("no reader for ", file)
  )
}

online_manifest <- function() {
  m <- hitop_artifacts[hitop_artifacts$format %in% c("qualtrics", "redcap"), ]
  unique(m$file)
}

test_that("only the HiTOP-HSUM Qualtrics survey has no generator in this package", {
  files <- online_manifest()
  expect_gt(length(files), 0)
  has_generator <- vapply(files, has_manifest_generator, logical(1))
  # That .qsf is exported from Qualtrics itself, not written by this package
  # (`hitop_artifacts`' changes column records the API build). Naming it here
  # means a generator appearing for it later turns this red rather than
  # silently dropping it from the rebuild comparison below.
  expect_identical(files[!has_generator], "hitophsum_qualtrics.qsf")
  expect_equal(sum(has_generator), 11L)
})

# This is a no-regression lock, not a content oracle: it asserts only that the
# item names did not MOVE, never that they are the right names. What the names
# ought to be is derived from the instrument datasets by the tests above and in
# test-generate_{qualtrics,redcap}.R, so the committed artifacts are not
# standing in as an oracle for their own content (D-054, annotating D-010).
test_that("a fresh default build reproduces every shipped export's item names", {
  extdata <- system.file("extdata", package = "hitop")
  files <- Filter(has_manifest_generator, online_manifest())
  expect_equal(length(files), 11L)

  for (f in files) {
    ext <- paste0(".", tools::file_ext(f))
    fresh <- withr::local_tempfile(fileext = ext)
    generator <- manifest_generator_fn(f)
    suppressMessages(generator(file = fresh))

    # The expectation is read off the COMMITTED artifact, never off the fresh
    # build, so a naming drift cannot define itself as correct.
    committed <- export_item_names(file.path(extdata, f))
    expect_gt(length(committed), 0)
    expect_identical(export_item_names(fresh), committed, info = f)
  }
})
