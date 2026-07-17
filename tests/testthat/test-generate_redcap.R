# Ground-truth tests for the REDCap export family (M10).
#
# Oracle: unzip the instrument ZIP, read its data-dictionary CSV back
# (helper-generators.R), and compare to the source datasets derived
# independently. The shared path (5 instruments) runs through build_redcap_zip()
# (R/generate_redcap.R:197); HiTOP-HSUM has a bespoke branching-logic path
# (R/generate_redcap.R:336) whose expected strings are hand-derived below from
# the published rule logic + the hitophsum_choices values.

# zip writing needs an external zip utility; reading (unzip) is internal.
skip_if_no_zip <- function() skip_if(unname(Sys.which("zip")) == "")

redcap_cols <- c(
  "Variable / Field Name",
  "Form Name",
  "Section Header",
  "Field Type",
  "Field Label",
  "Choices, Calculations, OR Slider Labels",
  "Field Note",
  "Text Validation Type OR Show Slider Number",
  "Text Validation Min",
  "Text Validation Max",
  "Identifier?",
  "Branching Logic (Show field only if...)",
  "Required Field?",
  "Custom Alignment",
  "Question Number (surveys only)"
)

# ---- Shared path: HiTOP-BR (full detail) ------------------------------------

test_that("generate_redcap_hitopbr builds a faithful data dictionary", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopbr(file = f))
  r <- read_redcap_csv(f)

  # 15 data-dictionary columns in exact order; instruction row + 45 items.
  expect_identical(names(r), redcap_cols)
  expect_equal(nrow(r), 46L)

  # Row 1 is the descriptive instruction row; items follow.
  expect_identical(r[["Variable / Field Name"]][1], "hbr_instructions")
  expect_identical(r[["Field Type"]][1], "descriptive")
  expect_identical(r[["Field Label"]][1], hitopbr_instructions$start[1])

  items <- r[-1, ]
  # Field names are lowercased instrument prefix + zero-padded number (width 2).
  expect_identical(items[["Variable / Field Name"]], sprintf("hbr_%02d", hitopbr_items$HBR))
  expect_true(all(items[["Field Type"]] == "radio"))
  expect_identical(items[["Field Label"]], hitopbr_items$Text)

  # Choices string equals the formatted instruction options, independently built.
  expected_choices <- paste(
    paste(hitopbr_instructions$options$value, hitopbr_instructions$options$label, sep = ", "),
    collapse = " | "
  )
  expect_true(all(items[["Choices, Calculations, OR Slider Labels"]] == expected_choices))

  # required = TRUE (default) -> every item "y"; the instruction row is blank.
  expect_true(all(items[["Required Field?"]] == "y"))
  expect_identical(r[["Required Field?"]][1], "")

  # Default breaks = 15 -> a <br> Section Header at item positions 16 and 31.
  expect_equal(which(items[["Section Header"]] == "<br>"), c(16L, 31L))
})

test_that("required = FALSE flips every item to 'n'", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitopbr(file = f, required = FALSE))
  items <- read_redcap_csv(f)[-1, ]
  expect_true(all(items[["Required Field?"]] == "n"))
})

# ---- Shared path: PID-5 FULL (padding width + page-break count) -------------

test_that("generate_redcap_pid5 pads to 3 digits and paginates correctly", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_pid5(file = f))
  r <- read_redcap_csv(f)

  expect_equal(nrow(r), 221L) # 220 items + instruction row
  items <- r[-1, ]

  # Items sorted by FULL number, zero-padded to width 3.
  src <- pid_items[!is.na(pid_items$FULL), ]
  src <- src[order(src$FULL), ]
  expect_identical(items[["Variable / Field Name"]], sprintf("pid5_%03d", src$FULL))
  expect_identical(items[["Field Label"]], src$Text)

  # Default breaks = 15 across 220 items -> <br> at 16, 31, ..., 211.
  expect_equal(which(items[["Section Header"]] == "<br>"), seq(16L, 211L, by = 15L))
})

# ---- Smoke coverage: the 5 shared-path generators ---------------------------

test_that("all shared-path REDCap generators produce a valid dictionary", {
  skip_if_no_zip()
  cases <- list(
    list(fn = generate_redcap_hitopbr, n = 45L),
    list(fn = generate_redcap_hitopsr, n = nrow(hitopsr_items)),
    list(fn = generate_redcap_pid5,    n = 220L),
    list(fn = generate_redcap_pid5sf,  n = 100L),
    list(fn = generate_redcap_pid5bf,  n = 25L)
  )
  for (case in cases) {
    f <- withr::local_tempfile(fileext = ".zip")
    suppressMessages(case$fn(file = f))
    expect_gt(file.info(f)$size, 0)
    r <- read_redcap_csv(f)
    expect_identical(names(r), redcap_cols)
    expect_equal(nrow(r), case$n + 1L) # + instruction row
  }
})

# ---- HSUM bespoke path: branching logic + field-type overrides --------------
#
# Expected strings hand-derived from the three rule types in the HSUM path and
# the hitophsum_choices values (nicotine_forms = 1:6; yn_pnts = {1,0,99}):
#   count>1   : sum of if([var(v)],1,0) over all parent values, "> 1"
#   comparison: radio parent -> "[var] >= 3"; when the parent choice set
#               contains 99 (Prefer not to say), a " and [var] <> '99'" guard
#               follows so PNTS never satisfies a threshold gate
#   discrete  : radio "[var] = 'v'"; checkbox "[var(v)] = '1'"
# Other-drug symptom items additionally carry the most-frequent argmax term
# (default other_drug_rule): a sum of if() indicators, one per competing
# other-drug frequency, that must equal 0 (no competitor strictly greater;
# ties therefore show both substances).

# The ten other-drug abbreviations in hitophsum_items order.
hsum_other_abbrs <- c(
  "can", "coc", "stm", "met", "inh", "sed", "hal", "sop", "pop", "oth"
)

# Hand-derived most-frequent branching for one other-drug symptom item.
hsum_argmax_bl <- function(abbr) {
  self <- sprintf("hsum_%s_freq", abbr)
  competitors <- sprintf(
    "hsum_%s_freq",
    setdiff(hsum_other_abbrs, abbr)
  )
  paste0(
    "[", self, "] >= 3 and [", self, "] <> '99' and (",
    paste(
      sprintf("if([%s] <> '99' and [%s] > [%s],1,0)", competitors, competitors, self),
      collapse = " + "
    ),
    ") = 0"
  )
}

test_that("HSUM REDCap branching logic matches the hand-derived rule strings", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitophsum(file = f))
  r <- read_redcap_csv(f)
  bl <- function(var) r[["Branching Logic (Show field only if...)"]][r[[1]] == var]

  # Rule A: count>1 over checkbox parent hsum_nic_form (values 1:6).
  expect_identical(
    bl("hsum_nic_most"),
    paste0(
      "(",
      paste(sprintf("if([hsum_nic_form(%d)],1,0)", 1:6), collapse = " + "),
      ") > 1"
    )
  )

  # Discrete list over checkbox parent: free-text nicotine quantity shows
  # only for forms 2/4/5/6 (not cigarettes = 1, not cigars = 3).
  expect_identical(
    bl("hsum_nic_quant_oth"),
    paste0(
      "(",
      paste(sprintf("[hsum_nic_form(%d)] = '1'", c(2, 4, 5, 6)), collapse = " OR "),
      ")"
    )
  )

  # Comparison gates carry the PNTS guard: 99 never satisfies a threshold.
  expect_identical(
    bl("hsum_alc_sud01"),
    "[hsum_alc_freq] >= 3 and [hsum_alc_freq] <> '99'"
  )
  expect_identical(
    bl("hsum_nic_sud01"),
    "[hsum_nic_freq] >= 5 and [hsum_nic_freq] <> '99'"
  )
  expect_identical(
    bl("hsum_alc_with33"),
    "[hsum_alc_freq] >= 3 and [hsum_alc_freq] <> '99'"
  )

  # No emitted branching string anywhere lets 99 through a comparison gate:
  # every "[var] >= n" is followed by its "[var] <> '99'" guard.
  all_bl <- r[["Branching Logic (Show field only if...)"]]
  comparisons <- regmatches(
    all_bl,
    gregexpr("\\[(hsum_[a-z_]+)\\] >= [0-9]+(?! and \\[\\1\\] <> '99')",
             all_bl, perl = TRUE)
  )
  expect_identical(unique(unlist(comparisons)), character(0))

  # Rule D (single value): radio parent hsum_alc, value 1.
  expect_identical(bl("hsum_alc_freq"), "[hsum_alc] = '1'")

  # Rule D (single value): checkbox parent hsum_nic_form, value 6.
  expect_identical(bl("hsum_nic_form_txt"), "[hsum_nic_form(6)] = '1'")
})

test_that("HSUM other_drug_rule = 'most_frequent' (default) emits argmax gates", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitophsum(file = f))
  r <- read_redcap_csv(f)
  bl <- function(var) r[["Branching Logic (Show field only if...)"]][r[[1]] == var]

  # First other drug (cannabis) and last (other), SUD and WITH items alike.
  expect_identical(bl("hsum_can_sud01"), hsum_argmax_bl("can"))
  expect_identical(bl("hsum_oth_with33"), hsum_argmax_bl("oth"))

  # Alcohol and nicotine symptom gates are untouched by the rule.
  expect_identical(
    bl("hsum_alc_sud01"),
    "[hsum_alc_freq] >= 3 and [hsum_alc_freq] <> '99'"
  )
  expect_identical(
    bl("hsum_nic_with01"),
    "[hsum_nic_freq] >= 5 and [hsum_nic_freq] <> '99'"
  )
})

test_that("HSUM other_drug_rule = 'per_drug' reproduces the loosened gates", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitophsum(file = f, other_drug_rule = "per_drug"))
  r <- read_redcap_csv(f)
  bl <- function(var) r[["Branching Logic (Show field only if...)"]][r[[1]] == var]

  # Per-drug threshold only (PNTS guard retained), no argmax term.
  expect_identical(
    bl("hsum_can_sud01"),
    "[hsum_can_freq] >= 3 and [hsum_can_freq] <> '99'"
  )
  expect_identical(
    bl("hsum_oth_with33"),
    "[hsum_oth_freq] >= 3 and [hsum_oth_freq] <> '99'"
  )
})

test_that("HSUM other_drug_rule rejects unknown values", {
  expect_error(
    generate_redcap_hitophsum(
      file = tempfile(fileext = ".zip"),
      other_drug_rule = "argmax"
    ),
    "'arg' should be one of"
  )
})

test_that("HSUM quantity items get the correct field-type/choice overrides", {
  skip_if_no_zip()
  f <- withr::local_tempfile(fileext = ".zip")
  suppressMessages(generate_redcap_hitophsum(file = f))
  r <- read_redcap_csv(f)
  ft <- function(var) r[["Field Type"]][r[[1]] == var]
  ch <- function(var) r[["Choices, Calculations, OR Slider Labels"]][r[[1]] == var]

  choices_1_to_20 <- paste(
    c(paste(1:19, 1:19, sep = ", "), "20, 20+", "99, Prefer not to say"),
    collapse = " | "
  )
  choices_1_to_60 <- paste(
    c(paste(1:59, 1:59, sep = ", "), "60, 60+", "99, Prefer not to say"),
    collapse = " | "
  )

  # Alcohol quantity -> dropdown 1..20; cigarette AND cigar quantity ->
  # dropdown 1..60 (cigars previously emitted an empty, invalid choice list).
  expect_identical(ft("hsum_alc_quant"), "dropdown")
  expect_identical(ch("hsum_alc_quant"), choices_1_to_20)
  expect_identical(ft("hsum_nic_quant_cig"), "dropdown")
  expect_identical(ch("hsum_nic_quant_cig"), choices_1_to_60)
  expect_identical(ft("hsum_nic_quant_cgr"), "dropdown")
  expect_identical(ch("hsum_nic_quant_cgr"), choices_1_to_60)

  # "Other" free-text quantity -> text with no choices.
  expect_identical(ft("hsum_nic_quant_oth"), "text")
  expect_identical(ch("hsum_nic_quant_oth"), "")

  # No choice-bearing field is left with an empty choices string.
  choice_types <- r[["Field Type"]] %in% c("radio", "dropdown", "checkbox")
  expect_true(all(nzchar(r[["Choices, Calculations, OR Slider Labels"]][choice_types])))
})
