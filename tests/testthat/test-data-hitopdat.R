# Shape of the HiTOP-DAT item, answer-set and instruction data. The expected
# values are stated here, read from the battery's Qualtrics file (cairn/SOURCES.md,
# "HiTOP-DAT"), and never derived from the tables under test.

# The seven measures in the battery's flow order, with their item counts.
dat_measure_sizes <- c(
  "WHODAS" = 12L, "IDAS-II" = 99L, "AUDIT" = 10L, "DUDIT" = 11L,
  "CAPE" = 20L, "CAT-PD" = 216L, "PHQ-15" = 14L
)

test_that("the items run 1 to 382 through the seven measures in flow order", {
  expect_identical(hitopdat_items$Item, 1:382)
  runs <- rle(hitopdat_items$Measure)
  expect_identical(stats::setNames(runs$lengths, runs$values), dat_measure_sizes)
  expect_true(is.integer(hitopdat_items$MeasureItem))
})

test_that("each measure keeps its own item numbers", {
  own <- list(
    "WHODAS" = 1:12,
    "IDAS-II" = 1:99,
    "AUDIT" = 1:10,
    "DUDIT" = 1:11,
    "CAPE" = c(2L, 5L, 6L, 7L, 10L, 11L, 13L, 15L, 17L, 20L, 22L, 24L, 26L,
               28L, 30L, 31L, 33L, 34L, 41L, 42L),
    "CAT-PD" = 1:216,
    # The file moves item 4 (menstrual problems) out of the battery.
    "PHQ-15" = c(1:3, 5:15)
  )
  for (m in names(own)) {
    expect_identical(
      hitopdat_items$MeasureItem[hitopdat_items$Measure == m], own[[m]],
      info = m
    )
  }
})

test_that("item text carries no markup, entity or number prefix", {
  expect_length(hitopdat_items$Text, 382)
  expect_false(anyNA(hitopdat_items$Text))
  expect_false(any(grepl("<", hitopdat_items$Text)))
  expect_false(any(grepl("&[a-z]+;", hitopdat_items$Text)))
  expect_false(any(grepl("^[0-9]+\\.\\s", hitopdat_items$Text)))
  expect_false(any(grepl("\\s{2,}|\n", hitopdat_items$Text)))
  expect_identical(hitopdat_items$Text, trimws(hitopdat_items$Text))
})

test_that("the text checks can fail: a planted entity and prefix are caught", {
  # The patterns above run over real text, so show each one matching the
  # defect it looks for.
  expect_true(grepl("&[a-z]+;", "Headaches?&nbsp;"))
  expect_true(grepl("^[0-9]+\\.\\s", "81. I felt the urge"))
  expect_true(grepl("<", "<span>Back pain?</span>"))
})

# The answer sets, labels in display order with the value the file grades each.
dat_sets <- list(
  whodas = c("None" = 0L, "Mild" = 1L, "Moderate" = 2L, "Severe" = 3L,
             "Extreme/Cannot do" = 4L),
  idas = c("Not at all" = 1L, "A little bit" = 2L, "Moderately" = 3L,
           "Quite a bit" = 4L, "Extremely" = 5L),
  audit_freq = c("Never" = 0L, "Monthly" = 1L, "2-4 times a month" = 2L,
                 "2-3 times a week" = 3L, "4 or more times a week" = 4L),
  audit_drinks = c("1 or 2" = 0L, "3 or 4" = 1L, "5 or 6" = 2L,
                   "7 to 9" = 3L, "10 or more" = 4L),
  audit_often = c("Never" = 0L, "Less than monthly" = 1L, "Monthly" = 2L,
                  "Weekly" = 3L, "Daily or almost daily" = 4L),
  audit_harm = c("No" = 0L, "Yes, but not in the last year" = 2L,
                 "Yes, during the last year" = 4L),
  dudit_freq = c("Never" = 0L, "Once a month or less often" = 1L,
                 "2 to 4 times a month" = 2L, "2 to 3 times a week" = 3L,
                 "4 times a week or more often" = 4L),
  dudit_times = c("0" = 0L, "1 to 2" = 1L, "3 to 4" = 2L, "5 to 6" = 3L,
                  "7 or more" = 4L),
  dudit_often = c("Never" = 0L, "Less than monthly" = 1L, "Monthly" = 2L,
                  "Weekly" = 3L, "Daily or almost daily" = 4L),
  dudit_harm = c("No" = 0L, "Yes, but not in the last year" = 2L,
                 "Yes, during the last year" = 4L),
  cape = c("Never" = 1L, "Sometimes" = 2L, "Often" = 3L, "Nearly Always" = 4L),
  catpd = c("Very Untrue of Me" = 1L, "Moderately Untrue of Me" = 2L,
            "Neither True nor Untrue of Me" = 3L, "Moderately True of Me" = 4L,
            "Very True of Me" = 5L),
  phq15 = c("Not bothered at all" = 0L, "Bothered a little" = 1L,
            "Bothered a lot" = 2L)
)

test_that("each answer set has the labels and values the file grades", {
  expect_setequal(unique(hitopdat_choices$Choice_Set), names(dat_sets))
  expect_true(is.integer(hitopdat_choices$Value))
  for (s in names(dat_sets)) {
    rows <- hitopdat_choices[hitopdat_choices$Choice_Set == s, ]
    expect_identical(stats::setNames(rows$Value, rows$Label), dat_sets[[s]], info = s)
  }
})

test_that("no answer set holds the file's Skip answer", {
  expect_false(any(grepl("skip", hitopdat_choices$Label, ignore.case = TRUE)))
})

test_that("each item uses the answer set its question shows", {
  sets <- c(
    rep("whodas", 12), rep("idas", 99),
    "audit_freq", "audit_drinks", rep("audit_often", 6), rep("audit_harm", 2),
    rep("dudit_freq", 2), "dudit_times", rep("dudit_often", 6),
    rep("dudit_harm", 2),
    rep("cape", 20), rep("catpd", 216), rep("phq15", 14)
  )
  expect_identical(hitopdat_items$Choice_Set, sets)
})

# ---- The instruction texts (internal data) ----------------------------------

test_that("each measure's instruction text is the file's, without markup", {
  start <- hitopdat_instructions$start
  expect_identical(names(start), names(dat_measure_sizes))
  expect_identical(start[["CAPE"]], NA_character_)
  expected <- c(
    "WHODAS" = "In the last 30 days, how much difficulty did you have in:",
    "IDAS-II" = paste(
      "Below is a list of feelings, sensations, problems, and experiences that",
      "people sometimes have. Read each item to determine how well it describes",
      "your recent feelings and experiences. Then, enter the choice that best",
      "describes how much you have felt or experienced things this way during",
      "THE PAST TWO WEEKS, including today."
    ),
    "AUDIT" = paste(
      "Because alcohol use can affect your health and can interfere with certain",
      "medications and treatments, it is important that we ask some questions",
      "about your use of alcohol. Your answers will remain confidential so please",
      "be honest. Check the box that best describes your answer to each question."
    ),
    "DUDIT" = paste(
      "Here are a few questions about drugs. Drugs include:",
      "Marijuana, hash, hash oil",
      "Methamphetamine, phenmetraline, khat, betel nut, ritaline (methylphenidate)",
      "Crack, freebase, coca leaves",
      "Smoked heroin, heroin, opium",
      paste(
        "Ecstasy, LSD, mescaline, peyote, PCP (phencyclidine), psilocybin,",
        "DMT (dimethyltrypamine)"
      ),
      "Thinner, trichlorethylene, gasoline, gas, solution, glue",
      paste(
        "GHB, anabolic steroids, laughing gas (halothane), amyl nitrate",
        "(poppers), anticholinergics"
      ),
      paste(
        "Pills count as drugs when you take them to feel good or get high. Pills",
        "that are sometimes used as drugs include sleeping pills, sedatives, and",
        "painkillers."
      ),
      paste(
        "Please answer as correctly and honestly as possible by indicating which",
        "answer is right for you."
      ),
      sep = "\n"
    ),
    "CAT-PD" = paste(
      "For the following questions, please describe yourself as you generally",
      "are now, not as you wish to be in the future. Describe yourself as you",
      "honestly see yourself, in relation to other people you know who are the",
      "same sex and roughly the same age as you. So that you can describe",
      "yourself in an honest manner, your responses will be kept in absolute",
      "confidence."
    ),
    "PHQ-15" = paste(
      "During the past 4 weeks, how much have you been bothered by any of the",
      "following problems?"
    )
  )
  expect_identical(start[names(expected)], expected)
  expect_false(any(grepl("<|&[a-z]+;", start[!is.na(start)])))
})
