# score_pid5(version = "FFBF") and the other PID-5 functions on the PID-5
# Forensic Faceted Brief Form (M162). Keying is checked in test-keying.R; the
# typed key tables (`ffbf_facets`, `ffbf_reverse`, `ffbf_domains`) and the
# fixture `fx_pid5ffbf()` are in helper-fixtures.R.

# Output stems: the 25 facets in the SF's order (the 100-item form the FFBF
# adapts), then the 5 APA domains and the 2 forensic domains of Niemeyer et al.
# (2022), typed.
ffbf_facet_order <- pid_scales[["SF"]]$camelCase
ffbf_domain_order <- c(
  "negativeAffectivity", "detachment", "antagonism", "disinhibition",
  "psychoticism", "disinhibitedAggression", "insecurity"
)
# The facet stem of each `ffbf_facets` entry (typed in the same order).
ffbf_facet_stems <- c(
  "anhedonia", "anxiousness", "attentionSeeking", "callousness",
  "deceitfulness", "depressivity", "distractibility", "eccentricity",
  "emotionalLability", "grandiosity", "hostility", "impulsivity",
  "intimacyAvoidance", "irresponsibility", "manipulativeness",
  "perceptualDysregulation", "perseveration", "restrictedAffectivity",
  "rigidPerfectionism", "riskTaking", "separationInsecurity",
  "submissiveness", "suspiciousness", "unusualBeliefsExperiences",
  "withdrawal"
)

# Expected values for fx_pid5ffbf(), worked out by hand. Reverse items 12
# (Impulsivity) and 26 (Anhedonia) score 3 - x. Under missing = "apa" a facet
# with 1 of 4 items missing is round_half_up(partial sum * 4 / 3) / 4, and a
# facet with 2 or more missing is NA; a domain is the mean of its 3 facets and
# NA if any is NA.
#
# R1: each facet's items are 0, 1, 2, 3 -> 6 / 4 = 1.5, except
#   Impulsivity: item 12 is 0 -> 3; 3 + 1 + 2 + 3 = 9 -> 9 / 4 = 2.25
#   Anhedonia: item 26 is 1 -> 2; 0 + 2 + 2 + 3 = 7 -> 7 / 4 = 1.75
# R2: each facet's items are 3, 2, 1, 0 -> 1.5, except
#   Impulsivity: item 12 is 3 -> 0; 0 + 2 + 1 + 0 = 3 -> 0.75
#   Anhedonia: item 26 is 2 -> 1; 3 + 1 + 1 + 0 = 5 -> 1.25
# R3: R1, except
#   Impulsivity: item 12 NA; 1 + 2 + 3 = 6 -> 6 * 4 / 3 = 8 -> 8 / 4 = 2
#   Hostility: item 36 NA; 0 + 2 + 3 = 5 -> 5 * 4 / 3 = 6.67 -> 7 -> 1.75
#     (the authors' code would give 5 / 3 = 1.667)
# R4: R2, except Emotional Lability NA (items 9 and 34 missing)
# R5: every item 0 -> 0, except Impulsivity and Anhedonia: one item 3 -> 0.75
# R6: every item 3 -> 3, except Impulsivity and Anhedonia: one item 0 -> 2.25
ffbf_facet_expected <- function(stem) {
  base <- c(1.5, 1.5, 1.5, 1.5, 0, 3)
  switch(
    stem,
    impulsivity = c(2.25, 0.75, 2, 0.75, 0.75, 2.25),
    anhedonia = c(1.75, 1.25, 1.75, 1.25, 0.75, 2.25),
    hostility = c(1.5, 1.5, 1.75, 1.5, 0, 3),
    emotionalLability = c(1.5, 1.5, 1.5, NA, 0, 3),
    base
  )
}
# Domains, from the facet rows above:
#   Negative affectivity (EL, Anx, SepIns): 1.5, 1.5, 1.5, NA, 0, 3
#   Detachment (Wd, Anh, IA): (3 + Anh) / 3 per row with Wd = IA = 1.5, and
#     (0 + 0.75 + 0) / 3 = 0.25 in R5, (3 + 2.25 + 3) / 3 = 2.75 in R6
#   Antagonism: 1.5, 1.5, 1.5, 1.5, 0, 3
#   Disinhibition (Irr, Imp, Dis): (3 + Imp) / 3; R5 0.25, R6 2.75
#   Psychoticism: 1.5, 1.5, 1.5, 1.5, 0, 3
#   Disinhibited Aggression (EL, Hos, Imp): R1 (1.5 + 1.5 + 2.25) / 3 = 1.75;
#     R2 (1.5 + 1.5 + 0.75) / 3 = 1.25; R3 (1.5 + 1.75 + 2) / 3 = 1.75; R4 NA;
#     R5 0.25; R6 2.75
#   Insecurity (SepIns, Anx, PD): 1.5, 1.5, 1.5, 1.5, 0, 3
ffbf_domain_expected <- list(
  negativeAffectivity = c(1.5, 1.5, 1.5, NA, 0, 3),
  detachment = c(4.75 / 3, 4.25 / 3, 4.75 / 3, 4.25 / 3, 0.25, 2.75),
  antagonism = c(1.5, 1.5, 1.5, 1.5, 0, 3),
  disinhibition = c(1.75, 1.25, 5 / 3, 1.25, 0.25, 2.75),
  psychoticism = c(1.5, 1.5, 1.5, 1.5, 0, 3),
  disinhibitedAggression = c(1.75, 1.25, 1.75, NA, 0.25, 2.75),
  insecurity = c(1.5, 1.5, 1.5, 1.5, 0, 3)
)

test_that("FFBF output is 25 facets then 7 domains, named and ordered", {
  f <- score_pid5(fx_pid5ffbf(), items = 1:100, version = "FFBF", append = FALSE)
  expect_identical(names(f), paste0("pid_", c(ffbf_facet_order, ffbf_domain_order)))
  # The facet and APA domain columns are named and ordered as the SF's.
  sf <- score_pid5(sim_pid5sf[1:2, ], items = 1:100, version = "SF", append = FALSE)
  expect_identical(names(f)[1:30], names(sf))
  g <- score_pid5(fx_pid5ffbf(), items = 1:100, version = "FFBF", prefix = "x_", append = FALSE)
  expect_identical(names(g), paste0("x_", c(ffbf_facet_order, ffbf_domain_order)))
})

test_that("FFBF scores match hand-computed values under the APA rule", {
  f <- score_pid5(fx_pid5ffbf(), items = 1:100, version = "FFBF", append = FALSE)
  for (stem in ffbf_facet_order) {
    expect_equal(f[[paste0("pid_", stem)]], ffbf_facet_expected(stem), info = stem)
  }
  for (stem in ffbf_domain_order) {
    expect_equal(f[[paste0("pid_", stem)]], ffbf_domain_expected[[stem]], info = stem)
  }
})

test_that("FFBF scores on a 1 to 4 response range are the 0 to 3 values plus 1", {
  # Reverse items score 5 - x on 1 to 4, and the proration sums shift by a
  # whole number, so every score moves by exactly 1.
  x <- fx_pid5ffbf() + 1L
  f <- score_pid5(x, items = 1:100, version = "FFBF", srange = c(1, 4), append = FALSE)
  for (stem in ffbf_facet_order) {
    expect_equal(f[[paste0("pid_", stem)]], ffbf_facet_expected(stem) + 1, info = stem)
  }
  for (stem in ffbf_domain_order) {
    expect_equal(f[[paste0("pid_", stem)]], ffbf_domain_expected[[stem]] + 1, info = stem)
  }
})

test_that("FFBF independent recomputation from the typed tables, each missing mode", {
  # Random answers with scattered NAs, so a wrong item in any facet list, a
  # wrong reverse flag or a wrong domain triplet moves some score.
  set.seed(162)
  n <- 40
  x <- as.data.frame(matrix(sample(0:3, n * 100, replace = TRUE), n, 100))
  x[matrix(stats::runif(n * 100) < 0.06, n, 100)] <- NA
  rev_x <- x
  rev_x[ffbf_reverse] <- 3 - rev_x[ffbf_reverse]
  apa <- function(m) {
    k <- ncol(m)
    apply(m, 1, function(v) {
      a <- sum(!is.na(v))
      if ((k - a) / k > 0.25) return(NA_real_)
      floor(sum(v, na.rm = TRUE) * k / a + 0.5) / k
    })
  }
  for (mode in c("apa", "available", "complete")) {
    facet <- lapply(ffbf_facets, function(i) {
      m <- as.matrix(rev_x[, i])
      if (mode == "apa") apa(m) else rowMeans(m, na.rm = mode == "available")
    })
    names(facet) <- ffbf_facet_stems
    pkg <- score_pid5(x, items = 1:100, version = "FFBF", missing = mode, append = FALSE)
    for (nm in ffbf_facet_stems) {
      expect_equal(pkg[[paste0("pid_", nm)]], facet[[nm]], info = paste(mode, nm))
    }
    for (d in seq_along(ffbf_domains)) {
      fs <- ffbf_facet_stems[match(ffbf_domains[[d]], names(ffbf_facets))]
      expected <- rowMeans(as.data.frame(facet[fs]), na.rm = mode == "available")
      expect_equal(pkg[[paste0("pid_", ffbf_domain_order[d])]], expected, info = paste(mode, d))
    }
  }
})

test_that("FFBF refuses a data frame with the wrong number of items", {
  expect_error(
    score_pid5(fx_pid5ffbf(), items = 1:99, version = "FFBF"),
    "Expected 100 items but got 99",
    fixed = TRUE
  )
})

test_that("FFBF version is matched case-insensitively, and an abbreviation is refused", {
  x <- fx_pid5ffbf()
  ref <- score_pid5(x, items = 1:100, version = "FFBF", append = FALSE)
  expect_identical(score_pid5(x, items = 1:100, version = "ffbf", append = FALSE), ref)
  expect_error(
    score_pid5(x, items = 1:100, version = "FF"),
    class = "hitop_unknown_version"
  )
})

test_that("version = \"F\" is refused in every PID-5 function (D-094)", {
  x <- fx_pid5ffbf()
  expect_error(score_pid5(x, items = 1:100, version = "F"), class = "hitop_unknown_version")
  expect_error(reliability_pid5(x, items = 1:100, version = "F"), class = "hitop_unknown_version")
  expect_error(rename_pid5_items(x, version = "F"), class = "hitop_unknown_version")
  expect_error(label_pid5(x, version = "F"), class = "hitop_unknown_version")
  # The three functions without FFBF read "F" as "FULL" before M170.
  expect_error(
    validity_pid5(fx_pid5(), items = 1:220, version = "F", append = FALSE),
    class = "hitop_unknown_version"
  )
  scored <- score_pid5(sim_pid5[1:3, ], items = 1:220, version = "FULL")
  sc <- paste0("pid_", pid_domains$camelCase)
  expect_error(
    norm_pid5(scored, scores = sc, version = "F", append = FALSE),
    class = "hitop_unknown_version"
  )
})

test_that("reliability_pid5() returns the 25 FFBF facets, as for the SF", {
  set.seed(1621)
  x <- as.data.frame(matrix(sample(0:3, 60 * 100, replace = TRUE), 60, 100))
  r <- reliability_pid5(x, items = 1:100, version = "FFBF", omega = FALSE)
  expect_identical(r$camelCase, ffbf_facet_order)
  expect_identical(r$nItems, rep(4L, 25))
  # Alpha for Impulsivity, recomputed with item 12 reversed (typed key).
  imp <- as.matrix(x[, ffbf_facets[["Impulsivity"]]])
  imp[, 1] <- 3 - imp[, 1]
  k <- ncol(imp)
  alpha <- k / (k - 1) * (1 - sum(apply(imp, 2, stats::var)) / stats::var(rowSums(imp)))
  expect_equal(r$alpha[r$camelCase == "impulsivity"], alpha)
})

test_that("rename_pid5_items() names FFBF columns by number and by any of the four texts", {
  df <- data.frame(pid_1 = 0, pid_100 = 1, age = 30)
  out <- suppressWarnings(rename_pid5_items(df, version = "FFBF"))
  expect_identical(names(out), c("pid5ffbf_001", "pid5ffbf_100", "age"))
  # Texts typed from Table S3 under the rule of cairn/references/niemeyer2022.md:
  # item 12 English self, item 26 English informant, item 51 German self,
  # item 100 German informant.
  df2 <- data.frame(a = 1, b = 2, c = 3, d = 4)
  out2 <- suppressWarnings(rename_pid5_items(
    df2, version = "FFBF", method = "text", item_cols = c("a", "b", "c", "d"),
    item_text = c(
      "I usually think before I act",
      "enjoys life to the extent it is possible to do so in prison",
      "Ich habe fast nie Freude an dem, was ich hier im Alltag so tue",
      "vermeidet möglichst jede Art von Gruppenaktivität"
    )
  ))
  expect_identical(names(out2), c("pid5ffbf_012", "pid5ffbf_026", "pid5ffbf_051", "pid5ffbf_100"))
  # The four texts of the form name 400 distinct strings, so the text method
  # never maps one string to two items.
  pool <- c(pid_ffbf_items$Text, pid_ffbf_items$TextIRF, pid_ffbf_items$TextDE, pid_ffbf_items$TextIRFDE)
  expect_false(anyDuplicated(pool) > 0)
})

test_that("label_pid5() labels the 100 FFBF items and the 32 FFBF scores", {
  items <- label_pid5(fx_pid5ffbf(), target = "items", version = "FFBF")
  labs <- vapply(items, function(v) attr(v, "label"), character(1))
  expect_identical(unname(labs), pid_ffbf_items$Text)
  expect_identical(attr(items$pid5ffbf_012, "label"), "I usually think before I act")
  scored <- score_pid5(fx_pid5ffbf(), items = 1:100, version = "FFBF", append = FALSE)
  scales <- label_pid5(scored, target = "scales", version = "FFBF")
  slabs <- vapply(scales, function(v) attr(v, "label"), character(1))
  expect_length(slabs, 32)
  expect_identical(unname(slabs[26:32]), c(
    "Negative affectivity", "Detachment", "Antagonism", "Disinhibition",
    "Psychoticism", "Disinhibited Aggression", "Insecurity"
  ))
  expect_identical(unname(slabs[1:25]), pid_scales[["SF"]]$Facet)
})

test_that("FFBF standard errors follow the facet and domain rules", {
  d <- hush_se(score_pid5(fx_pid5ffbf(), items = 1:100, version = "FFBF",
                          calc_se = TRUE, append = FALSE))
  # R1 Anhedonia, items 1, 26, 51, 76 after reversing item 26: 0, 2, 2, 3.
  expect_equal(d$pid_anhedonia_se[1], stats::sd(c(0, 2, 2, 3)) / sqrt(4))
  # R1 Disinhibited Aggression: facets 1.5, 1.5, 2.25 (arithmetic above).
  expect_equal(d$pid_disinhibitedAggression_se[1], stats::sd(c(1.5, 1.5, 2.25)) / sqrt(3))
  # R4: Emotional Lability is NA, so are its standard error and the standard
  # errors of both domains it enters.
  expect_true(is.na(d$pid_emotionalLability_se[4]))
  expect_true(is.na(d$pid_negativeAffectivity_se[4]))
  expect_true(is.na(d$pid_disinhibitedAggression_se[4]))
  expect_false(is.na(d$pid_insecurity_se[4]))
})

test_that("validity_pid5(), norm_pid5() and plot_pid5() refuse version = 'FFBF'", {
  x <- fx_pid5ffbf()
  refusals <- list(
    validity_pid5 = function() validity_pid5(x, items = 1:100, version = "FFBF"),
    norm_pid5 = function() norm_pid5(x, version = "FFBF")
  )
  # plot_pid5() checks for ggplot2 before it reads `version`.
  if (rlang::is_installed("ggplot2", version = "3.4.0")) {
    refusals$plot_pid5 <- function() plot_pid5(x, version = "FFBF")
  }
  for (nm in names(refusals)) {
    expect_error(refusals[[nm]](), class = "hitop_unknown_version", info = nm)
  }
})

test_that("reliability_pid5() gives each of the 25 FFBF facets its own alpha", {
  set.seed(1622)
  x <- as.data.frame(matrix(sample(0:3, 80 * 100, replace = TRUE), 80, 100))
  r <- reliability_pid5(x, items = 1:100, version = "FFBF", omega = FALSE)
  for (f in seq_along(ffbf_facets)) {
    m <- as.matrix(x[, ffbf_facets[[f]]])
    rev_cols <- ffbf_facets[[f]] %in% ffbf_reverse
    m[, rev_cols] <- 3 - m[, rev_cols]
    k <- ncol(m)
    alpha <- k / (k - 1) * (1 - sum(apply(m, 2, stats::var)) / stats::var(rowSums(m)))
    expect_equal(r$alpha[r$camelCase == ffbf_facet_stems[f]], alpha, info = names(ffbf_facets)[f])
  }
})

test_that("rename_pid5_items() renames all 100 FFBF numbers and all 400 texts", {
  by_number <- as.data.frame(matrix(0, 1, 100))
  names(by_number) <- paste0("pid_", 1:100)
  expect_identical(
    names(rename_pid5_items(by_number, version = "FFBF")),
    sprintf("pid5ffbf_%03d", 1:100)
  )
  for (col in c("Text", "TextIRF", "TextDE", "TextIRFDE")) {
    by_text <- as.data.frame(matrix(0, 1, 100))
    names(by_text) <- paste0("col_", 1:100)
    out <- rename_pid5_items(
      by_text, version = "FFBF", method = "text",
      item_cols = names(by_text), item_text = pid_ffbf_items[[col]]
    )
    expect_identical(names(out), sprintf("pid5ffbf_%03d", pid_ffbf_items$FFBF), info = col)
  }
})

test_that("rename_pid5_items() refuses two columns that match the same FFBF item", {
  df <- data.frame(a = 1, b = 2)
  # Item 1's English and German self-report texts, typed from Table S3.
  expect_error(
    rename_pid5_items(
      df, version = "FFBF", method = "text", item_cols = c("a", "b"),
      item_text = c(
        "I'm not really interested in anything (e.g. leisure time activities, books, magazines, TV shows, sports)",
        "Ich habe an nichts wirklich Interesse (z.B. Freizeitmaßnahmen, Bücher, Zeitschriften, Serien, Sport)"
      )
    ),
    "Two or more columns match the same PID-5-FFBF item"
  )
})

test_that("label_pid5(version = 'FFBF') reports unpadded and out-of-range item columns", {
  df <- data.frame(pid5ffbf_012 = 1, pid5ffbf_12 = 1, pid5ffbf_101 = 1)
  caught <- collect_warnings(label_pid5(df, target = "items", version = "FFBF"))
  labeled <- caught$value
  expect_identical(attr(labeled$pid5ffbf_012, "label"), "I usually think before I act")
  expect_null(attr(labeled$pid5ffbf_12, "label"))
  expect_null(attr(labeled$pid5ffbf_101, "label"))
  expect_length(caught$warnings, 1L)
  expect_s3_class(caught$warnings[[1]], "hitop_unpadded_items")
  text <- warning_text(caught)
  expect_true(grepl("pid5ffbf_12", text, fixed = TRUE))
  expect_true(grepl("pid5ffbf_101", text, fixed = TRUE))
  expect_true(grepl("PID-5-FFBF", text, fixed = TRUE))
})
