# score_hitopdat(): raw scale scores, proration and refusals.
#
# Expected scores come from recompute_hitopdat() below, which reads the item
# lists and reverse keys typed from the published keys (helper-hitopdat-keys.R
# and the battery ranges typed here), never `hitopdat_scales`. Scoring rules:
# cairn/references/andrews2009.md, bergman2002.md, berman2005.md, hinz2017.md
# and cairn/SOURCES.md, "HiTOP-DAT".

# The 56 scored scales in `hitopdat_scales` order, General Depression left out.
dat_stems <- c(
  "whodas",
  "dysphoria", "lassitude", "insomnia", "suicidality", "appetiteLoss",
  "appetiteGain", "wellBeing", "illTemper", "mania", "euphoria", "panic",
  "socialAnxiety", "claustrophobia", "traumaticIntrusions",
  "traumaticAvoidance", "checking", "ordering", "cleaning",
  "alcoholUse", "drugUse", "positiveSymptoms",
  "affectiveLability", "anger", "anhedonia", "anxiousness", "callousness",
  "cognitiveProblems", "depressiveness", "domineering", "emotionalDetachment",
  "exhibitionism", "fantasyProneness", "grandiosity", "healthAnxiety",
  "hostileAggression", "irresponsibility", "manipulativeness", "mistrust",
  "nonPerseverance", "nonPlanfulness", "normViolation", "peculiarity",
  "perfectionism", "relationshipInsecurity", "rigidity", "riskTaking",
  "romanticDisinterest", "rudeness", "selfHarm", "socialWithdrawal",
  "submissiveness", "unusualBeliefs", "unusualExperiences", "workaholism",
  "physicalSymptoms"
)

dat_names <- function(prefix = "dat_") {
  c(paste0(prefix, dat_stems), paste0(prefix, dat_stems, "_complete"))
}

# "Well-Being" -> "wellBeing", "Self Harm" -> "selfHarm".
key_stem <- function(name) {
  words <- strsplit(name, "[ -]")[[1]]
  words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
  words[[1]] <- tolower(words[[1]])
  paste(words, collapse = "")
}

# Each scale's battery items, reversed items and rule. CAT-PD items answer 1
# to 5, so a reversed item scores 6 minus the answer.
dat_key <- local({
  key <- list(
    whodas = list(items = 1:12, reverse = integer(), rule = "sum"),
    alcoholUse = list(items = 112:121, reverse = integer(), rule = "sum"),
    drugUse = list(items = 122:132, reverse = integer(), rule = "sum"),
    positiveSymptoms = list(items = 133:152, reverse = integer(), rule = "mean"),
    physicalSymptoms = list(items = 369:382, reverse = integer(), rule = "sum")
  )
  for (s in setdiff(names(watson_idas_scales), "General Depression")) {
    k <- watson_idas_scales[[s]]
    key[[key_stem(s)]] <- list(
      items = battery_numbers("IDAS-II", c(k$forward, k$reverse)),
      reverse = battery_numbers("IDAS-II", k$reverse),
      rule = "sum"
    )
  }
  for (s in names(ipip_catpd_facets)) {
    k <- ipip_catpd_facets[[s]]
    key[[key_stem(s)]] <- list(
      items = battery_numbers("CAT-PD", c(k$forward, k$reverse)),
      reverse = battery_numbers("CAT-PD", k$reverse),
      rule = "mean"
    )
  }
  key[dat_stems]
})

# Expected output for a 382-column answer matrix. A scale scores when its
# answered count a meets a / n >= num / den.
recompute_hitopdat <- function(answers, num = 4L, den = 5L, prefix = "dat_") {
  scores <- list()
  complete <- list()
  for (s in dat_stems) {
    k <- dat_key[[s]]
    x <- answers[, k$items, drop = FALSE]
    rev <- match(k$reverse, k$items)
    x[, rev] <- 6 - x[, rev]
    n <- length(k$items)
    a <- rowSums(!is.na(x))
    total <- rowSums(x, na.rm = TRUE)
    score <- if (k$rule == "sum") total * n / a else total / a
    score[a * den < num * n] <- NA_real_
    scores[[s]] <- score
    complete[[s]] <- a / n
  }
  out <- c(scores, complete)
  names(out) <- dat_names(prefix)
  tibble::as_tibble(out)
}

dat_sets <- lapply(
  hitopdat_items$Choice_Set,
  function(set) hitopdat_choices$Value[hitopdat_choices$Choice_Set == set]
)

dat_frame <- function(answers) {
  df <- as.data.frame(answers)
  names(df) <- paste0("q", 1:382)
  df
}

dat_items <- paste0("q", 1:382)

draw_answers <- function(rows) {
  matrix(
    vapply(dat_sets, function(v) v[sample.int(length(v), rows, replace = TRUE)],
           numeric(rows)),
    nrow = rows
  )
}

test_that("the output names are the 56 scored scales, then their _complete columns", {
  expect_length(dat_stems, 56)
  expect_named(dat_key, dat_stems)
  set.seed(174)
  df <- dat_frame(draw_answers(3))

  out <- score_hitopdat(df, dat_items, append = FALSE)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), dat_names())

  out <- score_hitopdat(df, dat_items, prefix = "x.", append = FALSE)
  expect_identical(names(out), dat_names("x."))

  out <- score_hitopdat(df, dat_items)
  expect_identical(names(out), c(names(df), dat_names()))
  expect_equal(as.data.frame(out[names(df)]), df)
})

test_that("fully answered scores equal the recomputation from the typed keys", {
  set.seed(1743)
  answers <- rbind(
    draw_answers(40),
    vapply(dat_sets, min, numeric(1)),
    vapply(dat_sets, max, numeric(1))
  )
  out <- score_hitopdat(dat_frame(answers), dat_items, append = FALSE)
  expect_equal(out, recompute_hitopdat(answers))

  # Each total's range as its source prints it.
  low <- out[41, ]
  high <- out[42, ]
  expect_identical(c(low$dat_whodas, high$dat_whodas), c(0, 48))
  expect_identical(c(low$dat_alcoholUse, high$dat_alcoholUse), c(0, 40))
  expect_identical(c(low$dat_drugUse, high$dat_drugUse), c(0, 44))
  expect_identical(c(low$dat_physicalSymptoms, high$dat_physicalSymptoms), c(0, 28))
  expect_identical(c(low$dat_dysphoria, high$dat_dysphoria), c(10, 50))
  expect_identical(c(low$dat_positiveSymptoms, high$dat_positiveSymptoms), c(1, 4))
  # Every answer at 1 scores Workaholism (no reversed item) at 1 and Social
  # Withdrawal (two of six reversed) at (4 * 1 + 2 * 5) / 6.
  expect_identical(low$dat_workaholism, 1)
  expect_equal(low$dat_socialWithdrawal, 14 / 6)
})

test_that("an item reversed only in General Depression scores forward in Well-Being", {
  answers <- matrix(dat_sets |> vapply(min, numeric(1)), nrow = 1)
  answers[, battery_numbers("IDAS-II", watson_idas_scales[["Well-Being"]]$forward)] <- 5
  out <- score_hitopdat(dat_frame(answers), dat_items, append = FALSE)
  expect_identical(out$dat_wellBeing, 40)
})

# For each scale, one row at the smallest answered count k that scores and one
# at k - 1, the missing items taken reversed items first.
missing_rows <- function(base, stems, smallest) {
  rows <- list()
  for (s in stems) {
    k <- dat_key[[s]]
    drop_order <- c(k$reverse, setdiff(k$items, k$reverse))
    n <- length(k$items)
    for (drop in c(n - smallest(n), n - smallest(n) + 1L)) {
      row <- base
      row[drop_order[seq_len(drop)]] <- NA
      rows[[length(rows) + 1L]] <- row
    }
  }
  do.call(rbind, rows)
}

check_proration <- function(stems, smallest, num, den, min_complete) {
  set.seed(17)
  base <- draw_answers(1)[1, ]
  answers <- missing_rows(base, stems, smallest)
  out <- score_hitopdat(dat_frame(answers), dat_items,
                        min_complete = min_complete, append = FALSE)
  expect_equal(out, recompute_hitopdat(answers, num, den))
  for (i in seq_along(stems)) {
    s <- stems[[i]]
    n <- length(dat_key[[s]]$items)
    k <- smallest(n)
    score <- out[[paste0("dat_", s)]][c(2 * i - 1, 2 * i)]
    complete <- out[[paste0("dat_", s, "_complete")]][c(2 * i - 1, 2 * i)]
    expect_false(is.na(score[[1]]), info = s)
    expect_true(is.na(score[[2]]), info = s)
    expect_equal(complete, c(k, k - 1) / n, info = s)
  }
}

test_that("each scale prorates at the default 80% and is NA one item below it", {
  check_proration(dat_stems, function(n) (4L * n + 4L) %/% 5L, 4L, 5L, 0.8)
})

test_that("min_complete = 1 scores only fully answered scales", {
  check_proration(dat_stems, function(n) n, 1L, 1L, 1)
})

test_that("min_complete = 0.7 scores Rigidity at 7 of 10 items, not 6", {
  check_proration("rigidity", function(n) 7L, 7L, 10L, 0.7)
})

test_that("a prorated sum is the answered sum times n over the answered count", {
  answers <- matrix(dat_sets |> vapply(max, numeric(1)), nrow = 1)
  answers[, 1:12] <- 3
  answers[, 1:2] <- NA
  answers[, 133:152] <- 2
  answers[, 133:136] <- NA
  out <- score_hitopdat(dat_frame(answers), dat_items, append = FALSE)
  expect_identical(out$dat_whodas, 36)
  expect_identical(out$dat_whodas_complete, 10 / 12)
  expect_identical(out$dat_positiveSymptoms, 2)
})

test_that("NA answers are missing, and zero rows give the named empty tibble", {
  df <- dat_frame(matrix(NA_real_, nrow = 2, ncol = 382))
  out <- score_hitopdat(df, dat_items, append = FALSE)
  expect_true(all(is.na(out[paste0("dat_", dat_stems)])))
  expect_true(all(out[paste0("dat_", dat_stems, "_complete")] == 0))

  set.seed(4)
  df <- dat_frame(draw_answers(2))[0, ]
  out <- score_hitopdat(df, dat_items, append = FALSE)
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), dat_names())
})

test_that("score_hitopdat() refuses bad data, items and arguments", {
  set.seed(5)
  df <- dat_frame(draw_answers(2))

  expect_error(score_hitopdat(as.list(df), dat_items), "must be a data frame")
  expect_error(score_hitopdat(df, dat_items[-1]), "wrong length")
  expect_error(score_hitopdat(df, c(dat_items[-382], "q1")), "distinct column")
  expect_error(score_hitopdat(df, c(dat_items[-382], "q999")), "must all be columns")

  bad <- df
  bad$q200 <- factor(bad$q200)
  expect_error(score_hitopdat(bad, dat_items), class = "hitop_nonnumeric_items")

  for (value in list(0, 1.1, -0.5, c(0.5, 0.6), "0.8", NA_real_, Inf)) {
    expect_error(score_hitopdat(df, dat_items, min_complete = value),
                 "min_complete", info = deparse(value))
  }
  expect_error(score_hitopdat(df, dat_items, prefix = 1), "single string")
  expect_error(score_hitopdat(df, dat_items, prefix = c("a", "b")), "single string")
  expect_error(score_hitopdat(df, dat_items, append = "yes"), "TRUE or FALSE")

  clash <- df
  clash$dat_whodas_complete <- 1
  expect_error(score_hitopdat(clash, dat_items), class = "hitop_append_collision")
  expect_no_error(score_hitopdat(clash, dat_items, append = FALSE))
})

test_that("an answer outside its item's answer set is refused by name", {
  set.seed(6)
  df <- dat_frame(draw_answers(2))
  # 1 lies inside the 0 to 4 range of an AUDIT harm item but not in its set.
  harm <- which(hitopdat_items$Choice_Set == "audit_harm")[[1]]
  cases <- list(
    list(item = 200L, value = 6),
    list(item = harm, value = 1),
    list(item = 13L, value = 2.5),
    list(item = 369L, value = -1)
  )
  for (case in cases) {
    bad <- df
    bad[[case$item]][[2]] <- case$value
    err <- expect_error(score_hitopdat(bad, dat_items), class = "hitop_invalid_answer",
                        info = case$item)
    expect_match(conditionMessage(err), paste0("q", case$item), fixed = TRUE,
                 info = case$item)
  }
  # Each set's smallest and largest value, and NA, are accepted.
  ok <- dat_frame(rbind(vapply(dat_sets, min, numeric(1)),
                        vapply(dat_sets, max, numeric(1)),
                        rep(NA_real_, 382)))
  expect_no_error(score_hitopdat(ok, dat_items))
})
