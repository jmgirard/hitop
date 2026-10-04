# External-source verification of the `pid_items` keying table.
#
# Unlike test-score_*/test-validity_* (which guard the CODE against the keying
# table), these tests guard the keying TABLE ITSELF against the authoritative
# published sources. Every expected value below is transcribed *from the source
# document* (cited per block), never from data-raw/pid_items.csv. Provenance and
# the two open discrepancies are documented in cairn/SOURCES.md.
#
# Ported from the PID-5-only fork (milestone M001). This repo's `pid_items` uses
# columns FULL / SF / BF where the fork used PID5 / PID5FSF / PID5BF. The BF
# `Domain` structure (5 domains x 5 items) is verified against the APA PID-5-BF
# Domain Scoring table in the BF block below (M006). The FULL/SF/IRF primary-facet
# -> domain map (`pid_domains`, driving score_pid5(version = "FULL"/"SF"/"IRF")
# domain output) is verified against the APA full-form Domain Table (M007) and
# the IRF key's Domain Table (M159).

# ---- Source: APA official PID-5 scoring key (Krueger et al., 2013), page 8 ----

test_that("reverse-keyed items match the APA scoring key (Step 1)", {
  apa_reverse <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  expect_setequal(pid_items$FULL[pid_items$Reverse], apa_reverse)
})

test_that("facet -> full-form item membership matches the APA Facet Table", {
  apa_facets <- list(
    "Anhedonia" = c(1, 23, 26, 30, 124, 155, 157, 189),
    "Anxiousness" = c(79, 93, 95, 96, 109, 110, 130, 141, 174),
    "Attention Seeking" = c(14, 43, 74, 111, 113, 173, 191, 211),
    "Callousness" = c(11, 13, 19, 54, 72, 73, 90, 153, 166, 183, 198, 200, 207, 208),
    "Deceitfulness" = c(41, 53, 56, 76, 126, 134, 142, 206, 214, 218),
    "Depressivity" = c(27, 61, 66, 81, 86, 104, 119, 148, 151, 163, 168, 169, 178, 212),
    "Distractibility" = c(6, 29, 47, 68, 88, 118, 132, 144, 199),
    "Eccentricity" = c(5, 21, 24, 25, 33, 52, 55, 70, 71, 152, 172, 185, 205),
    "Emotional Lability" = c(18, 62, 102, 122, 138, 165, 181),
    "Grandiosity" = c(40, 65, 114, 179, 187, 197),
    "Hostility" = c(28, 32, 38, 85, 92, 116, 158, 170, 188, 216),
    "Impulsivity" = c(4, 16, 17, 22, 58, 204),
    "Intimacy Avoidance" = c(89, 97, 108, 120, 145, 203),
    "Irresponsibility" = c(31, 129, 156, 160, 171, 201, 210),
    "Manipulativeness" = c(107, 125, 162, 180, 219),
    "Perceptual Dysregulation" = c(36, 37, 42, 44, 59, 77, 83, 154, 192, 193, 213, 217),
    "Perseveration" = c(46, 51, 60, 78, 80, 100, 121, 128, 137),
    "Restricted Affectivity" = c(8, 45, 84, 91, 101, 167, 184),
    "Rigid Perfectionism" = c(34, 49, 105, 115, 123, 135, 140, 176, 196, 220),
    "Risk Taking" = c(3, 7, 35, 39, 48, 67, 69, 87, 98, 112, 159, 164, 195, 215),
    "Separation Insecurity" = c(12, 50, 57, 64, 127, 149, 175),
    "Submissiveness" = c(9, 15, 63, 202),
    "Suspiciousness" = c(2, 103, 117, 131, 133, 177, 190),
    "Unusual Beliefs & Experiences" = c(94, 99, 106, 139, 143, 150, 194, 209),
    "Withdrawal" = c(10, 20, 75, 82, 136, 146, 147, 161, 182, 186)
  )
  for (f in names(apa_facets)) {
    expect_setequal(pid_items$FULL[pid_items$Facet == f], apa_facets[[f]])
  }
})

# ---- Source: Keeley et al. (2016), Table 1, page 354 (20 INC pairs) ----

test_that("INC item pairs match Keeley et al. (2016) Table 1", {
  keeley_inc <- list(
    c(79, 174), c(109, 110), c(148, 169), c(102, 122), c(138, 181),
    c(38, 92), c(80, 128), c(105, 123), c(50, 127), c(74, 173),
    c(191, 211), c(153, 166), c(125, 180), c(89, 145), c(132, 144),
    c(21, 55), c(24, 25), c(52, 152), c(70, 71), c(172, 185)
  )
  inc <- pid_items[!is.na(pid_items$INC), c("FULL", "INC")]
  pkg_pairs <- lapply(sort(unique(inc$INC)), function(i) sort(inc$FULL[inc$INC == i]))
  expect_length(pkg_pairs, 20)
  for (p in keeley_inc) {
    expect_true(any(vapply(pkg_pairs, function(q) setequal(q, p), logical(1))),
                info = paste("Keeley INC pair missing:", p[1], "-", p[2]))
  }
})

# ---- Source: Sellbom et al. (2018), Table 2, page 586 (10 ORS items) ----

test_that("ORS items match Sellbom et al. (2018) Table 2", {
  sellbom_ors <- c(2, 8, 39, 40, 44, 150, 166, 170, 171, 178)
  expect_setequal(pid_items$FULL[!is.na(pid_items$ORS)], sellbom_ors)
})

# ---- Source: Williams et al. (2019), Table 4 note, page 258 (22 PRD items) ----

test_that("PRD items match Williams et al. (2019) Table 4", {
  williams_prd <- c(2, 11, 36, 38, 42, 47, 96, 98, 106, 119, 122, 136,
                    148, 154, 162, 163, 168, 169, 183, 192, 198, 199)
  expect_setequal(pid_items$FULL[!is.na(pid_items$PRD)], williams_prd)
})

# ---- Source: Williams et al. (2019), Table 5 note, page 259 (SD-TD items) ----

test_that("SDTD contains the 16 items enumerated in Williams et al. (2019) Table 5", {
  # Table 5's note lists 16 item numbers. Asserted as a subset because the
  # paper's TEXT (p. 258) says "17 items" were summed, and pid_items includes a
  # 17th (item 38). See cairn/SOURCES.md open question OQ-1.
  williams_sdtd_note <- c(2, 4, 18, 23, 30, 50, 52, 57, 66, 68, 80, 82, 88, 93, 193, 209)
  expect_true(all(williams_sdtd_note %in% pid_items$FULL[!is.na(pid_items$SDTD)]))
})

test_that("OPEN QUESTION OQ-1: pid_items SDTD item 38 is unverified", {
  # pid_items lists 17 SDTD items; Williams Table 5's note enumerates only 16
  # (item 38 absent) while its text says 17. Cannot be resolved from the PDF.
  skip("SDTD item 38 disputed: Williams (2019) Table 5 note omits it; text says 17 items. Needs source adjudication (SOURCES.md OQ-1).")
})

# ---- Source: Maples et al. (2015), Appendix, page 1210 (100-item SF) ----

test_that("SF selection + facet assignments match Maples et al. (2015) Appendix", {
  maples_fsf <- list(
    "Anxiousness" = c(79, 109, 130, 174), "Emotional Lability" = c(122, 138, 165, 181),
    "Hostility" = c(38, 92, 158, 170), "Perseveration" = c(60, 80, 100, 128),
    "Restricted Affectivity" = c(84, 91, 167, 184), "Separation Insecurity" = c(50, 127, 149, 175),
    "Submissiveness" = c(9, 15, 63, 202), "Anhedonia" = c(23, 26, 124, 157),
    "Depressivity" = c(81, 151, 163, 169), "Intimacy Avoidance" = c(89, 120, 145, 203),
    "Suspiciousness" = c(2, 117, 133, 190), "Withdrawal" = c(82, 136, 146, 186),
    "Eccentricity" = c(25, 70, 152, 205), "Perceptual Dysregulation" = c(44, 154, 192, 217),
    "Unusual Beliefs & Experiences" = c(106, 139, 150, 209), "Attention Seeking" = c(74, 173, 191, 211),
    "Callousness" = c(19, 153, 166, 183), "Deceitfulness" = c(53, 134, 206, 218),
    "Grandiosity" = c(40, 114, 187, 197), "Manipulativeness" = c(107, 125, 162, 219),
    "Distractibility" = c(118, 132, 144, 199), "Impulsivity" = c(4, 16, 17, 22),
    "Irresponsibility" = c(129, 156, 160, 171), "Rigid Perfectionism" = c(105, 123, 176, 196),
    "Risk Taking" = c(39, 48, 67, 159)
  )
  # 100 selected full-form items overall
  expect_setequal(pid_items$FULL[!is.na(pid_items$SF)],
                  unlist(maples_fsf, use.names = FALSE))
  # facet-by-facet
  for (f in names(maples_fsf)) {
    got <- pid_items$FULL[!is.na(pid_items$SF) & pid_items$Facet == f]
    expect_setequal(got, maples_fsf[[f]])
  }
})

test_that("SF carries no reverse-keyed items (Maples 'r' marks are factor orientation)", {
  # Maples' Appendix marks 8 items 'r' (Restricted Affectivity 84,91,167,184;
  # Rigid Perfectionism 105,123,176,196), reflecting each facet's negative
  # loading on its higher-order factor -- NOT full-form facet reverse-keying.
  # None are among the APA reverse items, so score_pid5(version="SF") scores
  # them forward.
  maples_r <- c(84, 91, 167, 184, 105, 123, 176, 196)
  apa_reverse <- c(7, 30, 35, 58, 87, 90, 96, 97, 98, 131, 142, 155, 164, 177, 210, 215)
  expect_length(intersect(maples_r, apa_reverse), 0)
  expect_equal(sum(pid_items$Reverse & !is.na(pid_items$SF)), 0)
})

# ---- Source: Lowmaster et al. (2020), pages 744-749 (INC-S: 10 pairs, cut 8) ----

test_that("INC-S encodes 10 pairs, each a Keeley INC pair fully present in the SF", {
  keeley_inc <- list(
    c(79, 174), c(109, 110), c(148, 169), c(102, 122), c(138, 181),
    c(38, 92), c(80, 128), c(105, 123), c(50, 127), c(74, 173),
    c(191, 211), c(153, 166), c(125, 180), c(89, 145), c(132, 144),
    c(21, 55), c(24, 25), c(52, 152), c(70, 71), c(172, 185)
  )
  fsf <- pid_items$FULL[!is.na(pid_items$SF)]
  incs <- pid_items[!is.na(pid_items$INCS), c("FULL", "INCS")]
  pkg_pairs <- lapply(sort(unique(incs$INCS)), function(i) sort(incs$FULL[incs$INCS == i]))

  expect_length(pkg_pairs, 10)                       # Lowmaster: 10 pairs
  for (p in pkg_pairs) {
    expect_true(all(p %in% fsf))                     # both items in the SF
    expect_true(any(vapply(keeley_inc, function(q) setequal(q, p), logical(1))))  # is a Keeley INC pair
  }
})

# ---- Source: Lowmaster et al. (2021) Correction, Table 1 (authoritative INC-S) ----

test_that("INC-S pairs match Lowmaster et al. (2021) Correction Table 1", {
  # The 2021 Correction (JPA 103(4), 571) enumerates the 10 INC-S item pairs
  # explicitly (full 220-item numbering) and clarifies that 11 of Keeley's 20
  # pairs transfer to the SF, with pair 38-92 "inadvertently not included."
  correction_incs <- list(
    c(79, 174), c(138, 181), c(80, 128), c(105, 123), c(50, 127),
    c(74, 173), c(191, 211), c(153, 166), c(89, 145), c(132, 144)
  )
  incs <- pid_items[!is.na(pid_items$INCS), c("FULL", "INCS")]
  pkg_pairs <- lapply(sort(unique(incs$INCS)), function(i) sort(incs$FULL[incs$INCS == i]))
  expect_length(pkg_pairs, 10)
  for (p in correction_incs) {
    expect_true(any(vapply(pkg_pairs, function(q) setequal(q, sort(p)), logical(1))),
                info = paste("Correction INC-S pair missing:", p[1], "-", p[2]))
  }
  # 38-92 must NOT be present (inadvertently omitted from the validated scale)
  expect_false(any(vapply(pkg_pairs, function(q) setequal(q, c(38, 92)), logical(1))))
})

# ---- Source: APA PID-5-BF Adult, "Personality Trait Domain Scoring" table -----
# Krueger, Derringer, Markon, Watson, & Skodol (c) 2013 APA. Item numbers are in
# BF-relative numbering (1:25). Verifies the `Domain` column for the 25 BF items
# -- the structure score_pid5(version = "BF") groups on -- against the published
# table, independent of pid_items.csv. All BF items are forward-scored (the table
# marks no reverse items) and each domain score is an average (raw sum / 5).

test_that("BF domain membership matches the APA PID-5-BF Domain Scoring table", {
  # Transcribed from the APA table. Keys are the pid_items `Domain` labels; the
  # APA prints "Negative Affect" where pid_items stores "Negative affectivity".
  apa_bf_domains <- list(
    "Negative affectivity" = c(8, 9, 10, 11, 15),
    "Detachment"           = c(4, 13, 14, 16, 18),
    "Antagonism"           = c(17, 19, 20, 22, 25),
    "Disinhibition"        = c(1, 2, 3, 5, 6),
    "Psychoticism"         = c(7, 12, 21, 23, 24)
  )
  for (d in names(apa_bf_domains)) {
    got <- pid_items$BF[!is.na(pid_items$BF) & pid_items$Domain == d]
    expect_setequal(got, apa_bf_domains[[d]])
  }
})

test_that("BF is 25 items, 1:25, exactly 5 per domain, none reverse-keyed", {
  bf <- pid_items[!is.na(pid_items$BF), ]
  expect_setequal(bf$BF, 1:25)                       # complete 25-item form
  expect_equal(nrow(bf), 25L)                        # no duplicate BF numbers
  expect_equal(as.integer(table(bf$Domain)), rep(5L, 5))  # 5 domains x 5 items
  expect_false(any(bf$Reverse))                      # APA table marks no reverse items
})

# ---- Source: APA PID-5 scoring key (Krueger et al., 2013), p. 8 Domain Table --
# FULL/SF/IRF domain scores average the 3 facets contributing PRIMARILY to each
# domain (Step 3). That 15-facet primary map is stored in `pid_domains` and drives
# score_pid5(version = "FULL"/"SF"/"IRF") domain output (M007). Verify the map against the
# published Domain Table, independent of how `pid_domains` was built. NOTE: this
# is the 3-primary-facet subset, NOT the broader 21-facet `pid_items$Domain`
# grouping used for the BF.

test_that("pid_domains primary-facet map matches the APA full-form Domain Table", {
  # Transcribed from the APA scoring key Domain Table (facet labels as printed;
  # the APA prints "Negative Affect" where pid_domains stores "Negative affectivity").
  apa_domains <- list(
    "Negative affectivity" = c("Emotional Lability", "Anxiousness", "Separation Insecurity"),
    "Detachment"           = c("Withdrawal", "Anhedonia", "Intimacy Avoidance"),
    "Antagonism"           = c("Manipulativeness", "Deceitfulness", "Grandiosity"),
    "Disinhibition"        = c("Irresponsibility", "Impulsivity", "Distractibility"),
    "Psychoticism"         = c("Unusual Beliefs & Experiences", "Eccentricity", "Perceptual Dysregulation")
  )
  for (d in names(apa_domains)) {
    got <- pid_domains$primaryFacets[[which(pid_domains$Domain == d)]]
    expect_setequal(got, apa_domains[[d]])
  }
})

test_that("pid_domains is 5 domains x 3 primary facets with valid, distinct stems", {
  expect_equal(nrow(pid_domains), 5L)
  expect_true(all(lengths(pid_domains$primaryFacets) == 3L))
  expect_true(all(lengths(pid_domains$facetStems) == 3L))
  # Every facet stem is a real FULL-form facet output stem (guards transcription).
  expect_true(all(unlist(pid_domains$facetStems) %in% pid_scales[["FULL"]]$camelCase))
  # The 5 domain stems match the BF DOMAIN output names (cross-form consistency).
  # BF also carries a `total` row (M026), which is not a domain and is excluded
  # here deliberately: this asserts the five domain names agree across forms.
  bf_domains <- setdiff(pid_scales[["BF"]]$camelCase, "total")
  expect_setequal(pid_domains$camelCase, bf_domains)
  # 15 distinct primary facets: no facet contributes to two domains.
  expect_equal(length(unlist(pid_domains$facetStems)), 15L)
  expect_equal(length(unique(unlist(pid_domains$facetStems))), 15L)
})

# ---- Source: FU Berlin PID5BF+M key sheet (Kerber, 2020), p. 2 ---------------
# "PID5BF+ M Kodierschema" (cairn/references/fuberlin2020pid5bfpm.md). Typed in
# from the sheet's table, not from data-raw/pid_bfpm_key.csv or `pid_items`.
# Each facet lists its BF+M item pair and the PID-5 items in the same order: the
# first BF+M number of a pair is the first PID-5 number. Facets and domains are
# in the sheet's order. Names are the package's printed names (D-088(b)); the
# references page gives the sheet's German name and Bach et al.'s English name
# for each.
bfpm_key_sheet <- list(
  "Negative affectivity" = list(
    "Emotional Lability"            = list(bfpm = c(1, 19),  pid5 = c(62, 122)),
    "Anxiousness"                   = list(bfpm = c(7, 25),  pid5 = c(109, 110)),
    "Separation Insecurity"         = list(bfpm = c(13, 31), pid5 = c(50, 64))
  ),
  "Detachment" = list(
    "Withdrawal"                    = list(bfpm = c(4, 22),  pid5 = c(82, 136)),
    "Anhedonia"                     = list(bfpm = c(10, 28), pid5 = c(23, 189)),
    "Intimacy Avoidance"            = list(bfpm = c(16, 34), pid5 = c(89, 108))
  ),
  "Antagonism" = list(
    "Manipulativeness"              = list(bfpm = c(2, 20),  pid5 = c(162, 219)),
    "Deceitfulness"                 = list(bfpm = c(8, 26),  pid5 = c(126, 218)),
    "Grandiosity"                   = list(bfpm = c(14, 32), pid5 = c(187, 197))
  ),
  "Disinhibition" = list(
    "Irresponsibility"              = list(bfpm = c(3, 21),  pid5 = c(129, 160)),
    "Impulsivity"                   = list(bfpm = c(9, 27),  pid5 = c(4, 17)),
    "Distractibility"               = list(bfpm = c(15, 33), pid5 = c(6, 132))
  ),
  "Anankastia" = list(
    "Perfectionism"                 = list(bfpm = c(6, 18),  pid5 = c(123, 176)),
    "Rigidity"                      = list(bfpm = c(12, 24), pid5 = c(140, 220)),
    "Orderliness"                   = list(bfpm = c(30, 36), pid5 = c(34, 115))
  ),
  "Psychoticism" = list(
    "Unusual Beliefs & Experiences" = list(bfpm = c(5, 23),  pid5 = c(194, 209)),
    "Eccentricity"                  = list(bfpm = c(11, 29), pid5 = c(25, 185)),
    "Perceptual Dysregulation"      = list(bfpm = c(17, 35), pid5 = c(44, 77))
  )
)
bfpm_sheet_facets <- unlist(
  lapply(bfpm_key_sheet, function(d) d),
  recursive = FALSE,
  use.names = FALSE
)
names(bfpm_sheet_facets) <- unlist(lapply(bfpm_key_sheet, names), use.names = FALSE)

test_that("pid_items$BFPM puts each BF+M number on the PID-5 row the key sheet names", {
  bfpm <- unlist(lapply(bfpm_sheet_facets, `[[`, "bfpm"), use.names = FALSE)
  pid5 <- unlist(lapply(bfpm_sheet_facets, `[[`, "pid5"), use.names = FALSE)
  expect_true(is.integer(pid_items$BFPM))
  expect_setequal(pid_items$FULL[!is.na(pid_items$BFPM)], pid5)
  expect_identical(
    pid_items$BFPM[match(pid5, pid_items$FULL)],
    as.integer(bfpm)
  )
  expect_setequal(pid_items$BFPM[!is.na(pid_items$BFPM)], 1:36)
  # The sheet reverses no item.
  expect_false(any(pid_items$Reverse[!is.na(pid_items$BFPM)]))
})

test_that("pid_scales$BFPM holds the key sheet's 18 facets, pairs and order", {
  tbl <- pid_scales[["BFPM"]]
  expect_identical(tbl$Facet, names(bfpm_sheet_facets))
  expect_identical(
    unname(tbl$itemNumbers),
    lapply(unname(bfpm_sheet_facets), function(f) as.integer(f$bfpm))
  )
  expect_identical(tbl$nItems, rep(2L, 18))
  expect_identical(names(tbl$itemNumbers), tbl$camelCase)
  # D-088(b): new stems for the three anankastia facets only.
  expect_identical(
    tbl$camelCase[13:15],
    c("perfectionism", "rigidity", "orderliness")
  )
})

test_that("pid_bfpm_domains holds the key sheet's 6 domains of 3 facets each", {
  expect_identical(pid_bfpm_domains$Domain, names(bfpm_key_sheet))
  expect_identical(
    pid_bfpm_domains$primaryFacets,
    unname(lapply(bfpm_key_sheet, names))
  )
  expect_identical(
    pid_bfpm_domains$camelCase,
    c("negativeAffectivity", "detachment", "antagonism", "disinhibition",
      "anankastia", "psychoticism")
  )
  # Each facet stem names a pid_scales$BFPM row, so score_pid5()'s domain map
  # reads real facet columns, and each facet is in one domain only.
  stems <- unlist(pid_bfpm_domains$facetStems)
  expect_identical(stems, pid_scales[["BFPM"]]$camelCase)
  # The five shared domains keep their pid_domains names and stems.
  shared <- match(pid_domains$Domain, pid_bfpm_domains$Domain)
  expect_false(anyNA(shared))
  expect_identical(pid_bfpm_domains$camelCase[shared], pid_domains$camelCase)
})

# ---- Source: APA PID-5 Informant Form (Markon et al., 2013), p. 8 ----
#
# The typed key tables (`irf_reverse`, `irf_facets`, `irf_domains`) live in
# helper-fixtures.R, typed from the key's Facet Table and Domain Table
# (cairn/references/apa2013pid5irf.md), never from data-raw/pid_irf_items.csv,
# so the scoring and reliability tests share them. All 218 item texts are
# checked against the PDF by data-raw/check_pid_irf_text.R, which needs the
# shelf copy.

test_that("pid_items$IRF numbers the 218 informant items 1 to 218 in APA order", {
  expect_true(is.integer(pid_items$IRF))
  irf_rows <- pid_items[!is.na(pid_items$IRF), ]
  expect_identical(irf_rows$IRF, 1:218)
  # The IRF has no counterpart to self-report items 96 and 177.
  expect_identical(pid_items$FULL[is.na(pid_items$IRF)], c(96L, 177L))
  expect_false(anyNA(irf_rows$TextIRF))
  expect_true(all(is.na(pid_items$TextIRF[is.na(pid_items$IRF)])))
  # Text anchors typed from the form (PDF pp. 2, 4, 6 and 7), normalized as
  # cairn/references/apa2013pid5irf.md states (no stem or leading ellipsis, no
  # final period, ASCII apostrophes).
  irf_text <- function(n) irf_rows$TextIRF[irf_rows$IRF == n]
  expect_identical(
    irf_text(1),
    "doesn't get as much pleasure out of things as others seem to"
  )
  expect_identical(irf_text(98), "sometimes hears things that aren't really there")
  expect_identical(
    irf_text(176),
    "mentions that they will commit suicide sooner or later"
  )
  expect_identical(irf_text(218), "has a strict way of doing things")
})

test_that("IRF reverse-keyed items are the 14 R marks of the key's Facet Table", {
  expect_setequal(pid_items$IRF[pid_items$Reverse & !is.na(pid_items$IRF)], irf_reverse)
  # Step 1's extra two items are not reversed (D-089(c)).
  expect_false(any(pid_items$Reverse[pid_items$IRF %in% c(98, 176)]))
})

test_that("IRF facet -> item membership matches the key's Facet Table", {
  expect_length(irf_facets, 25)
  expect_setequal(unlist(irf_facets), 1:218)
  for (f in names(irf_facets)) {
    expect_setequal(pid_items$IRF[pid_items$Facet == f & !is.na(pid_items$IRF)], irf_facets[[f]])
  }
  tbl <- pid_scales[["IRF"]]
  expect_setequal(tbl$Facet, names(irf_facets))
  for (i in seq_len(nrow(tbl))) {
    expect_setequal(tbl$itemNumbers[[i]], irf_facets[[tbl$Facet[i]]])
  }
  expect_identical(tbl$nItems, lengths(irf_facets[tbl$Facet], use.names = FALSE))
  # Same facets and stems as the full form, so the domain map carries over.
  expect_identical(tbl$camelCase, pid_scales[["FULL"]]$camelCase)
  expect_identical(names(tbl$itemNumbers), tbl$camelCase)
})

test_that("IRF domains match the key's Domain Table through pid_domains", {
  expect_identical(pid_domains$primaryFacets, irf_domains)
  expect_true(all(unlist(pid_domains$facetStems) %in% pid_scales[["IRF"]]$camelCase))
})

test_that("BF+M facets shared with the PID-5 are subsets of the same PID-5 facet", {
  for (f in names(bfpm_sheet_facets)) {
    pid5_facet <- pid_items$Facet[match(bfpm_sheet_facets[[f]]$pid5, pid_items$FULL)]
    expected <- if (f %in% c("Perfectionism", "Rigidity", "Orderliness")) {
      "Rigid Perfectionism"
    } else {
      f
    }
    expect_identical(pid5_facet, rep(expected, 2), info = f)
  }
})

# ---- Source: Niemeyer et al. (2022), Table S3 and the authors' code ----
#
# The typed key tables (`ffbf_reverse`, `ffbf_facets`, `ffbf_domains`) live in
# helper-fixtures.R. All 400 item texts are checked against the PDF by
# data-raw/check_pid_ffbf_text.R, which needs the shelf copy.

test_that("pid_ffbf_items numbers the 100 FFBF items 1 to 100 with four texts each", {
  expect_true(is.integer(pid_ffbf_items$FFBF))
  expect_identical(pid_ffbf_items$FFBF, 1:100)
  for (col in c("Text", "TextIRF", "TextDE", "TextIRFDE")) {
    expect_false(anyNA(pid_ffbf_items[[col]]), info = col)
    expect_true(all(nzchar(pid_ffbf_items[[col]])), info = col)
  }
  # Spot texts as Table S3 prints them, under the text rule of
  # cairn/references/niemeyer2022.md.
  expect_identical(pid_ffbf_items$Text[12], "I usually think before I act")
  expect_identical(pid_ffbf_items$TextIRF[50], "doesn't like spending time with the other inmates")
  expect_identical(
    pid_ffbf_items$TextIRFDE[93],
    "reagiert kaum auf Dinge, bei denen andere meistens Gefühle zeigen"
  )
})

test_that("FFBF reverse-keyed items are Table S3's two (-) items", {
  expect_true(is.logical(pid_ffbf_items$Reverse))
  expect_setequal(pid_ffbf_items$FFBF[pid_ffbf_items$Reverse], ffbf_reverse)
  for (i in seq_len(nrow(pid_scales[["FFBF"]]))) {
    itm <- pid_scales[["FFBF"]]$itemdata[[i]]
    expect_identical(itm$Reverse, itm$FFBF %in% ffbf_reverse, info = pid_scales[["FFBF"]]$Facet[i])
  }
})

test_that("FFBF facet -> item membership matches Table S3's facet headings", {
  expect_length(ffbf_facets, 25)
  expect_setequal(unlist(ffbf_facets), 1:100)
  for (f in names(ffbf_facets)) {
    expect_equal(sort(pid_ffbf_items$FFBF[pid_ffbf_items$Facet == f]), sort(as.integer(ffbf_facets[[f]])), info = f)
  }
  tbl <- pid_scales[["FFBF"]]
  expect_setequal(tbl$Facet, names(ffbf_facets))
  for (i in seq_len(nrow(tbl))) {
    expect_equal(sort(tbl$itemNumbers[[i]]), sort(as.integer(ffbf_facets[[tbl$Facet[i]]])), info = tbl$Facet[i])
  }
  expect_identical(tbl$nItems, rep(4L, 25))
  # Same facets, stems and order as the SF, the form it adapts.
  expect_identical(tbl$camelCase, pid_scales[["SF"]]$camelCase)
  expect_identical(names(tbl$itemNumbers), tbl$camelCase)
})

test_that("pid_ffbf_domains holds the 5 APA domains and the 2 forensic domains", {
  expect_identical(pid_ffbf_domains$Domain, names(ffbf_domains))
  expect_identical(pid_ffbf_domains$primaryFacets, unname(ffbf_domains))
  expect_identical(pid_ffbf_domains[1:5, ], pid_domains)
  expect_identical(
    pid_ffbf_domains$camelCase[6:7],
    c("disinhibitedAggression", "insecurity")
  )
  expect_true(all(unlist(pid_ffbf_domains$facetStems) %in% pid_scales[["FFBF"]]$camelCase))
})
