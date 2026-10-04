# Hand-computed fixtures for ground-truth oracle tests (milestone M002).
#
# These build tiny synthetic response sets whose scores are worked out by hand
# from the published PID-5 scoring keys. Item -> scale memberships are copied
# from the OFFICIAL KEY (the same numbers verified against source in
# tests/testthat/test-keying.R), NEVER read back from the package. Expected
# values asserted in the test files are derived here in the comments.
#
# Item range is c(0, 3). On the full form, 16 items are reverse-keyed
# (reverse(x) = 3 - x): 7,30,35,58,87,90,96,97,98,131,142,155,164,177,210,215.
# The SF, BF and BFPM contain NO reverse-keyed items (see test-keying.R).
#
# These fixtures were written for FULL/SF facets, BF domains and all validity
# scales (M002). The FULL/SF domain expectations came later (M007) and live in
# test-score_pid5.R. fx_pid5bfpm() covers the PID5BF+M's facets and domains,
# with its own header below.

# ---- Full PID-5 (220 items) -------------------------------------------------
#
# Rows:
#   R1  all items = 0
#   R2  all items = 1
#   R3  all items = 1, plus validity overrides (INC, ORS)
#   R4  all items = 1, but items 1:22 set NA (missingness)
#
# Facet = mean of its items after reverse-keying (columns are pid_<camelCase>):
#   Anhedonia (pid_anhedonia) = 1,23,26,30,124,155,157,189 (n=8; reverse 30,155)
#     R1: (six 0s + two 3s)/8 = 6/8 = 0.75
#     R2: (six 1s + two 2s)/8 = 10/8 = 1.25
#   Anxiousness (pid_anxiousness) = 79,93,95,96,109,110,130,141,174 (n=9; rev 96)
#     R1: (eight 0s + one 3)/9 = 3/9 = 1/3
#     R2: (eight 1s + one 2)/9 = 10/9
#   Intimacy Avoidance (pid_intimacyAvoidance) = 89,97,108,120,145,203 (n=6; rev 97)
#     R1: (five 0s + one 3)/6 = 3/6 = 0.5
#     R2: (five 1s + one 2)/6 = 7/6
#   Separation Insecurity, Emotional Lability, Withdrawal: no reverse items
#     R1 = 0, R2 = 1
#   R4 drops items 1:22 (NA), scored via rowMeans(na.rm = TRUE):
#     Anhedonia loses item 1 -> (five 1s + two 2s)/7 = 9/7 = 1.285714...
fx_pid5 <- function() {
  df <- as.data.frame(matrix(NA_integer_, nrow = 4, ncol = 220))
  names(df) <- sprintf("pid5_%03d", seq_len(220))
  df[1, ] <- 0L
  df[2, ] <- 1L
  df[3, ] <- 1L
  df[3, c(79, 174)]  <- c(3L, 0L)  # INC pair 1 -> |3-0| = 3
  df[3, c(109, 110)] <- c(3L, 0L)  # INC pair 2 -> |3-0| = 3   => INC = 6
  df[3, c(2, 8, 39)] <- 3L         # 3 ORS items at max        => ORS = 3
  df[4, ] <- 1L
  df[4, 1:22] <- NA_integer_       # PNA = 22/220 = 0.1
  df
}

# Validity expectations for fx_pid5(), from the official key (FULL numbering):
#   INC pairs (20): sum of |item_a - item_b|. R3 perturbs pairs 1 & 2 only.
#   ORS items (10): 2,8,39,40,44,150,166,170,171,178 ; score = count == max(3).
#   PRD items (22): 2,11,36,38,42,47,96,98,106,119,122,136,148,154,162,163,
#                   168,169,183,192,198,199 ; score = raw sum (no reverse-key).
#   SDTD items (17): 2,4,18,23,30,38,50,52,57,66,68,80,82,88,93,193,209 ; raw sum.
#     R1: PNA 0, INC 0, ORS 0, PRD 0,  SDTD 0
#     R2: PNA 0, INC 0, ORS 0, PRD 22 (22x1), SDTD 17 (17x1)
#     R3: PNA 0, INC 6, ORS 3, PRD 24 (22 + item2 bumped 1->3 = +2),
#         SDTD 19 (17 + item2 +2)
#     R4: PNA 0.1; INC/ORS/PRD/SDTD = NA (missing items, no na.rm in validity)

# ---- PID-5-SF (100 items) ---------------------------------------------------
#
# SF-relative numbering (1:100). No reverse-keyed items.
# Rows:
#   R1  all items = 0   -> every facet = 0 (proves no reverse-keying)
#   R2  all items = 2   -> every facet = 2
#   R3  all items = 1, f_anhedo items (9,11,43,65) = 0,1,2,3 -> pid_anhedonia = 1.5
#   R4  all items = 1, INCS + ORSS overrides
#   R5  all items = 1, items 1:10 NA (PNA = 10/100 = 0.1)
#
#   Anhedonia (SF items 9,11,43,65):  R3 = (0+1+2+3)/4 = 6/4 = 1.5
#   Grandiosity (SF items 14,37,85,90): R3 untouched = 1
fx_pid5sf <- function() {
  df <- as.data.frame(matrix(NA_integer_, nrow = 5, ncol = 100))
  names(df) <- sprintf("pid5sf_%03d", seq_len(100))
  df[1, ] <- 0L
  df[2, ] <- 2L
  df[3, ] <- 1L
  df[3, c(9, 11, 43, 65)] <- c(0L, 1L, 2L, 3L)  # pid_anhedonia = 1.5
  df[4, ] <- 1L
  df[4, c(24, 78)] <- c(3L, 0L)   # INCS pair 1 -> |3-0| = 3
  df[4, c(53, 81)] <- c(3L, 1L)   # INCS pair 2 -> |3-1| = 2   => INCS = 5
  df[4, c(1, 13, 14)] <- 3L       # 3 ORSS items at max        => ORSS = 3
  df[5, ] <- 1L
  df[5, 1:10] <- NA_integer_      # PNA = 10/100 = 0.1
  df
}

# Validity expectations for fx_pid5sf(), from the official key (SF numbering):
#   INCS pairs (10): (24,78)(53,81)(25,46)(33,42)(17,45)(23,77)(87,97)(62,72)
#                    (29,56)(49,55).
#   ORSS items (8): 1,13,14,15,59,72,75,76 ; score = count == max(3).
#   PRDS items (12): 1,12,34,41,52,63,69,70,74,82,88,91 ; raw sum.
#   SDTDS items (8): 1,2,9,12,17,25,27,96 ; raw sum.
#     R1: all 0.
#     R2: PNA 0, INCS 0, ORSS 0, PRDS 24 (12x2), SDTDS 16 (8x2).
#     R3: PNA 0, INCS 0, ORSS 0, PRDS 12 (12x1, none in {9,11,43,65}),
#         SDTDS 7 (8 items, but item 9 was set 1->0, so 7x1 + 0 = 7).
#     R4: PNA 0, INCS 5, ORSS 3,
#         PRDS 14 (12 + item1 bumped 1->3 = +2),
#         SDTDS 10 (8 + item1 +2).
#     R5: PNA 0.1; ORSS/PRDS/SDTDS = NA (items 1:10 missing; item 1 in
#         ORSS/PRDS/SDTDS). INCS pairs do not use items 1:10, so INCS(R5) = 0.
#   Full asserted vectors (rows R1..R5):
#     PNA   = c(0, 0, 0, 0, 0.1)
#     INCS  = c(0, 0, 0, 5, 0)
#     ORSS  = c(0, 0, 0, 3, NA)
#     PRDS  = c(0, 24, 12, 14, NA)
#     SDTDS = c(0, 16, 7, 10, NA)

# ---- PID-5-BF (25 items) ----------------------------------------------------
#
# BF-relative numbering (1:25). No reverse-keyed items. score_pid5(version="BF")
# returns 5 DOMAIN averages (5 items each). Domain -> BF item map, transcribed
# from the APA PID-5-BF Domain Scoring table (verified in test-keying.R / M006):
#   Disinhibition (pid_disinhibition)             = 1,2,3,5,6
#   Detachment (pid_detachment)                   = 4,13,14,16,18
#   Psychoticism (pid_psychoticism)               = 7,12,21,23,24
#   Negative Affectivity (pid_negativeAffectivity) = 8,9,10,11,15
#   Antagonism (pid_antagonism)                   = 17,19,20,22,25
#
# Rows:
#   R1  all items = 0 -> every domain = 0 (proves no reverse-keying)
#   R2  all items = 2 -> every domain = 2
#   R3  all items = 1, Disinhibition items (1,2,3,5,6) = 0,1,2,3,3 -> 9/5 = 1.8;
#       all other domains untouched = 1
#   R4  all items = 1, items 1:5 NA -> PNA = 5/25 = 0.2; domains scored via
#       rowMeans(na.rm = TRUE) still resolve to 1 (each domain keeps >= 1 item)
fx_pid5bf <- function() {
  df <- as.data.frame(matrix(NA_integer_, nrow = 4, ncol = 25))
  names(df) <- sprintf("pid5bf_%02d", seq_len(25))
  df[1, ] <- 0L
  df[2, ] <- 2L
  df[3, ] <- 1L
  df[3, c(1, 2, 3, 5, 6)] <- c(0L, 1L, 2L, 3L, 3L)  # pid_disinhibition = 9/5 = 1.8
  df[4, ] <- 1L
  df[4, 1:5] <- NA_integer_        # PNA = 5/25 = 0.2
  df
}

# Validity expectations for fx_pid5bf(): only PNA is defined for the BF.
#   R1 0, R2 0, R3 0, R4 5/25 = 0.2

# ---- PID5BF+M (36 items) ----------------------------------------------------
#
# BF+M-relative numbering (1:36). No reverse-keyed items. Facet = mean of its 2
# items; domain = mean of its 3 facet scores (Bach et al., 2020, p. 181; D-088).
# Facet -> BF+M item pairs, transcribed from the FU Berlin key sheet, p. 2
# (verified in test-keying.R), with each pair's (first, second) R3 values:
#   Negative affectivity: EL (1,19)=(0,1)  ANX (7,25)=(1,2)  SEP (13,31)=(3,3)
#   Detachment:           WD (4,22)=(0,0)  ANH (10,28)=(2,1) INT (16,34)=(2,2)
#   Antagonism:           MAN (2,20)=(1,1) DEC (8,26)=(3,2)  GRA (14,32)=(0,1)
#   Disinhibition:        IRR (3,21)=(2,3) IMP (9,27)=(0,2)  DIST (15,33)=(3,0)
#   Anankastia:           PERF (6,18)=(3,1) RIG (12,24)=(1,0) ORD (30,36)=(2,0)
#   Psychoticism:         UB (5,23)=(1,3)  ECC (11,29)=(0,3) PD (17,35)=(3,3)
#
# Rows:
#   R1  all items = 0 -> every facet and domain = 0 (no reverse-keying)
#   R2  all items = 3 -> every facet and domain = 3
#   R3  the pattern above. Facets: EL 0.5, ANX 1.5, SEP 3, WD 0, ANH 1.5,
#       INT 2, MAN 1, DEC 2.5, GRA 0.5, IRR 2.5, IMP 1, DIST 1.5, PERF 2,
#       RIG 0.5, ORD 1, UB 2, ECC 1.5, PD 3. Domains:
#         negativeAffectivity (0.5 + 1.5 + 3)/3 = 5/3
#         detachment          (0 + 1.5 + 2)/3   = 7/6
#         antagonism          (1 + 2.5 + 0.5)/3 = 4/3
#         disinhibition       (2.5 + 1 + 1.5)/3 = 5/3
#         anankastia          (2 + 0.5 + 1)/3   = 7/6
#         psychoticism        (2 + 1.5 + 3)/3   = 13/6
#   R4  R3 with item 1 (EL first, 0) NA.
#       "available": EL = 1 (item 19 alone); negativeAffectivity =
#         (1 + 1.5 + 3)/3 = 11/6. The mean of its 5 answered items would be
#         (1 + 1 + 2 + 3 + 3)/5 = 2, so this row tells the two rules apart.
#       "apa" and "complete": EL is NA (1 of 2 items = 50% missing, over the
#         APA 25%), so negativeAffectivity is NA. Every other scale = R3.
#   R5  R3 with items 12 (RIG first, 1) and 29 (ECC second, 3) NA.
#       "available": RIG = 0 (item 24), anankastia = (2 + 0 + 1)/3 = 1;
#         ECC = 0 (item 11), psychoticism = (2 + 0 + 3)/3 = 5/3 (5-item mean
#         would be (1 + 3 + 0 + 3 + 3)/5 = 2).
#       "apa" and "complete": RIG, ECC, anankastia and psychoticism are NA.
#       Every other scale = R3.
# The PID-5 item number at each BF+M position 1 to 36, typed from the key sheet,
# p. 2 (cairn/references/fuberlin2020pid5bfpm.md), so tests can reach an item's
# text through `pid_items$FULL` rather than through `pid_items$BFPM`.
bfpm_pid5_numbers <- c(
   62, 162, 129,  82, 194, 123, 109, 126,   4,  23,  25, 140,  # BF+M 1-12
   50, 187,   6,  89,  44, 176, 122, 219, 160, 136, 209, 220,  # BF+M 13-24
  110, 218,  17, 189, 185,  34,  64, 197, 132, 108,  77, 115   # BF+M 25-36
)

fx_pid5bfpm <- function() {
  r3 <- c(
    0L, 1L, 2L, 0L, 1L, 3L, 1L, 3L, 0L, 2L, 0L, 1L,   # items 1-12
    3L, 0L, 3L, 2L, 3L, 1L, 1L, 1L, 3L, 0L, 3L, 0L,   # items 13-24
    2L, 2L, 2L, 1L, 3L, 2L, 3L, 1L, 0L, 2L, 3L, 0L    # items 25-36
  )
  df <- as.data.frame(matrix(NA_integer_, nrow = 5, ncol = 36))
  names(df) <- sprintf("pid5bfpm_%02d", seq_len(36))
  df[1, ] <- 0L
  df[2, ] <- 3L
  df[3, ] <- r3
  df[4, ] <- r3
  df[4, 1] <- NA_integer_
  df[5, ] <- r3
  df[5, c(12, 29)] <- NA_integer_
  df
}

# ---- PID-5 Informant Form (218 items) ----------------------------------------
#
# The APA IRF key's tables (M159), typed from its Facet Table and Domain Table
# (cairn/references/apa2013pid5irf.md), never from data-raw/pid_irf_items.csv.
# The reverse list is the Facet Table's R marks, which D-089(c) chose over
# Step 1's list (Step 1 adds 98 and 176). test-keying.R checks the package
# tables against these; the scoring and reliability tests recompute from them.
irf_reverse <- c(7, 30, 35, 58, 87, 90, 96, 97, 130, 141, 154, 163, 208, 213)
irf_facets <- list(
  "Anhedonia" = c(1, 23, 26, 30, 123, 154, 156, 187),
  "Anxiousness" = c(79, 93, 95, 108, 109, 129, 140, 173),
  "Attention Seeking" = c(14, 43, 74, 110, 112, 172, 189, 209),
  "Callousness" = c(11, 13, 19, 54, 72, 73, 90, 152, 165, 181, 196, 198, 205, 206),
  "Deceitfulness" = c(41, 53, 56, 76, 125, 133, 141, 204, 212, 216),
  "Depressivity" = c(27, 61, 66, 81, 86, 103, 118, 147, 150, 162, 167, 168, 176, 210),
  "Distractibility" = c(6, 29, 47, 68, 88, 117, 131, 143, 197),
  "Eccentricity" = c(5, 21, 24, 25, 33, 52, 55, 70, 71, 151, 171, 183, 203),
  "Emotional Lability" = c(18, 62, 101, 121, 137, 164, 179),
  "Grandiosity" = c(40, 65, 113, 177, 185, 195),
  "Hostility" = c(28, 32, 38, 85, 92, 115, 157, 169, 186, 214),
  "Impulsivity" = c(4, 16, 17, 22, 58, 202),
  "Intimacy Avoidance" = c(89, 96, 107, 119, 144, 201),
  "Irresponsibility" = c(31, 128, 155, 159, 170, 199, 208),
  "Manipulativeness" = c(106, 124, 161, 178, 217),
  "Perceptual Dysregulation" = c(36, 37, 42, 44, 59, 77, 83, 153, 190, 191, 211, 215),
  "Perseveration" = c(46, 51, 60, 78, 80, 99, 120, 127, 136),
  "Restricted Affectivity" = c(8, 45, 84, 91, 100, 166, 182),
  "Rigid Perfectionism" = c(34, 49, 104, 114, 122, 134, 139, 175, 194, 218),
  "Risk Taking" = c(3, 7, 35, 39, 48, 67, 69, 87, 97, 111, 158, 163, 193, 213),
  "Separation Insecurity" = c(12, 50, 57, 64, 126, 148, 174),
  "Submissiveness" = c(9, 15, 63, 200),
  "Suspiciousness" = c(2, 102, 116, 130, 132, 188),
  "Unusual Beliefs & Experiences" = c(94, 98, 105, 138, 142, 149, 192, 207),
  "Withdrawal" = c(10, 20, 75, 82, 135, 145, 146, 160, 180, 184)
)
# The key prints "Negative Affect"; the package keeps "Negative affectivity"
# (D-089(d)), so the domains are matched by their primary facets.
irf_domains <- list(
  c("Emotional Lability", "Anxiousness", "Separation Insecurity"),
  c("Withdrawal", "Anhedonia", "Intimacy Avoidance"),
  c("Manipulativeness", "Deceitfulness", "Grandiosity"),
  c("Irresponsibility", "Impulsivity", "Distractibility"),
  c("Unusual Beliefs & Experiences", "Eccentricity", "Perceptual Dysregulation")
)
#
# Hand-computed fixture for score_pid5(version = "IRF") (M159, AC2). Facet
# membership, the 14 R items (D-089(c)) and the domain triplets are typed from
# the APA IRF key (cairn/references/apa2013pid5irf.md); the expected values and
# their arithmetic are in test-score_pid5.R. Item i is IRF item i. Rows:
#   R1  item i answered i %% 4 (answers vary within every facet)
#   R2  item i answered 3 - i %% 4
#   R3  R1 with item 9 NA (Submissiveness, exactly 25% of 4: prorated) and
#       items 1, 23, 26 NA (Anhedonia, 3 of 8 = 37.5%, the fewest past 25%:
#       NA, so Detachment is NA while Withdrawal and Intimacy Avoidance score)
#   R4  R1 with items 11, 13, 19 NA and item 54 = 3 (Callousness prorates
#       12 * 14 / 11 = 15.27 -> 15, where a ceiling gives 16), and
#       Distractibility answered NA, 1, 0, 1, 0, 1, 0, 1, 0 (prorates
#       4 * 9 / 8 = 4.5 -> 5, where base round() gives 4; Disinhibition is
#       scored from this prorated primary facet)
#   R5  every item 0
# Items 98 and 176 are answered 2 and 0 in R1, R3 and R4, 1 and 3 in R2, and
# 0 and 0 in R5, so Step 1's 16-item reverse list would change Unusual Beliefs
# & Experiences and Depressivity in every row.
fx_pid5irf <- function() {
  i <- seq_len(218)
  df <- as.data.frame(matrix(NA_integer_, nrow = 5, ncol = 218))
  names(df) <- sprintf("pid5irf_%03d", i)
  df[1, ] <- i %% 4L
  df[2, ] <- 3L - i %% 4L
  df[3, ] <- i %% 4L
  df[3, c(9, 1, 23, 26)] <- NA_integer_
  df[4, ] <- i %% 4L
  df[4, c(11, 13, 19)] <- NA_integer_
  df[4, 54] <- 3L
  df[4, c(6, 29, 47, 68, 88, 117, 131, 143, 197)] <-
    c(NA, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L)
  df[5, ] <- 0L
  df
}

# ---- HiTOP-SR (405 items) ---------------------------------------------------
#
# Hand-computed fixture for score_hitopsr() (milestone M005). Item range is the
# default c(1, 4); reverse(x) = 1 + 4 - x = 5 - x. Exactly ONE HiTOP-SR item is
# reverse-keyed: HSR 310 (in Romantic Disinterest). Scale -> item memberships
# are copied from the SOURCE (hitopsr_items.csv / hitopsr_scales), NOT read back
# from the package inside the assertions.
#
#   romanticDisinterest = 42,152,187,310,338  (n=5; reverse item = 310)
#   appetiteLoss        = 144,202,389         (n=3; no reverse)
#   bingeEating         = 358,392,398         (n=3; no reverse)
#
# Rows (columns are hsr_001 .. hsr_405, passed to score_hitopsr in order):
#   R1  all items = 1 (scale minimum)
#       romanticDisinterest: 1,1,1,reverse(1)=4,1 -> (1+1+1+4+1)/5 = 8/5  = 1.6
#       appetiteLoss / bingeEating: all 1          -> 1
#   R2  all items = 4 (scale maximum)
#       romanticDisinterest: 4,4,4,reverse(4)=1,4 -> (4+4+4+1+4)/5 = 17/5 = 3.4
#       appetiteLoss / bingeEating: all 4          -> 4
#   R3  all items = 2, romanticDisinterest raw = (42,152,187,310,338) = 1,2,3,4,2
#       romanticDisinterest: 1,2,3,reverse(4)=1,2 -> (1+2+3+1+2)/5 = 9/5  = 1.8
#       appetiteLoss / bingeEating: all 2          -> 2
#   R4  all items = 3, item 144 = NA (missingness; scored with na.rm = TRUE)
#       appetiteLoss: NA,3,3 -> mean(3,3)          = 3
#       romanticDisinterest: 3,3,3,reverse(3)=2,3  -> (3+3+3+2+3)/5 = 14/5 = 2.8
fx_hitopsr <- function() {
  df <- as.data.frame(matrix(NA_integer_, nrow = 4, ncol = 405))
  names(df) <- sprintf("hsr_%03d", seq_len(405))
  df[1, ] <- 1L
  df[2, ] <- 4L
  df[3, ] <- 2L
  df[3, c(42, 152, 187, 310, 338)] <- c(1L, 2L, 3L, 4L, 2L)
  df[4, ] <- 3L
  df[4, 144] <- NA_integer_
  df
}

# Expected score_hitopsr() values (prefix "hsr_"), rows R1..R4:
#   hsr_romanticDisinterest = c(1.6, 3.4, 1.8, 2.8)
#   hsr_appetiteLoss        = c(1,   4,   2,   3)
#   hsr_bingeEating         = c(1,   4,   2,   3)

# ---- HiTOP-BR (45 items) ----------------------------------------------------
#
# Hand-computed fixture for score_hitopbr() (milestone M005). Item range default
# c(1, 4). The HiTOP-BR has NO reverse-keyed items. Scale -> item memberships
# copied from SOURCE (hitopbr_items.csv / hitopbr_scales). The externalizing
# and pFactor scales are OVERLAPPING supersets built from the marker columns
# hitopbr_items$Externalizing / $Pfactor:
#   antagonism      = 1,2,5,13,25,27,33,40,45              (n=9)
#   detachment      = 7,12,30,31,37                        (n=5)
#   disinhibition   = 15,16,20,24,29,32,34,35,43           (n=9)
#   internalizing   = 8,9,18,22,23,36,42,44                (n=8)
#   somatoform      = 6,10,14,17,19,21,26,41               (n=8)
#   thoughtDisorder = 3,4,11,28,38,39                      (n=6)
#   externalizing   = 1,13,15,16,25,32,34,35,40,45         (n=10)
#   pFactor         = 1,6,11,14,22,23,25,28,31,32,35,37    (n=12)
#
# Rows (columns hbr_01 .. hbr_45, passed to score_hitopbr in order):
#   R1  all items = 1 -> every scale = 1
#   R2  all items = 4 -> every scale = 4
#   R3  all items = 2, disinhibition items (15,16,20,24,29,32,34,35,43) = 4
#       disinhibition          -> 4
#       antagonism/detachment/internalizing/somatoform/thoughtDisorder -> 2
#       externalizing: of its 10 items, {15,16,32,34,35} are disinhibition (=4)
#         and {1,13,25,40,45} are not (=2) -> (5*4 + 5*2)/10 = 30/10 = 3.0
#       pFactor: of its 12 items, {32,35} are disinhibition (=4), other 10 = 2
#         -> (2*4 + 10*2)/12 = 28/12 = 7/3 = 2.333333...
#   R4  all items = 3, items 1:5 NA (missingness; na.rm = TRUE)
#       antagonism drops 1,2,5 -> mean of 13,25,27,33,40,45 (all 3) = 3
#       every scale still resolves to 3 (all retained items = 3)
fx_hitopbr <- function() {
  df <- as.data.frame(matrix(NA_integer_, nrow = 4, ncol = 45))
  names(df) <- paste0("HBR_", seq_len(45))
  df[1, ] <- 1L
  df[2, ] <- 4L
  df[3, ] <- 2L
  df[3, c(15, 16, 20, 24, 29, 32, 34, 35, 43)] <- 4L
  df[4, ] <- 3L
  df[4, 1:5] <- NA_integer_
  df
}

# Expected score_hitopbr() values (prefix "hbr_"), rows R1..R4:
#   hbr_disinhibition = c(1, 4, 4,   3)
#   hbr_antagonism    = c(1, 4, 2,   3)
#   hbr_externalizing = c(1, 4, 3.0, 3)   # overlap: 5 disinhibition members
#   hbr_pFactor       = c(1, 4, 7/3, 3)   # overlap: 2 disinhibition members

# Muffle only the `calc_se` deprecation warning, leaving every other condition
# the call signals to reach the test. The argument is deprecated but its
# behavior is still under test all over this suite; without this the warning
# would be reported from all 23 wrapped call sites and bury a warning worth
# reading.
# Targeted by class rather than suppressWarnings() for exactly that reason.
# The warning itself is asserted in test-deprecated-calc_se.R.
hush_se <- function(expr) {
  withCallingHandlers(
    expr,
    hitop_deprecated_calc_se = function(w) invokeRestart("muffleWarning")
  )
}

# Every warning a call signals, in order, alongside the call's value. Needed
# where one call now raises more than one warning and each must be named by
# class: `expect_warning()` sees only the first, and `expect_no_warning(message
# = )` reads `message` as a selector, so it passes on a warning it did not
# select (M032).
collect_warnings <- function(expr) {
  found <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      found[[length(found) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = found)
}

# The plain-text message of the `i`th warning `collect_warnings()` caught, with
# cli's styling removed so `grepl(fixed = TRUE)` can look for a column name.
warning_text <- function(caught, i = 1L) {
  cli::ansi_strip(conditionMessage(caught$warnings[[i]]))
}

# A one-row numeric frame carrying exactly the named columns, for probing which
# column names a label helper recognizes.
frame_of_cols <- function(cols) {
  df <- as.data.frame(matrix(0, nrow = 1, ncol = length(cols)))
  names(df) <- cols
  df
}

# One caught warning with cli's styling and line wrapping flattened, so a whole
# sentence can be looked for as a single string.
squashed_warning <- function(caught, i = 1L) {
  gsub("[[:space:]]+", " ", warning_text(caught, i))
}

# Where `needle` first appears in `haystack`, as a character position, or -1.
# Used to place a reported column name inside one sentence of a two-sentence
# report rather than merely somewhere in the message. Named for that job: a
# helper file every test sees is no place for a name as broad as `at()`.
sentence_pos <- function(haystack, needle) {
  regexpr(needle, haystack, fixed = TRUE)[[1]]
}

# HiTOP-SR subscale item numbers (M141), used by test-score_hitopsr.R and
# test-reliability.R. Transcribed from HiTOP-SR-Final.xlsx, sheet "HiTOP-SR
# items by scale" (cairn/references/sources/). That sheet lists each subscale's
# items by their item-pool IDs (shown after each line), not by HiTOP-SR number,
# so each item was placed at its HiTOP-SR number by its item text. These
# numbers are never read from hitopsr_subscales; test-score_hitopsr.R compares
# the two.
subscale_key <- list(
  affectiveLability    = c(200, 204, 231),           # HiTOP_570-572
  angryHostility       = c(26, 111, 125, 263),       # Ext_89, 193, 323, 514
  anhedonia            = c(64, 300, 380),            # HiTOP_8, 173, 508
  animalInsectPhobia   = c(61, 102, 323, 401, 403),  # HiTOP_181-185
  anxiousWorry         = c(194, 224, 304),           # HiTOP_187, 190, 191
  bloodInjectionPhobia = c(222, 232, 331),           # HiTOP_200-202
  cynicism             = c(8, 181, 261, 352),        # Ext_79, 198, 279, 448
  deceitfulness        = c(7, 75, 226, 377),         # Ext_303, 395, 432, 444
  delusions            = c(91, 116, 130, 165, 223),  # HiTOP_527, 531-534
  depressedMood        = c(11, 100, 170, 365),       # exp8, 9, 12, 14
  hallucinations       = c(3, 51, 221, 259, 280, 385), # HiTOP_594-596, 601, 606, 608
  irritability         = c(106, 135, 214, 270),      # exp4-7
  lassitude            = c(233, 367, 386),           # exp17-19
  manipulativeness     = c(14, 175, 303, 327),       # Ext_22, 38, 101, 262
  shameGuilt           = c(20, 343, 394),            # HiTOP_333, 334, 336
  situationalPhobias   = c(161, 173, 254, 347),      # HiTOP_339-342
  suspiciousness       = c(21, 241, 251, 264)        # Ext_58, HiTOP_56, 661, 662
)

# The parent scale of each subscale, from the same sheet's Scale column.
subscale_parent <- list(
  distressDysphoria   = c("anhedonia", "anxiousWorry", "depressedMood",
                          "lassitude", "shameGuilt"),
  dishonesty          = c("deceitfulness", "manipulativeness"),
  emotionality        = c("affectiveLability", "angryHostility", "irritability"),
  mistrust            = c("cynicism", "suspiciousness"),
  realityDistortion   = c("delusions", "hallucinations"),
  specificPhobiaIndex = c("animalInsectPhobia", "bloodInjectionPhobia",
                          "situationalPhobias")
)
