# Hand-computed fixtures for ground-truth oracle tests (milestone M2).
#
# These build tiny synthetic response sets whose scores are worked out by hand
# from the published PID-5 scoring keys. Item -> scale memberships are copied
# from the OFFICIAL KEY (the same numbers verified against source in
# tests/testthat/test-keying.R), NEVER read back from the package. Expected
# values asserted in the test files are derived here in the comments.
#
# Item range is c(0, 3). On the full form, 16 items are reverse-keyed
# (reverse(x) = 3 - x): 7,30,35,58,87,90,96,97,98,131,142,155,164,177,210,215.
# The SF and BF contain NO reverse-keyed items (see test-keying.R).
#
# score_pid5() outputs 25 facets for FULL/SF and 5 domains for BF; it does NOT
# output FULL/SF domains (that feature + its domain->facet oracle is M7). So
# these fixtures cover FULL/SF facets, BF domains, and all validity scales only.

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
  names(df) <- paste0("pid_", seq_len(220))
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
  names(df) <- paste0("pid_", seq_len(100))
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
# from the APA PID-5-BF Domain Scoring table (verified in test-keying.R / M6):
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
  names(df) <- paste0("pid_", seq_len(25))
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
