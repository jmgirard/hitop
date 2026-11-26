# HiTOP-DAT norms typed from the committed source notes, never read from
# `hitopdat_norms`: cairn/references/andrews2009.md (Table 1, total),
# watson2012.md (Table 2, "Adults"), galletly2016.md (Table 1, CAPE positive)
# and simms2026catpdworkbook.md ("CAT-PD Scoring" E3:F35). Rows run in
# `hitopdat_scales` order without General Depression, the AUDIT, the DUDIT
# and the PHQ-15.
typed_dat_norms <- data.frame(
  camelCase = c(
    "whodas",
    "dysphoria", "lassitude", "insomnia", "suicidality", "appetiteLoss",
    "appetiteGain", "wellBeing", "illTemper", "mania", "euphoria", "panic",
    "socialAnxiety", "claustrophobia", "traumaticIntrusions",
    "traumaticAvoidance", "checking", "ordering", "cleaning",
    "positiveSymptoms",
    "affectiveLability", "anger", "anhedonia", "anxiousness", "callousness",
    "cognitiveProblems", "depressiveness", "domineering",
    "emotionalDetachment", "exhibitionism", "fantasyProneness", "grandiosity",
    "healthAnxiety", "hostileAggression", "irresponsibility",
    "manipulativeness", "mistrust", "nonPerseverance", "nonPlanfulness",
    "normViolation", "peculiarity", "perfectionism", "relationshipInsecurity",
    "rigidity", "riskTaking", "romanticDisinterest", "rudeness", "selfHarm",
    "socialWithdrawal", "submissiveness", "unusualBeliefs",
    "unusualExperiences", "workaholism"
  ),
  M = c(
    3.1,
    19.37, 12.50, 11.48, 7.12, 4.61, 6.14, 23.35, 7.87, 8.05, 8.26, 10.73,
    10.02, 6.06, 6.05, 6.62, 4.52, 7.98, 9.05,
    (26.26 * 125 + 26.58 * 180) / 305 / 20,
    2.140302, 2.108249, 2.00097, 2.408313, 1.676236, 2.031049, 2.026511,
    2.366881, 2.638858, 2.475595, 2.422023, 1.987954, 2.151155, 1.411761,
    1.761346, 1.570993, 2.241225, 2.360963, 2.248438, 1.753451, 2.034683,
    3.283652, 2.015666, 2.643119, 1.858946, 2.066845, 1.960858, 1.242576,
    2.183413, 2.12, 1.665727, 1.452366, 2.775229
  ),
  SD = c(
    5.3,
    7.57, 5.04, 4.97, 2.37, 2.21, 2.88, 6.71, 3.73, 3.67, 3.14, 3.90,
    4.56, 2.63, 3.01, 3.39, 2.33, 3.54, 3.73,
    3.52 / 20,
    0.8457127, 0.8440041, 0.8156681, 0.8564947, 0.6857011, 0.6946615,
    0.9627519, 0.8826642, 0.8487732, 0.9222846, 0.9844998, 0.8116232,
    0.8604635, 0.5424737, 0.669054, 0.7097781, 0.8245, 0.839808, 0.7819425,
    0.7595038, 0.9407927, 0.8309183, 0.8540606, 0.6727987, 0.8576587,
    0.905436, 0.8197296, 0.6058149, 0.8346834, 0.79341, 0.7388285,
    0.7239242, 0.9211361
  ),
  stringsAsFactors = FALSE
)
