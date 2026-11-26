# HiTOP-DAT keys typed from their published sources, shared by
# test-keying-hitopdat.R (which checks `hitopdat_scales` against them) and
# test-score_hitopdat.R (which recomputes scores from them). Every number below
# is transcribed from the source named above its block, never read from the
# package's tables. Provenance is in cairn/SOURCES.md, "HiTOP-DAT".
#
# The keys number items within their own measure. The package uses battery
# numbers, so each key number is mapped through `hitopdat_items$MeasureItem`;
# test-data-hitopdat.R pins that column independently.

battery_numbers <- function(measure, own) {
  rows <- hitopdat_items[hitopdat_items$Measure == measure, ]
  rows$Item[match(own, rows$MeasureItem)]
}

# ---- Source: IPIP CAT-PD-SF v1.1 key, https://ipip.ori.org/newCAT-PD-SFv1.1Keys.htm ----
# ---- (page last modified 2015-12-18, read 2026-09-29). Numbers are SF #.          ----

ipip_catpd_facets <- list(
  "Affective Lability" = list(forward = c(1L, 34L, 67L, 100L), reverse = c(134L, 167L)),
  "Anger" = list(forward = c(14L, 47L, 80L, 114L), reverse = c(147L, 180L)),
  "Anhedonia" = list(forward = c(16L, 49L, 82L, 116L), reverse = c(149L, 181L)),
  "Anxiousness" = list(forward = c(10L, 43L, 76L, 110L, 143L, 176L), reverse = c(201L)),
  "Callousness" = list(forward = c(38L, 71L, 105L, 138L, 172L, 198L), reverse = c(5L)),
  "Cognitive Problems" = list(forward = c(9L, 42L, 75L, 109L, 142L, 175L), reverse = c(200L, 210L)),
  "Depressiveness" = list(forward = c(32L, 65L, 98L, 132L), reverse = c(165L, 196L)),
  "Domineering" = list(forward = c(12L, 45L, 78L, 112L, 145L, 178L), reverse = integer()),
  "Emotional Detachment" = list(forward = c(19L, 52L, 119L, 152L, 203L), reverse = c(85L, 184L)),
  "Exhibitionism" = list(forward = c(3L, 36L, 69L, 102L, 136L), reverse = c(169L)),
  "Fantasy Proneness" = list(forward = c(6L, 39L, 72L, 106L, 139L, 173L), reverse = integer()),
  "Grandiosity" = list(forward = c(8L, 41L, 74L, 108L, 141L, 174L, 199L), reverse = integer()),
  "Health Anxiety" = list(forward = c(22L, 55L, 88L, 122L, 155L, 187L), reverse = c(205L)),
  "Hostile Aggression" = list(forward = c(17L, 50L, 83L, 117L, 150L, 182L, 202L, 211L), reverse = integer()),
  "Irresponsibility" = list(forward = c(31L, 164L, 195L, 209L), reverse = c(64L, 97L, 131L)),
  "Manipulativeness" = list(forward = c(20L, 53L, 86L, 120L, 153L), reverse = c(185L)),
  "Mistrust" = list(forward = c(2L, 35L, 68L, 101L), reverse = c(135L, 168L)),
  "Non-Perseverance" = list(forward = c(29L, 62L, 95L, 129L, 193L), reverse = c(162L)),
  "Non-Planfulness" = list(forward = c(33L, 66L, 99L, 197L), reverse = c(133L, 166L)),
  "Norm Violation" = list(forward = c(23L, 56L, 156L, 188L, 206L), reverse = c(89L, 123L)),
  "Peculiarity" = list(forward = c(15L, 48L, 81L, 115L), reverse = c(148L)),
  "Perfectionism" = list(forward = c(13L, 46L, 79L, 113L, 146L, 179L), reverse = integer()),
  "Relationship Insecurity" = list(forward = c(28L, 61L, 94L, 128L, 161L), reverse = c(192L, 208L)),
  "Rigidity" = list(forward = c(26L, 59L, 92L, 104L, 126L, 159L, 171L, 191L, 207L, 212L), reverse = integer()),
  "Risk Taking" = list(forward = c(27L, 60L, 93L, 127L), reverse = c(160L)),
  "Romantic Disinterest" = list(forward = c(30L, 63L, 96L, 163L), reverse = c(130L, 194L)),
  "Rudeness" = list(forward = c(25L, 58L, 91L, 125L, 158L, 190L, 214L), reverse = integer()),
  "Self Harm" = list(forward = c(7L, 40L, 73L, 107L, 140L, 213L, 216L), reverse = integer()),
  "Social Withdrawal" = list(forward = c(90L, 124L, 157L, 189L), reverse = c(24L, 57L)),
  "Submissiveness" = list(forward = c(4L, 37L, 70L, 103L, 137L, 170L), reverse = integer()),
  "Unusual Beliefs" = list(forward = c(21L, 54L, 87L, 121L, 154L, 186L, 204L), reverse = integer()),
  "Unusual Experiences" = list(forward = c(11L, 44L, 77L, 111L, 144L, 177L, 215L), reverse = integer()),
  "Workaholism" = list(forward = c(18L, 51L, 84L, 118L, 151L, 183L), reverse = integer())
)

# ---- Source: IDAS-II scoring key (Watson, 2011), "IDAS-II (Items + Scoring).doc",   ----
# ---- sha256 dc77c5fe (cairn/SOURCES.md), under "Composition of the IDAS-II scales". ----
# ---- Starred (reverse-keyed) items are `reverse`. Numbers are the key's 1 to 99.     ----

watson_idas_scales <- list(
  "General Depression" = list(forward = c(1L, 2L, 5L, 6L, 8L, 9L, 11L, 13L, 21L, 26L, 30L, 31L, 40L, 48L, 51L, 52L, 57L, 61L), reverse = c(27L, 64L)),
  "Dysphoria" = list(forward = c(2L, 5L, 8L, 9L, 21L, 31L, 40L, 48L, 57L, 61L), reverse = integer()),
  "Lassitude" = list(forward = c(6L, 29L, 30L, 43L, 54L, 55L), reverse = integer()),
  "Insomnia" = list(forward = c(4L, 11L, 17L, 25L, 36L, 51L), reverse = integer()),
  "Suicidality" = list(forward = c(13L, 22L, 33L, 38L, 46L, 52L), reverse = integer()),
  "Appetite Loss" = list(forward = c(1L, 26L, 60L), reverse = integer()),
  "Appetite Gain" = list(forward = c(19L, 24L, 63L), reverse = integer()),
  "Well-Being" = list(forward = c(3L, 10L, 23L, 27L, 50L, 53L, 59L, 64L), reverse = integer()),
  "Ill Temper" = list(forward = c(12L, 35L, 37L, 44L, 62L), reverse = integer()),
  "Mania" = list(forward = c(67L, 71L, 77L, 83L, 87L), reverse = integer()),
  "Euphoria" = list(forward = c(72L, 78L, 88L, 92L, 97L), reverse = integer()),
  "Panic" = list(forward = c(7L, 16L, 32L, 39L, 45L, 49L, 56L, 58L), reverse = integer()),
  "Social Anxiety" = list(forward = c(15L, 18L, 20L, 41L, 47L, 99L), reverse = integer()),
  "Claustrophobia" = list(forward = c(74L, 80L, 84L, 90L, 94L), reverse = integer()),
  "Traumatic Intrusions" = list(forward = c(14L, 28L, 34L, 42L), reverse = integer()),
  "Traumatic Avoidance" = list(forward = c(73L, 79L, 89L, 93L), reverse = integer()),
  "Checking" = list(forward = c(68L, 75L, 81L), reverse = integer()),
  "Ordering" = list(forward = c(65L, 69L, 82L, 85L, 95L), reverse = integer()),
  "Cleaning" = list(forward = c(66L, 70L, 76L, 86L, 91L, 96L, 98L), reverse = integer())
)
