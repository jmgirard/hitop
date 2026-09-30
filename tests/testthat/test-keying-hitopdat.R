# External-source verification of the `hitopdat_scales` keying table.
#
# These tests guard the HiTOP-DAT scale table against the published sources it
# is keyed to, never against the Qualtrics file it is built from. Every expected
# value below is transcribed from the source cited above its block. Provenance
# and the open questions are in cairn/SOURCES.md, "HiTOP-DAT".
#
# The keys number items within their own measure. The table uses battery
# numbers, so each key number is mapped through `hitopdat_items$MeasureItem`;
# test-data-hitopdat.R pins that column independently.

battery_numbers <- function(measure, own) {
  rows <- hitopdat_items[hitopdat_items$Measure == measure, ]
  rows$Item[match(own, rows$MeasureItem)]
}

scale_row <- function(scale) {
  hitopdat_scales[hitopdat_scales$Scale == scale, ]
}

# ---- Source: HiTOP-DAT manual (Jonas et al., 2021), "Scale definitions",  ----
# ---- pp. 21-26: the 57 definitions in the order the manual prints them.  ----

manual_scales <- c(
  "WHODAS" = "WHODAS",
  "General Depression" = "IDAS-II", "Dysphoria" = "IDAS-II",
  "Lassitude" = "IDAS-II", "Suicidality" = "IDAS-II", "Insomnia" = "IDAS-II",
  "Appetite Loss" = "IDAS-II", "Appetite Gain" = "IDAS-II",
  "Ill Temper" = "IDAS-II", "Panic" = "IDAS-II",
  "Traumatic Intrusions" = "IDAS-II", "Traumatic Avoidance" = "IDAS-II",
  "Claustrophobia" = "IDAS-II", "Social Anxiety" = "IDAS-II",
  "Cleaning" = "IDAS-II", "Ordering" = "IDAS-II", "Checking" = "IDAS-II",
  "Affective Lability" = "CAT-PD", "Anger" = "CAT-PD",
  "Anxiousness" = "CAT-PD", "Depressiveness" = "CAT-PD",
  "Self-Harm" = "CAT-PD", "Mistrust" = "CAT-PD", "Submissiveness" = "CAT-PD",
  "Relationship Insecurity" = "CAT-PD", "Cognitive Problems" = "CAT-PD",
  "Mania" = "IDAS-II", "Euphoria" = "IDAS-II", "Well-Being" = "IDAS-II",
  "Health Anxiety" = "CAT-PD", "Physical Symptoms" = "PHQ-15",
  "Alcohol Use" = "AUDIT", "Drug Use" = "DUDIT",
  "Non-Premeditation" = "CAT-PD", "Non-Perseverance" = "CAT-PD",
  "Risk Taking" = "CAT-PD", "Irresponsibility" = "CAT-PD",
  "Perfectionism" = "CAT-PD", "Workaholism" = "CAT-PD",
  "Rigidity" = "CAT-PD", "Callousness" = "CAT-PD",
  "Manipulativeness" = "CAT-PD", "Grandiosity" = "CAT-PD",
  "Domineering" = "CAT-PD", "Norm Violation" = "CAT-PD",
  "Hostile Aggression" = "CAT-PD", "Rudeness" = "CAT-PD",
  "Positive Symptoms" = "CAPE", "Unusual Beliefs" = "CAT-PD",
  "Unusual Experiences" = "CAT-PD", "Fantasy Proneness" = "CAT-PD",
  "Peculiarity" = "CAT-PD", "Anhedonia" = "CAT-PD",
  "Exhibitionism" = "CAT-PD", "Social Withdrawal" = "CAT-PD",
  "Emotional Detachment" = "CAT-PD", "Romantic Disinterest" = "CAT-PD"
)

test_that("the scale names are the manual's definitions, in both directions", {
  expect_length(manual_scales, 57)
  expect_setequal(hitopdat_scales$Scale, names(manual_scales))
  expect_false(anyDuplicated(hitopdat_scales$Scale) > 0)
})

test_that("each scale belongs to the measure the manual names for it", {
  measures <- stats::setNames(hitopdat_scales$Measure, hitopdat_scales$Scale)
  expect_identical(measures[names(manual_scales)], manual_scales)
})

# The manual's name for a scale, mapped to its name in each key. A name absent
# here is the same in the key. The manual's Non-Premeditation is IPIP's
# Non-Planfulness, and the manual hyphenates IPIP's "Self Harm".
key_names <- c(
  "Non-Premeditation" = "Non-Planfulness",
  "Self-Harm" = "Self Harm"
)
key_name <- function(manual_name) {
  if (manual_name %in% names(key_names)) key_names[[manual_name]] else manual_name
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

# The key's item text, SF 1 to 216 in order.
ipip_catpd_text <- c(
  "Have frequent mood swings.",
  "Feel like people often are out to get something from me.",
  "Love to be the center of attention.",
  "Am easily controlled by others in my life.",
  "Care about others.",
  "Sometimes get lost in my daydreams.",
  "Have urges to cut myself.",
  "Deserve special treatment from others.",
  "Frequently get things mixed up in my head.",
  "Feel my anxiety overwhelms me.",
  "Feel at times that I have left my body and am somehow outside my physical self.",
  "Boss people around.",
  "Expect nothing less than perfection.",
  "Get angry easily.",
  "Am a strange person.",
  "Find nothing excites me.",
  "Am often out for revenge.",
  "Work too much.",
  "Have difficulty expressing my feelings.",
  "Take advantage of others.",
  "Believe I have supernatural powers.",
  "Worry a lot about catching a serious illness.",
  "Have always been a rule-breaker.",
  "Enjoy going to social gatherings.",
  "Insult people.",
  "Do not like reading or hearing opinions that go against my way of thinking.",
  "Love dangerous situations.",
  "Am always worried that my partner is going to leave me.",
  "Quickly lose interest in the tasks I start.",
  "Don't think much about sex.",
  "Neglect my duties.",
  "Tend to feel very hopeless.",
  "Do things without thinking of the consequences.",
  "Lose control over my behavior when I'm emotional.",
  "Feel that others are out to get me.",
  "Like to stand out in a crowd.",
  "Let others take advantage of me.",
  "Am not a caring person.",
  "Sometimes have fantasies that are overwhelming.",
  "Have thoughts of injuring myself.",
  "Should get special privileges.",
  "Often feel like my thoughts make no sense.",
  "Am nervous or tense most of the time.",
  "See strange figures or visions when nothing is really there.",
  "Like having authority over others.",
  "Don't consider a task finished until it's perfect.",
  "Often feel overwhelmed with rage.",
  "Am odd.",
  "Feel that nothing seems to make me feel good.",
  "Am excited to inflict pain on others.",
  "Am a workaholic, with little time for fun or pleasure.",
  "Think it's best to keep my emotions to myself.",
  "Cheat to get ahead.",
  "Can see into the future.",
  "Am prone to complain about my health.",
  "Get in trouble with the law.",
  "Feel comfortable around people.",
  "Ridicule people.",
  "Find it difficult to consider as valid opinions that differ from my own.",
  "Like to do frightening things.",
  "Am usually convinced that my friends and romantic partners will betray me.",
  "Have difficulty keeping my attention on a task.",
  "Have little desire for sex or romance.",
  "Follow through with my plans.",
  "Am sad most of the time.",
  "Act without planning.",
  "Have unpredictable emotions and moods.",
  "Believe that, sooner or later, people always let you down.",
  "Am likely to show off if I get the chance.",
  "Let myself be pushed around.",
  "Am a cold-hearted person.",
  "Sometimes find myself in a trance-like state without trying.",
  "Feel that cutting myself helps me feel better.",
  "Believe that I am better than others.",
  "Often space out and lose track of what's going on.",
  "Panic easily.",
  "Hear voices talking about me when nobody is really there.",
  "Insist that others do things my way.",
  "Am not happy until all the details are taken care of.",
  "Get irritated easily.",
  "Have been told that my behavior often is bizarre.",
  "Am not a joyful person.",
  "Get even with others.",
  "Have noticed that I put my work ahead of too many other things.",
  "Am open about my feelings.",
  "Like to trick people into doing things for me.",
  "Am able to read the minds of others.",
  "Often am concerned about diseases I might have.",
  "Am a law-abiding citizen.",
  "Keep to myself even when I'm around other people.",
  "Say inappropriate things.",
  "Have been told that I am rigid and inflexible.",
  "Get a thrill out of doing things that might kill me.",
  "Get jealous easily.",
  "Am easily distracted.",
  "Could easily live without having sex.",
  "Keep my appointments.",
  "Generally focus on the negative side of things.",
  "Jump into things without thinking.",
  "Overreact to every little thing in life.",
  "Suspect hidden motives in others.",
  "Use my looks to get what I want.",
  "Prefer that others make the major decisions in my life.",
  "Have fixed opinions.",
  "Do not care how my actions affect others.",
  "Feel like my imagination can run wild.",
  "Frequently have thoughts about killing myself.",
  "Don't think I should have to wait in lines like others.",
  "Often have disorganized thoughts.",
  "Feel that my worry and anxiety is out of control.",
  "Have had the feeling that I might not be human.",
  "Make demands on others.",
  "Set high standards for myself and others.",
  "Have a violent temper.",
  "Am considered to be kind of eccentric.",
  "Have trouble getting interested in things.",
  "Hurt people.",
  "Work longer hours than most people.",
  "Am not good at describing the emotions I feel throughout the day.",
  "Deceive people.",
  "Have the power to cast spells on others.",
  "Am afraid that my life will be cut short by illness.",
  "Respect authority.",
  "Rarely enjoy being with people.",
  "Shoot my mouth off.",
  "Am often accused of being narrow-minded.",
  "Would do anything to get an adrenaline rush.",
  "Usually believe that my friends will abandon me.",
  "Quit tasks as soon as I get bored.",
  "Enjoy sexual experiences intensely.",
  "Am a very reliable person.",
  "Dislike myself.",
  "Am a firm believer in thinking things through.",
  "Know how to cope.",
  "Believe that people are basically honest and good.",
  "Enjoy flirting with complete strangers.",
  "Let myself be directed by others.",
  "Can't be bothered with others’ needs.",
  "Am sometimes so preoccupied with my own thoughts I don't realize others are trying to speak to me.",
  "Have written a suicide note.",
  "Feel that others are beneath me.",
  "Am easily disoriented.",
  "Am generally a fearful person.",
  "Have had the feeling that I was someone else.",
  "Have a strong need for power.",
  "Demand perfection in others.",
  "Am not easily annoyed.",
  "Would describe myself as a normal person.",
  "Have a lot of fun.",
  "Will spread false rumors as a way to hurt others.",
  "Work so hard that my relationships have suffered.",
  "Have difficulty showing affection.",
  "Have exploited others for my own gain.",
  "Can control objects with my mind.",
  "Have medical problems that my doctors don't understand.",
  "Have a rebellious side that gets me into trouble.",
  "Do not feel close to people.",
  "Have a mouth that gets me into trouble.",
  "Am convinced that my way is the best way.",
  "Prefer safety over risk.",
  "Am paralyzed by a fear of rejection.",
  "Finish what I start.",
  "See little need for romance in my life.",
  "Avoid responsibilities.",
  "Look at the bright side of life.",
  "Make careful choices.",
  "Can remain cool-headed when stressed out.",
  "Am pretty trusting of others' motives.",
  "Don't enjoy being in the spotlight.",
  "Need others to help run my life",
  "Believe strongly that the world would be a much better place if I had my way.",
  "Am not a sympathetic person.",
  "Sometimes have extremely vivid pictures in my head.",
  "Believe that I am always right.",
  "Easily lose my train of thought.",
  "Am easily startled.",
  "Sometimes think the TV is talking directly to me.",
  "Am known as a controlling person.",
  "Strive in every way possible to be flawless.",
  "Don't let little things anger me.",
  "Am an energetic person.",
  "Am ready to hit someone when I get angry.",
  "Push myself very hard to succeed.",
  "Am able to describe my feelings easily.",
  "Am an honest person.",
  "Use magic to ward off bad thoughts about me.",
  "Worry about my health.",
  "Got in trouble a lot at school.",
  "Find it difficult to approach others.",
  "Have a reputation for asking inappropriate questions.",
  "Am inflexible when I think I'm right.",
  "Am secure in my relationships.",
  "Am quick to quit when the going gets tough.",
  "Love the feeling of being intimately close with someone.",
  "Cannot be counted on to get things done.",
  "Rarely feel depressed.",
  "Prefer to 'live in the moment' rather than plan things out.",
  "Am indifferent to the feelings of others.",
  "Treat people as inferiors.",
  "Have a good memory for things I've done throughout the day.",
  "Rarely worry.",
  "Like to start fights.",
  "Am emotionally reserved.",
  "Can predict the outcome of events.",
  "Think that I am in good medical condition.",
  "Have done many things for which I could have been (or was) arrested.",
  "Find it difficult to compromise in policy debates.",
  "Generally trust my partners to be faithful to me.",
  "Am not a dependable person.",
  "Formulate ideas clearly.",
  "Enjoy a good brawl.",
  "Believe that most questions have one right answer.",
  "I have intentionally done myself physical harm.",
  "I am known for saying offensive things.",
  "I feel as if my body, or a part of it, has disappeared.",
  "I have no will to live."
)

test_that("the IPIP key is transcribed whole", {
  numbers <- unlist(lapply(ipip_catpd_facets, unlist), use.names = FALSE)
  expect_length(ipip_catpd_facets, 33)
  expect_setequal(numbers, 1:216)
  expect_length(numbers, 216)
  expect_length(ipip_catpd_text, 216)
})

test_that("each CAT-PD facet's items and reverse keys equal the IPIP key", {
  facets <- hitopdat_scales$Scale[hitopdat_scales$Measure == "CAT-PD"]
  expect_setequal(vapply(facets, key_name, ""), names(ipip_catpd_facets))
  for (f in facets) {
    key <- ipip_catpd_facets[[key_name(f)]]
    row <- scale_row(f)
    expect_setequal(
      row$itemNumbers[[1]],
      battery_numbers("CAT-PD", c(key$forward, key$reverse))
    )
    expect_setequal(row$reverseNumbers[[1]], battery_numbers("CAT-PD", key$reverse))
    expect_identical(row$nItems, length(c(key$forward, key$reverse)))
  }
})

test_that("the IPIP key's text matches the item text at each CAT-PD number", {
  # The battery gives each key item in the first person and without its final
  # period: "Have frequent mood swings." is "I have frequent mood swings". The
  # four key items that already start with "I " gain no second one.
  bare <- sub("\\.$", "", ipip_catpd_text)
  expected <- ifelse(
    startsWith(bare, "I "),
    bare,
    paste0("I ", tolower(substr(bare, 1, 1)), substring(bare, 2))
  )
  items <- hitopdat_items$Text[battery_numbers("CAT-PD", 1:216)]
  expect_identical(items, expected)
})

# ---- Source: Watson et al. (2012), Table 1, p. 406: items per IDAS-II scale. ----
# ---- Table 1 lists the 18 non-overlapping scales, not General Depression.   ----

watson_counts <- c(
  "Dysphoria" = 10L, "Well-Being" = 8L, "Panic" = 8L, "Cleaning" = 7L,
  "Lassitude" = 6L, "Insomnia" = 6L, "Suicidality" = 6L,
  "Social Anxiety" = 6L, "Ill Temper" = 5L, "Mania" = 5L, "Euphoria" = 5L,
  "Claustrophobia" = 5L, "Ordering" = 5L, "Traumatic Avoidance" = 4L,
  "Traumatic Intrusions" = 4L, "Checking" = 3L, "Appetite Loss" = 3L,
  "Appetite Gain" = 3L
)

test_that("each non-overlapping IDAS-II scale has the item count Table 1 prints", {
  expect_identical(sum(watson_counts), 99L)
  idas <- hitopdat_scales[hitopdat_scales$Measure == "IDAS-II", ]
  expect_setequal(
    setdiff(idas$Scale, "General Depression"),
    vapply(names(watson_counts), key_name, "")
  )
  counts <- stats::setNames(idas$nItems, idas$Scale)
  expect_identical(counts[names(watson_counts)], watson_counts)
  expect_identical(
    lengths(idas$itemNumbers[idas$Scale %in% names(watson_counts)]),
    stats::setNames(idas$nItems, idas$camelCase)[idas$Scale %in% names(watson_counts)]
  )
})

test_that("the 18 non-overlapping IDAS-II scales hold each IDAS-II item once", {
  idas <- hitopdat_scales[hitopdat_scales$Measure == "IDAS-II" &
                            hitopdat_scales$Scale != "General Depression", ]
  members <- unlist(idas$itemNumbers, use.names = FALSE)
  expect_setequal(members, battery_numbers("IDAS-II", 1:99))
  expect_length(members, 99)
})

# ---- The five totals: each holds every item of its measure, none reversed. ----
# ---- Battery ranges from the battery's flow order (test-data-hitopdat.R). ----

test_that("each total holds exactly the battery numbers of its measure", {
  totals <- list(
    "WHODAS" = 1:12,
    "Alcohol Use" = 112:121,
    "Drug Use" = 122:132,
    "Positive Symptoms" = 133:152,
    "Physical Symptoms" = 369:382
  )
  for (s in names(totals)) {
    row <- scale_row(s)
    expect_identical(row$itemNumbers[[1]], totals[[s]])
    expect_identical(row$reverseNumbers[[1]], integer())
    expect_identical(row$nItems, length(totals[[s]]))
  }
})
