# External-source verification of the `hitophsum_items`/`hitophsum_choices`
# keying tables.
#
# Like test-keying.R, these tests guard the keying TABLES THEMSELVES against
# the authoritative source. Every expected string below is hand-transcribed
# from the "revised SUD module-August 2024" sheet of the HiTOP Society
# workbook "SUD module final analyses July 2024.xlsx" (sheet row numbers
# cited per block), never from data-raw/hitophsum_items.csv. Deliberate
# divergences from the sheet (typo repairs, label normalizations, REDCap
# adaptations) are itemized in cairn/SOURCES.md ("HiTOP-HSUM" section);
# each is marked "[divergence]" where applied below.
#
# Sheet layout: columns G/H/I hold the alcohol / nicotine / other-drugs
# versions of each prompt or item; other-drug texts pipe the substance name
# into a shared template.

# Substance phrase piped into the other-drug template, keyed by variable
# abbreviation (from sheet rows 11-22, lowercased mid-sentence [divergence]).
hsum_other_substances <- c(
  can = "cannabis",
  coc = "cocaine",
  stm = "prescription stimulants",
  met = "methamphetamine",
  inh = "inhalants",
  sed = "sedatives or sleeping pills",
  hal = "hallucinogens",
  sop = "street opioids",
  pop = "prescription opioids",
  oth = "other specified substances"
)

hsum_text <- function(var) {
  hitophsum_items$Text[hitophsum_items$Variable == var]
}

# ---- Structure: item counts (sheet rows 112-128 and 138-178) ----------------

test_that("HSUM SUD/WITH item counts match the sheet structure", {
  sud <- hitophsum_items[grepl("^Symptom - SUD", hitophsum_items$Tier), ]
  with <- hitophsum_items[grepl("^Symptom - Withdrawal", hitophsum_items$Tier), ]

  # 17 SUD items for alcohol and each other drug; 13 for nicotine (SUD04-07
  # are "not assessed for nicotine" on the sheet).
  sud_counts <- table(sud$Substance)
  expect_equal(unname(sud_counts[["Nicotine"]]), 13L)
  non_nic <- sud_counts[names(sud_counts) != "Nicotine"]
  expect_equal(length(non_nic), 11L) # alcohol + 10 other drugs
  expect_true(all(non_nic == 17L))
  expect_setequal(
    hitophsum_items$Variable[
      hitophsum_items$Substance == "Nicotine" &
        grepl("^Symptom - SUD", hitophsum_items$Tier)
    ],
    sprintf("hsum_nic_sud%02d", c(1:3, 8:17))
  )

  # 33 WITH items for every symptom-bearing substance (12 total).
  with_counts <- table(with$Substance)
  expect_equal(length(with_counts), 12L)
  expect_true(all(with_counts == 33L))
})

# ---- Screening items (sheet rows 9-42) ---------------------------------------

test_that("HSUM screening item text matches the sheet", {
  # Rows 11-21; row 18 "Librium,Rohypnol" spaced [divergence]; trailing
  # whitespace trimmed throughout [divergence].
  expect_identical(hsum_text("hsum_alc"), "Alcohol")
  expect_identical(
    hsum_text("hsum_can"),
    "Cannabis (marijuana, pot, grass, hash, etc.)"
  )
  expect_identical(
    hsum_text("hsum_nic"),
    "Nicotine (cigarettes, vaping/e-cigarettes, nicotine pouches, over-the-counter nicotine gum, patch, lozenge)"
  )
  expect_identical(hsum_text("hsum_coc"), "Cocaine (coke, crack, etc.)")
  expect_identical(
    hsum_text("hsum_stm"),
    "Prescription stimulants (Ritalin, Concerta, Dexedrine, Adderall, diet pills, etc.)"
  )
  expect_identical(
    hsum_text("hsum_met"),
    "Methamphetamine (speed, crystal meth, ice, etc.)"
  )
  expect_identical(
    hsum_text("hsum_inh"),
    "Inhalants (nitrous oxide, glue, gas, paint thinner, etc.)"
  )
  expect_identical(
    hsum_text("hsum_sed"),
    "Sedatives or sleeping pills (Valium, Serepax, Ativan, Xanax, Librium, Rohypnol, GHB, etc.)"
  )
  expect_identical(
    hsum_text("hsum_hal"),
    "Hallucinogens (LSD, acid, mushrooms, PCP, Special K, ecstasy, etc.)"
  )
  expect_identical(
    hsum_text("hsum_sop"),
    "Street opioids (heroin, opium, fentanyl, etc.)"
  )
  expect_identical(
    hsum_text("hsum_pop"),
    "Prescription opioids (oxycodone [OxyContin, Percocet], hydrocodone [Vicodin], methadone, buprenorphine, etc.)"
  )

  # Rows 26 and 36 (nicotine form follow-ups).
  expect_identical(
    hsum_text("hsum_nic_form"),
    "In what forms did you use nicotine over the past 12 months? (Select all that apply)"
  )
  expect_identical(
    hsum_text("hsum_nic_most"),
    "In what form did you MOST OFTEN use nicotine over the past 12 months?"
  )
})

# ---- Consumption items (sheet rows 53-95) ------------------------------------

test_that("HSUM alcohol/nicotine consumption text matches the sheet", {
  # Row 53 alcohol reads "did you use drink alcohol"; sheet typo repaired
  # [divergence].
  expect_identical(
    hsum_text("hsum_alc_freq"),
    "How often did you drink alcohol over the past 12 months?"
  )
  expect_identical(
    hsum_text("hsum_nic_freq"),
    "How often did you use nicotine (in any form) over the past 12 months?"
  )
  # Rows 64, 76-85.
  expect_identical(
    hsum_text("hsum_alc_intox"),
    "Over the past 12 months, how often did you get intoxicated/drunk while drinking alcohol?"
  )
  expect_identical(
    hsum_text("hsum_alc_quant"),
    "On days when you drank alcohol, how many drinks did you typically consume on a single occasion?"
  )
  expect_identical(
    hsum_text("hsum_nic_quant_cig"),
    "On days when you smoked cigarettes, how many did you typically smoke?"
  )
  expect_identical(
    hsum_text("hsum_nic_quant_cgr"),
    "On days when you smoked cigars, cigarillos, or filtered cigars, how many did you typically smoke?"
  )
  expect_identical(
    hsum_text("hsum_nic_quant_oth"),
    "On days when you used nicotine, how much did you typically use?"
  )
  expect_identical(
    hsum_text("hsum_alc_heavy"),
    "Over the past 12 months, how often did you have more than 4 drinks (for women) or 5 drinks (for men) on a single occasion?"
  )
})

# ---- SUD items: alcohol column (sheet rows 112-128, column G) ----------------

test_that("HSUM alcohol SUD item text matches the sheet", {
  alcohol_sud <- c(
    "Reminders of alcohol gave me a strong urge to drink.",
    "I craved a drink of alcohol.",
    "I had strong desires for alcohol.",
    "I drove a car while intoxicated.",
    "When I was under the influence of alcohol, I was in a situation where I could have gotten hurt (e.g., when riding a bicycle, swimming, operating machinery).",
    "I damaged something valuable while under the influence of alcohol.",
    "I did risky or dangerous things while drinking alcohol that I would not have done sober.",
    "I was not able to cut back drinking when I wanted to.",
    "I drank too much.",
    "I tried to drink only at certain times, but it did not work for long.",
    "I got drunk despite previously deciding not to.",
    "I continued to drink even though it made my performance at work suffer.",
    "Drinking or being sick from drinking got in the way of me taking care of myself, my home, or my family.",
    "I missed work, school, or other obligations because I was getting over the effects of drinking (e.g., feeling hungover).",
    "Over time, I needed to drink more alcohol in order to get the same effect.",
    "I consumed a good amount of alcohol before others realized I had been drinking.",
    "I drank a lot more than others before feeling drunk."
  )
  for (i in 1:17) {
    expect_identical(
      hsum_text(sprintf("hsum_alc_sud%02d", i)),
      alcohol_sud[i],
      label = sprintf("hsum_alc_sud%02d", i)
    )
  }
})

# ---- SUD items: nicotine column (sheet rows 112-128, column H) ---------------

test_that("HSUM nicotine SUD item text matches the sheet", {
  nicotine_sud <- c(
    "01" = "Reminders of using nicotine gave me a strong urge to smoke or take nicotine.",
    "02" = "I craved nicotine.",
    "03" = "I had strong desires to use nicotine.",
    "08" = "I was not able to cut back on using nicotine when I wanted to.",
    "09" = "I used nicotine too much.",
    "10" = "I tried to use nicotine only at certain times, but it did not work for long.",
    "11" = "I used nicotine despite previously deciding not to.",
    "12" = "I continued to use nicotine even though it made my performance at work suffer.",
    "13" = "Using nicotine got in the way of me taking care of myself, my home, or my family.",
    "14" = "Using nicotine interfered with my health.",
    "15" = "Over time, I needed to use more nicotine to get the same effect.",
    "16" = "I've noticed that using nicotine does not have the same effect that it used to have.",
    "17" = "My use of nicotine has increased over time."
  )
  for (nn in names(nicotine_sud)) {
    expect_identical(
      hsum_text(paste0("hsum_nic_sud", nn)),
      unname(nicotine_sud[nn]),
      label = paste0("hsum_nic_sud", nn)
    )
  }
})

# ---- SUD items: other-drugs column (sheet rows 112-128, column I) ------------

test_that("HSUM other-drug SUD item text matches the sheet template", {
  # {S} is the sheet's "[pipe substance name here]" placeholder. Two sheet
  # typos repaired [divergence]: SUD01 ends "urge to drink." on the sheet
  # (alcohol carryover); SUD07 spells the placeholder "sunstacne".
  other_sud <- c(
    "Reminders of {S} gave me a strong urge to use {S}.",
    "I craved {S}.",
    "I had strong desires for {S}.",
    "I drove a car while under the influence of {S}.",
    "When I was under the influence of {S}, I was in a situation where I could have gotten hurt (e.g., when riding a bicycle, swimming, operating machinery).",
    "I damaged something valuable while using {S}.",
    "I did risky or dangerous things while using {S} that I would not have done sober.",
    "I was not able to cut back on using {S} when I wanted to.",
    "I used too much {S}.",
    "I tried to use {S} only at certain times, but it did not work for long.",
    "I got high on {S} despite previously deciding not to.",
    "I continued to use {S} even though it made my performance at work suffer.",
    "Using {S} or being sick from using {S} got in the way of me taking care of myself, my home, or my family.",
    "I missed work, school, or other obligations because I was getting over the effects of using {S}.",
    "Over time, I needed to use more {S} to get the same effect.",
    "I used a good amount of {S} before others realized I had been using it.",
    "I used {S} a lot more than others before feeling high."
  )
  for (abbr in names(hsum_other_substances)) {
    for (i in 1:17) {
      expect_identical(
        hsum_text(sprintf("hsum_%s_sud%02d", abbr, i)),
        gsub("{S}", hsum_other_substances[abbr], other_sud[i], fixed = TRUE),
        label = sprintf("hsum_%s_sud%02d", abbr, i)
      )
    }
  }
})

# ---- WITH items (sheet rows 131 and 137-178) ---------------------------------

test_that("HSUM withdrawal item text matches the sheet stem and symptoms", {
  # Stem from row 131; the sheet joins stem and symptom with an ellipsis,
  # rendered here as ": " [divergence].
  with_symptoms <- c(
    "Goose bumps", "Heart racing", "Muscle aches", "Physical shakes",
    "Seizures", "Sweating", "Constipation", "Diarrhea", "Nausea or Vomiting",
    "Coughing", "Dilated pupils", "Dry mouth", "Fever",
    "Headaches or Migraines", "Mouth sores", "Runny nose", "Teary eyes",
    "Yawning", "Anxiety or Nervousness", "Depression or Sadness",
    "Trouble enjoying things", "Anger or Irritability",
    "Trouble concentrating", "Trouble paying attention",
    "Trouble thinking clearly", "Restlessness",
    "Seeing things that weren't really there",
    "Feeling things that weren't really there",
    "Hearing things that weren't really there", "Decreased appetite",
    "Increased appetite", "Sleeping too much", "Insomnia or trouble sleeping"
  )
  substances <- c(
    alc = "alcohol",
    nic = "nicotine",
    hsum_other_substances
  )
  for (abbr in names(substances)) {
    for (i in 1:33) {
      expect_identical(
        hsum_text(sprintf("hsum_%s_with%02d", abbr, i)),
        sprintf(
          "As a result of stopping, quitting, or cutting down on my use of %s, I experienced: %s",
          substances[abbr],
          with_symptoms[i]
        ),
        label = sprintf("hsum_%s_with%02d", abbr, i)
      )
    }
  }
})

# ---- Gates (sheet rows 24, 34, 79, 82, 101-105) -------------------------------

test_that("HSUM gate variables and values match the sheet's looping rules", {
  # Row 79: cigar quantity only for form 3; row 82: free-text nicotine
  # quantity only for the non-cigarette, non-cigar forms (2, 4, 5, 6).
  cgr <- hitophsum_items[hitophsum_items$Variable == "hsum_nic_quant_cgr", ]
  expect_identical(cgr$Field_Type, "dropdown")
  expect_identical(cgr$Choice_Set, "quant_nic_cgr")
  expect_identical(cgr$Gate_Variable, "hsum_nic_form")
  expect_identical(cgr$Gate_Value, "3")
  expect_identical(
    hitophsum_items$Gate_Value[
      hitophsum_items$Variable == "hsum_nic_quant_oth"
    ],
    "2,4,5,6"
  )

  # Rows 102-104: alcohol/other-drug symptom items require at least monthly
  # use (freq >= 3); nicotine requires at least 1-2 days per week (>= 5).
  symptom <- hitophsum_items[grepl("^Symptom", hitophsum_items$Tier), ]
  is_nic <- symptom$Substance == "Nicotine"
  expect_true(all(symptom$Gate_Value[is_nic] == ">= 5"))
  expect_true(all(symptom$Gate_Value[!is_nic] == ">= 3"))
  expect_true(all(symptom$Gate_Variable == sub("_(sud|with)\\d+$", "_freq", symptom$Variable)))
})

# ---- Choice sets (sheet rows 54-62, 65-74, 77-86, 86-95, 108) ----------------

test_that("HSUM choice-set labels match the sheet", {
  cs <- function(set) {
    hitophsum_choices[hitophsum_choices$Choice_Set == set, ]
  }

  # Rows 54-62; "Prefer not to answer" normalized to "Prefer not to say"
  # across all sets [divergence].
  freq <- cs("freq_12m")
  expect_equal(freq$Value, c(1:8, 99))
  expect_identical(
    freq$Label,
    c(
      "1-5 days total", "6-11 days total", "about once a month",
      "2-3 days per month", "1-2 days per week", "3-4 days per week",
      "5-6 days per week", "everyday", "Prefer not to say"
    )
  )

  # Rows 86-95 (alcohol heavy-use column).
  heavy <- cs("freq_heavy")
  expect_equal(heavy$Value, c(0:8, 99))
  expect_identical(
    heavy$Label,
    c(
      "Never drank this many drinks on a single occasion", "1-5 times",
      "6-11 times", "about once a month", "2-3 times per month",
      "1-2 times per week", "3-4 times per week", "5-6 times per week",
      "everyday", "Prefer not to say"
    )
  )

  # Row 108.
  sym <- cs("symptom_4pt")
  expect_equal(sym$Value, 0:3)
  expect_identical(sym$Label, c("not at all", "a little", "moderately", "a lot"))

  # Row 77: alcohol quantity dropdown, 1 to 20+ drinks plus prefer not to say.
  qa <- cs("quant_alcohol")
  expect_equal(qa$Value, c(1:20, 99))
  expect_identical(qa$Label, c(as.character(1:19), "20+", "Prefer not to say"))

  # Rows 77 and 80: cigarette and cigar quantity dropdowns, 1 to 60+.
  for (set in c("quant_nic_cig", "quant_nic_cgr")) {
    q <- cs(set)
    expect_equal(q$Value, c(1:60, 99), label = set)
    expect_identical(
      q$Label,
      c(as.character(1:59), "60+", "Prefer not to say"),
      label = set
    )
  }
})
