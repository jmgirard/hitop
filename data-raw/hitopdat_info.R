## HiTOP-DAT Items and Choices
## Source: the HiTOP-DAT Qualtrics file `data-raw/HiTOP-DAT.qsf`, shared with the
## maintainer and not committed (provenance, sha256 and how to obtain it:
## cairn/SOURCES.md, "HiTOP-DAT"). When the file is present, this script reads
## it and rewrites `data-raw/hitopdat_items.csv` and
## `data-raw/hitopdat_choices.csv`. When it is absent, the committed CSVs are
## used as they are, so the datasets still rebuild from a fresh clone.
## Needs {jsonlite} (an Import) to read the file.

qsf_path <- "data-raw/HiTOP-DAT.qsf"

## The seven measure blocks, in the order the file's survey flow shows them.
## Each block's description in the file, the measure name this package uses,
## and the pattern of the block's item tags.
dat_measures <- data.frame(
  Block = c("WHODAS", "IDAS-II", "AUDIT", "DUDIT", "CAPE", "CAT", "PHQ"),
  Measure = c("WHODAS", "IDAS-II", "AUDIT", "DUDIT", "CAPE", "CAT-PD", "PHQ-15"),
  Tag = c("^WHODAS_\\d+$", "^IDAS_\\d+$", "^AUDIT_\\d+$", "^DUDIT_\\d+$",
          "^CAPE\\d+$", "^CATPD_\\d+$", "^PHQ_\\d+$")
)

## The answer sets. Each item's labels (in display order, "Skip" left out) must
## match exactly one set below, or the script stops.
dat_sets <- list(
  whodas = c("None", "Mild", "Moderate", "Severe", "Extreme/Cannot do"),
  idas = c("Not at all", "A little bit", "Moderately", "Quite a bit",
           "Extremely"),
  audit_freq = c("Never", "Monthly", "2-4 times a month", "2-3 times a week",
                 "4 or more times a week"),
  audit_drinks = c("1 or 2", "3 or 4", "5 or 6", "7 to 9", "10 or more"),
  audit_often = c("Never", "Less than monthly", "Monthly", "Weekly",
                  "Daily or almost daily"),
  audit_harm = c("No", "Yes, but not in the last year",
                 "Yes, during the last year"),
  dudit_freq = c("Never", "Once a month or less often", "2 to 4 times a month",
                 "2 to 3 times a week", "4 times a week or more often"),
  dudit_times = c("0", "1 to 2", "3 to 4", "5 to 6", "7 or more"),
  dudit_often = c("Never", "Less than monthly", "Monthly", "Weekly",
                  "Daily or almost daily"),
  dudit_harm = c("No", "Yes, but not in the last year",
                 "Yes, during the last year"),
  cape = NULL,
  catpd = c("Very Untrue of Me", "Moderately Untrue of Me",
            "Neither True nor Untrue of Me", "Moderately True of Me",
            "Very True of Me"),
  phq15 = c("Not bothered at all", "Bothered a little", "Bothered a lot")
)

## Plain text from the file's HTML: tags removed, the two entities the file
## uses decoded, line and list breaks kept as "\n", other runs of spaces made
## one space.
dat_plain <- function(x) {
  x <- gsub("<br\\s*/?>|</div>|</p>|</li>", "\n", x, ignore.case = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("[ \t\r\f\v]+", " ", x)
  x <- gsub(" *\n *", "\n", x)
  x <- gsub("\n+", "\n", x)
  trimws(x)
}

if (file.exists(qsf_path)) {
  qsf <- jsonlite::fromJSON(qsf_path, simplifyVector = FALSE)
  elements <- qsf$SurveyElements
  kinds <- vapply(elements, function(e) e$Element, "")
  questions <- lapply(elements[kinds == "SQ"], `[[`, "Payload")
  names(questions) <- vapply(questions, `[[`, "", "QuestionID")
  blocks <- elements[[which(kinds == "BL")]]$Payload
  block_ids <- vapply(blocks, function(b) b$ID %||% NA_character_, "")
  flow_ids <- unlist(lapply(
    elements[[which(kinds == "FL")]]$Payload$Flow,
    function(f) f$ID
  ))
  flow_blocks <- blocks[match(flow_ids, block_ids)]
  flow_names <- vapply(flow_blocks, `[[`, "", "Description")
  stopifnot(
    "the measure blocks are not in the expected flow order" =
      identical(intersect(flow_names, dat_measures$Block), dat_measures$Block)
  )

  ## One row per displayed label of one item: its answer id, label and grades.
  item_rows <- list()
  for (m in seq_len(nrow(dat_measures))) {
    block <- flow_blocks[[match(dat_measures$Block[m], flow_names)]]
    qids <- unlist(lapply(block$BlockElements, function(e) {
      if (identical(e$Type, "Question")) e$QuestionID
    }))
    for (qid in qids) {
      p <- questions[[qid]]
      if (!grepl(dat_measures$Tag[m], p$DataExportTag)) next
      if (p$QuestionType == "Matrix") {
        rows <- as.character(unlist(p$ChoiceOrder))
        cols <- as.character(unlist(p$AnswerOrder))
        labels <- vapply(cols, function(k) p$Answers[[k]]$Display, "")
        for (r in rows) {
          raw <- dat_plain(p$Choices[[r]]$Display)
          grades <- Filter(function(g) g$ChoiceID == r, p$GradingData)
          item_rows[[length(item_rows) + 1]] <- list(
            Measure = dat_measures$Measure[m],
            MeasureItem = as.integer(sub("^(\\d+)\\..*", "\\1", raw)),
            Text = sub("^\\d+\\.\\s+", "", raw),
            labels = unname(labels),
            answer_ids = cols,
            grades = setNames(
              lapply(grades, `[[`, "Grades"),
              vapply(grades, `[[`, "", "AnswerID")
            )
          )
        }
      } else {
        cols <- as.character(unlist(p$ChoiceOrder))
        labels <- vapply(cols, function(k) dat_plain(p$Choices[[k]]$Display), "")
        item_rows[[length(item_rows) + 1]] <- list(
          Measure = dat_measures$Measure[m],
          MeasureItem = as.integer(sub("^\\D+_?", "", p$DataExportTag)),
          Text = dat_plain(p$QuestionText),
          labels = unname(labels),
          answer_ids = cols,
          grades = setNames(
            lapply(p$GradingData, `[[`, "Grades"),
            vapply(p$GradingData, `[[`, "", "ChoiceID")
          )
        )
      }
    }
  }

  ## Each item's forward values: for every scoring category that grades the
  ## item's labels (not "Skip"), the grades in label order. A category that
  ## grades the labels in rising order is forward, and one that grades them in
  ## falling order reverses the item. The item's forward values are the rising
  ## sequence, or the falling one turned round when no category is forward.
  ## Categories that give every label the same grade (the "Skipped" counters)
  ## carry no values. A label with no grade in any category is NA.
  forward_values <- function(row) {
    keep <- row$labels != "Skip"
    ids <- row$answer_ids[keep]
    cats <- unique(unlist(lapply(row$grades, names)))
    seqs <- lapply(cats, function(cat) {
      vapply(ids, function(a) {
        g <- row$grades[[a]][[cat]]
        if (is.null(g)) NA_integer_ else as.integer(g)
      }, 1L, USE.NAMES = FALSE)
    })
    seqs <- Filter(function(s) length(unique(s[!is.na(s)])) > 1, seqs)
    up <- Filter(function(s) !is.unsorted(s, na.rm = TRUE), seqs)
    down <- Filter(function(s) !is.unsorted(rev(s), na.rm = TRUE), seqs)
    if (length(up) > 0) {
      unique(up)
    } else {
      unique(lapply(down, rev))
    }
  }

  ## An item is one sentence or question, so the line breaks the file wraps
  ## some items with become spaces.
  items <- data.frame(
    Item = seq_along(item_rows),
    Measure = vapply(item_rows, `[[`, "", "Measure"),
    MeasureItem = vapply(item_rows, `[[`, 1L, "MeasureItem"),
    Text = gsub("\\s+", " ", vapply(item_rows, `[[`, "", "Text"))
  )

  ## Where the file's item text differs from the published key's, the text
  ## follows the key. Each change is listed in cairn/SOURCES.md, "HiTOP-DAT".
  ## CAT-PD item 194: the file ends the item "someon". IPIP CAT-PD-SF v1.1 key,
  ## Romantic Disinterest, SF 194: "Love the feeling of being intimately close
  ## with someone."
  cat194 <- items$Measure == "CAT-PD" & items$MeasureItem == 194L
  stopifnot(endsWith(items$Text[cat194], "close with someon"))
  items$Text[cat194] <- paste0(items$Text[cat194], "e")
  item_labels <- lapply(item_rows, function(r) r$labels[r$labels != "Skip"])
  cape_labels <- item_labels[[which(items$Measure == "CAPE")[1]]]
  dat_sets$cape <- cape_labels
  items$Choice_Set <- vapply(seq_along(item_rows), function(i) {
    stem <- c(
      "WHODAS" = "whodas", "IDAS-II" = "idas", "AUDIT" = "audit",
      "DUDIT" = "dudit", "CAPE" = "cape", "CAT-PD" = "catpd",
      "PHQ-15" = "phq15"
    )[[items$Measure[i]]]
    hit <- names(dat_sets)[
      startsWith(names(dat_sets), stem) &
        vapply(dat_sets, identical, TRUE, item_labels[[i]])
    ]
    if (length(hit) != 1) {
      stop("item ", i, " matches ", length(hit), " answer sets")
    }
    hit
  }, "")

  ## Each set's values are the forward values its items share. An item whose
  ## forward values differ from its set's is a defect of the file, recorded in
  ## cairn/SOURCES.md; only the ones listed here are allowed.
  known_grade_defects <- c(
    ## IDAS-II item 99 grades "Skip" as 6 and leaves "Extremely" ungraded.
    "IDAS-II 99"
  )
  item_values <- lapply(item_rows, forward_values)
  choices <- do.call(rbind, lapply(names(dat_sets), function(set) {
    members <- which(items$Choice_Set == set)
    found <- unique(unlist(item_values[members], recursive = FALSE))
    complete <- Filter(function(v) !anyNA(v), found)
    if (length(complete) != 1) {
      stop("answer set ", set, " has ", length(complete), " value sequences")
    }
    odd <- members[!vapply(
      item_values[members],
      function(v) length(v) == 1 && identical(v[[1]], complete[[1]]),
      TRUE
    )]
    odd_names <- paste(items$Measure[odd], items$MeasureItem[odd])
    if (!all(odd_names %in% known_grade_defects)) {
      stop("unexpected grades in ", paste(odd_names, collapse = ", "))
    }
    data.frame(Choice_Set = set, Value = complete[[1]], Label = dat_sets[[set]])
  }))

  readr::write_csv(items, "data-raw/hitopdat_items.csv")
  readr::write_csv(choices, "data-raw/hitopdat_choices.csv")

  ## The file's scale memberships: one row per item a scoring category grades.
  ## A category grading the labels in falling order reverses the item. The
  ## "Skipped" counters, and the categories the file grades but does not define
  ## (two on every CAPE item), are left out.
  scoring <- elements[[which(kinds == "SCO")]]$Payload
  categories <- data.frame(
    ID = vapply(scoring$ScoringCategories, `[[`, "", "ID"),
    Category = vapply(scoring$ScoringCategories, `[[`, "", "Name")
  )
  categories <- categories[!grepl("skipped", categories$Category,
                                  ignore.case = TRUE), ]
  memberships <- do.call(rbind, lapply(seq_along(item_rows), function(i) {
    row <- item_rows[[i]]
    ids <- row$answer_ids[row$labels != "Skip"]
    do.call(rbind, lapply(seq_len(nrow(categories)), function(k) {
      s <- vapply(ids, function(a) {
        g <- row$grades[[a]][[categories$ID[k]]]
        if (is.null(g)) NA_integer_ else as.integer(g)
      }, 1L, USE.NAMES = FALSE)
      if (length(unique(s[!is.na(s)])) < 2) return(NULL)
      data.frame(
        Category = categories$Category[k],
        Item = i,
        Reverse = is.unsorted(s, na.rm = TRUE)
      )
    }))
  }))
  memberships <- memberships[
    order(match(memberships$Category, categories$Category), memberships$Item),
  ]
  readr::write_csv(memberships, "data-raw/hitopdat_scale_items.csv")
}

## Item and response values are read as integers, never guessed and coerced
## afterwards, so the `spec` attribute the reader stores stays a truthful record
## of the types.
hitopdat_items <- readr::read_csv(
  "data-raw/hitopdat_items.csv",
  col_types = readr::cols(
    Item = readr::col_integer(),
    MeasureItem = readr::col_integer(),
    .default = readr::col_character()
  )
)
usethis::use_data(hitopdat_items, overwrite = TRUE)

hitopdat_choices <- readr::read_csv(
  "data-raw/hitopdat_choices.csv",
  col_types = readr::cols(
    Value = readr::col_integer(),
    .default = readr::col_character()
  )
)
usethis::use_data(hitopdat_choices, overwrite = TRUE)

## HiTOP-DAT Scales
## One row per scale the file scores. The file's own category names and their
## memberships are in `data-raw/hitopdat_scale_items.csv`. `Scale` is the name of
## the matching scale definition in the DAT manual (2021, pp. 20-25), and the
## crosswalk below maps each file category to it. The file's spellings that
## differ are listed in cairn/SOURCES.md, "HiTOP-DAT".
dat_scale_names <- c(
  "WHODAS" = "WHODAS",
  "IDAS - General Depression" = "General Depression",
  "IDAS - Dysphoria" = "Dysphoria",
  "IDAS - Lassitude" = "Lassitude",
  "IDAS - Insomnia" = "Insomnia",
  "IDAS - Sucidality" = "Suicidality",
  "IDAS - Appetite Loss" = "Appetite Loss",
  "IDAS - Appetite Gain" = "Appetite Gain",
  "IDAS - Well-Being" = "Well-Being",
  "IDAS - Ill Temper" = "Ill Temper",
  "IDAS - Mania" = "Mania",
  "IDAS - Euphoria" = "Euphoria",
  "IDAS - Panic" = "Panic",
  "IDAS - Social Anxiety" = "Social Anxiety",
  "IDAS - Claustraphobia" = "Claustrophobia",
  "IDAS - Traumatic Intrusions" = "Traumatic Intrusions",
  "IDAS - Traumatic Avoidance" = "Traumatic Avoidance",
  "IDAS - Checking" = "Checking",
  "IDAS - Ordering" = "Ordering",
  "IDAS - Cleaning" = "Cleaning",
  "AUDIT" = "Alcohol Use",
  "DUDIT" = "Drug Use",
  "CAPE - Positive" = "Positive Symptoms",
  "CAT - Affective Liability" = "Affective Lability",
  "CAT - Anger" = "Anger",
  "CAT - Anhedonia" = "Anhedonia",
  "CAT - Anxiousness" = "Anxiousness",
  "CAT - Callousness" = "Callousness",
  "CAT - Cognitive Problems" = "Cognitive Problems",
  "CAT - Depressiveness" = "Depressiveness",
  "CAT - Domineering" = "Domineering",
  "CAT - Emotional Detachment" = "Emotional Detachment",
  "CAT - Exhibitionism" = "Exhibitionism",
  "CAT - Fantasy Proneness" = "Fantasy Proneness",
  "CAT - Grandiosity" = "Grandiosity",
  "CAT - Health Anxiety" = "Health Anxiety",
  "CAT - Hostile Aggression" = "Hostile Aggression",
  "CAT - Irresponsibility" = "Irresponsibility",
  "CAT - Manipulativeness" = "Manipulativeness",
  "CAT - Mistrust" = "Mistrust",
  "CAT - Non-Perserverance" = "Non-Perseverance",
  "CAT - Non-Planfulness" = "Non-Premeditation",
  "CAT - Norm Violation" = "Norm Violation",
  "CAT - Peculiarity" = "Peculiarity",
  "CAT - Perfectionism" = "Perfectionism",
  "CAT - Relationship Insecurity" = "Relationship Insecurity",
  "CAT - Rigidity" = "Rigidity",
  "CAT - Risk Taking" = "Risk Taking",
  "CAT - Romantic Disinterest" = "Romantic Disinterest",
  "CAT - Rudeness" = "Rudeness",
  "CAT - Self-Harm" = "Self-Harm",
  "CAT - Social Withdrawal" = "Social Withdrawal",
  "CAT - Submissiveness" = "Submissiveness",
  "CAT - Unusual Beliefs" = "Unusual Beliefs",
  "CAT - Unusual Experiences" = "Unusual Experiences",
  "CAT - Workaholism" = "Workaholism",
  "PHQ" = "Physical Symptoms"
)

dat_memberships <- readr::read_csv(
  "data-raw/hitopdat_scale_items.csv",
  col_types = readr::cols(
    Category = readr::col_character(),
    Item = readr::col_integer(),
    Reverse = readr::col_logical()
  )
)
stopifnot(
  "a file category has no manual name" =
    setequal(unique(dat_memberships$Category), names(dat_scale_names))
)

## Where the file differs from a published key, the table follows the key.
## Each difference is listed in cairn/SOURCES.md, "HiTOP-DAT", and
## tests/testthat/test-keying-hitopdat.R checks the table against each key.

## Scales in battery order of their measure, then in the file's category order.
dat_measure_of <- function(category) {
  prefix <- sub(" - .*", "", category)
  unname(c(
    "WHODAS" = "WHODAS", "IDAS" = "IDAS-II", "AUDIT" = "AUDIT",
    "DUDIT" = "DUDIT", "CAPE" = "CAPE", "CAT" = "CAT-PD", "PHQ" = "PHQ-15"
  )[prefix])
}
dat_categories <- unique(dat_memberships$Category)
dat_measure_order <- unique(hitopdat_items$Measure)
dat_categories <- dat_categories[order(
  match(dat_measure_of(dat_categories), dat_measure_order),
  seq_along(dat_categories)
)]

hitopdat_scales <- tibble::tibble(
  Measure = dat_measure_of(dat_categories),
  Scale = unname(dat_scale_names[dat_categories]),
  camelCase = snakecase::to_any_case(Scale, case = "lower_camel"),
  itemNumbers = lapply(dat_categories, function(k) {
    dat_memberships$Item[dat_memberships$Category == k]
  }),
  reverseNumbers = lapply(dat_categories, function(k) {
    dat_memberships$Item[dat_memberships$Category == k &
                           dat_memberships$Reverse]
  })
)
hitopdat_scales$nItems <- lengths(hitopdat_scales$itemNumbers)
names(hitopdat_scales$itemNumbers) <- hitopdat_scales$camelCase
names(hitopdat_scales$reverseNumbers) <- hitopdat_scales$camelCase
stopifnot(!anyDuplicated(hitopdat_scales$camelCase))
usethis::use_data(hitopdat_scales, overwrite = TRUE)

## hitopdat_instructions (administration text) is internal data — see data-raw/sysdata.R
