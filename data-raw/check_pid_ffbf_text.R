# Check the PID-5-FFBF transcription against Table S3 and the authors' code
# (M162, AC1 and AC2)
#
# data-raw/pid_ffbf_items.csv, from which data-raw/pid_info.R builds the shipped
# `pid_ffbf_items` that this script reads, was built from the word positions that
# `pdftotext -bbox` reports for Table S3 of Niemeyer et al. (2022): each word
# went to the cell its column and row place it in. This script reads the same
# PDF a second way, `pdftotext -raw`, which keeps each cell's lines together in
# the order self German, self English, informant German, informant English.
# For each item it looks for the split of the item's lines into four runs that
# gives the table's four texts under the rule below. So every word must be in
# the table, in its cell in reading order. It also checks:
#
#   1. the item numbers, 1 to 100, each once;
#   2. each item's facet, read from the facet heading above it;
#   3. the reverse items, read from a "(-)" mark anywhere in the item's text
#      (Table S3 prints it in the two German cells);
#   4. against the authors' code: each facet's four items in its self-report
#      and informant lists, for the FFBF and for the original form; the two
#      recoded items, for self report and both informants; the facets of the
#      two four-factor domains that are not APA domains, for both forms; and
#      the unadapted items (named without "_N_") against Table S3's a, b and
#      c marks.
#
# The rule (M162 AC1): drop a parenthetical source note that begins "(G" or
# "(E-", the "(-)" mark, the stray markers E14, E18 and E77, a leading ellipsis
# and a final period; turn typographic quotes, apostrophes and the acute
# accent into ASCII; in a German text, drop a hyphen inside a word (a hyphen
# followed by a lowercase letter, with or without a line break); in an English
# text, join a word split at a hyphen by a line break, keeping the hyphen.
#
# Maintainer-run, never CI: it needs the gitignored shelf and pdftotext. It
# exits non-zero on any departure, so a printed report cannot be mistaken for
# a pass. Source: cairn/references/niemeyer2022.md.

source_pdf <- "cairn/references/sources/niemeyer2022_tableS3.pdf"
source_sha <- "d0a04a14237d4ff6cd54c9cf97cd761a32080a84061c8d90a5eb9c5e231330b4"
code_file <- "cairn/references/sources/niemeyer2022_code.R"
code_sha <- "41547644d7ad0b0e59993e25c01cca63c281482d71e77fadeba42661629bbcdc"

bad <- character(0)
note <- function(...) bad <<- c(bad, paste0(...))

# Run a shell tool and stop with its name if it fails, so a missing or failing
# pdftotext, pdfinfo or shasum is not reported as a downstream parse error.
run <- function(cmd, args) {
  out <- suppressWarnings(system2(cmd, args, stdout = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop(cmd, " failed with status ", status, ".", call. = FALSE)
  }
  out
}

check_sha <- function(path, want) {
  got <- run("shasum", c("-a", "256", shQuote(path)))
  if (sub(" .*", "", got) != want) {
    stop(path, " does not match the recorded sha256.", call. = FALSE)
  }
}
check_sha(source_pdf, source_sha)
check_sha(code_file, code_sha)

normalize <- function(x, german) {
  x <- gsub("\\((G|E)\\.?-\\s*PID[^)]*\\)", " ", x, perl = TRUE)
  x <- gsub("(-)", " ", x, fixed = TRUE)
  x <- gsub("\\bE(14|18|77)\\b", " ", x, perl = TRUE)
  x <- gsub("[‘’´]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- trimws(gsub("\\s+", " ", x))
  x <- sub("^(…|\\.\\.\\.)\\s*", "", x)
  if (german) {
    x <- gsub("(\\w)- ?(?=[a-zäöüß])", "\\1", x, perl = TRUE)
  } else {
    x <- gsub("(\\w)- (\\w)", "\\1-\\2", x, perl = TRUE)
  }
  sub("\\.$", "", trimws(x))
}

load("data/pid_ffbf_items.rda")
items <- as.data.frame(pid_ffbf_items)
if (!identical(items$FFBF, 1:100)) note("pid_ffbf_items is not items 1 to 100 in order")
cols <- c("TextDE", "Text", "TextIRFDE", "TextIRF")
german <- c(TRUE, FALSE, TRUE, FALSE)

load("data/pid_items.rda")
facet_names <- sort(unique(pid_items$Facet))

# Read the PDF page by page; drop each page's first line (its page number).
n_pages <- as.integer(sub(".*:\\s+", "", grep(
  "^Pages:", run("pdfinfo", shQuote(source_pdf)),
  value = TRUE
)))
lines <- character(0)
for (p in seq_len(n_pages)) {
  pg <- run("pdftotext", c("-raw", "-f", p, "-l", p, shQuote(source_pdf), "-"))
  pg <- trimws(gsub("\f", "", pg, fixed = TRUE))
  pg <- pg[nzchar(pg)]
  lines <- c(lines, pg[-1])
}
note_at <- grep("^Note\\. \\(-\\) = reverse coded", lines)
lines <- lines[seq_len(note_at - 1)]
start_at <- grep("^Item Content of Self", lines)[1]
if (is.na(start_at) || start_at >= length(lines)) {
  stop("Table S3's title line was not found before its items.", call. = FALSE)
}
lines <- lines[(start_at + 1):length(lines)]

# Walk the lines: a facet heading sets the facet, an item-number line starts
# an item. "84)" and "5 Item 83)" are wrapped note text, not item numbers.
item_re <- "^([0-9]{1,3})[a-f]*( (.*))?$"
cur_facet <- NA_character_
marks <- list()
seen <- list()
cur <- NULL
flush <- function() {
  if (!is.null(cur)) seen[[length(seen) + 1]] <<- cur
}
for (ln in lines) {
  ln <- trimws(ln)
  if (ln %in% facet_names) {
    flush(); cur <- NULL
    cur_facet <- ln
  } else if (grepl(item_re, ln) && !grepl("^[0-9]+ Item|^[0-9]+\\)", ln)) {
    flush()
    num <- as.integer(sub(item_re, "\\1", ln))
    letters_on <- strsplit(sub("^[0-9]{1,3}([a-f]*).*$", "\\1", ln), "")[[1]]
    for (l in letters_on) marks[[l]] <- c(marks[[l]], num)
    rest <- sub(item_re, "\\3", ln)
    cur <- list(num = num, facet = cur_facet, lines = if (nzchar(rest)) rest else character(0))
  } else if (!is.null(cur)) {
    cur$lines <- c(cur$lines, ln)
  }
}
flush()

nums <- vapply(seen, `[[`, integer(1), "num")
if (!identical(sort(nums), 1:100)) {
  note("PDF item numbers are not 1 to 100, each once: ", toString(nums))
}

# Split an item's words into four runs, in order, whose texts are the table's
# four texts under the rule. Two cells can share a line in -raw output, so
# the split points are word positions. Backtracking tries every point at which
# a run matches, since a run can match at two points (a trailing "(-)").
split_four <- function(x, want) {
  w <- unlist(strsplit(paste(x, collapse = " "), " ", fixed = TRUE))
  w <- w[nzchar(w)]
  go <- function(from, k) {
    if (k > 4) return(from > length(w))
    if (from > length(w)) return(FALSE)
    for (to in from:length(w)) {
      if (normalize(paste(w[from:to], collapse = " "), german[k]) == want[k] &&
          go(to + 1, k + 1)) {
        return(TRUE)
      }
    }
    FALSE
  }
  go(1, 1)
}

pdf_reverse <- integer(0)
for (it in seen) {
  row <- items[items$FFBF == it$num, ]
  if (nrow(row) != 1) next
  if (!identical(row$Facet, it$facet)) {
    note("item ", it$num, ": table facet ", row$Facet, ", PDF heading ", it$facet)
  }
  want <- unlist(row[cols])
  if (!split_four(it$lines, want)) {
    note("item ", it$num, ": no split of the PDF lines gives the table's four texts")
  }
  if (grepl("(-)", paste(it$lines, collapse = " "), fixed = TRUE)) {
    pdf_reverse <- c(pdf_reverse, it$num)
  }
}
pdf_reverse <- sort(pdf_reverse)
if (!identical(pdf_reverse, sort(items$FFBF[items$Reverse]))) {
  note("reverse items: PDF marks ", toString(pdf_reverse), ", table flags ",
       toString(items$FFBF[items$Reverse]))
}

# The authors' code: facet item lists (self report), recodes, domains.
code <- readLines(code_file, encoding = "UTF-8", warn = FALSE)
code_facets <- c(
  insec = "Separation Insecurity", anxiou = "Anxiousness",
  emotion = "Emotional Lability", submiss = "Submissiveness",
  persev = "Perseveration", withdraw = "Withdrawal",
  intim = "Intimacy Avoidance", anhed = "Anhedonia",
  affect = "Restricted Affectivity", depress = "Depressivity",
  suspic = "Suspiciousness", manipu = "Manipulativeness",
  deceit = "Deceitfulness", grandios = "Grandiosity",
  callou = "Callousness", attent = "Attention Seeking",
  hostil = "Hostility", impuls = "Impulsivity",
  irres = "Irresponsibility", perfect = "Rigid Perfectionism",
  distract = "Distractibility", risk = "Risk Taking",
  beliefs = "Unusual Beliefs & Experiences",
  dysreg = "Perceptual Dysregulation", eccent = "Eccentricity"
)
# Each facet list is defined twice per form, first for the FFBF (item names
# with "_N_" for adapted items) and later for the original form (lines 3311 to
# 3361); all four must equal the table's. The FFBF definitions also give the
# unadapted items, those named without "_N_".
unadapted <- list(self = integer(0), acqu = integer(0))
for (form in c("self", "acqu")) {
  for (stem in names(code_facets)) {
    ln <- grep(paste0("^", stem, "_", form, "\\.nam\\s*<-\\s*", form, "\\.nam"), code,
               value = TRUE, perl = TRUE)
    if (length(ln) != 2) {
      note("code: ", length(ln), " ", form, " lists for ", stem, ", not 2")
      next
    }
    csv <- sort(items$FFBF[items$Facet == code_facets[[stem]]])
    for (one in ln) {
      got <- sort(as.integer(regmatches(one, gregexpr("(?<=PID)[0-9]+", one, perl = TRUE))[[1]]))
      if (!identical(got, csv)) {
        note("code: ", stem, "_", form, " lists ", toString(got), ", table ", toString(csv))
      }
    }
    plain <- regmatches(ln[1], gregexpr("(?<=PID)[0-9]+(?=_(self|acqu))", ln[1], perl = TRUE))[[1]]
    unadapted[[form]] <- c(unadapted[[form]], as.integer(plain))
  }
}
# Table S3's note: a = not adapted, b = not adapted for informant reports,
# c = not adapted for self-reports.
want_self <- sort(unique(c(marks$a, marks$c)))
want_acqu <- sort(unique(c(marks$a, marks$b)))
if (!identical(sort(unadapted$self), want_self)) {
  note("code's unadapted self items ", toString(sort(unadapted$self)),
       ", Table S3 a and c marks ", toString(want_self))
}
if (!identical(sort(unadapted$acqu), want_acqu)) {
  note("code's unadapted informant items ", toString(sort(unadapted$acqu)),
       ", Table S3 a and b marks ", toString(want_acqu))
}
# The recodes: 3 minus the response, for self report and both informants.
for (suffix in c("self", "acqu1", "acqu2")) {
  pat <- paste0("^data\\$PID([0-9]+)_N_", suffix, " <- 3 - ")
  recoded <- sort(as.integer(unique(sub(paste0(pat, ".*"), "\\1", grep(pat, code, value = TRUE)))))
  if (!identical(recoded, sort(items$FFBF[items$Reverse]))) {
    note("code recodes (", suffix, ") ", toString(recoded), ", table flags ",
         toString(items$FFBF[items$Reverse]))
  }
}
# The two four-factor domains that are not APA domains, for both forms,
# against the shipped `pid_ffbf_domains` rows 6 and 7.
dom_line <- function(form, k) {
  ln <- grep(paste0("^domain4_", form, "_i\\.nam\\[\\[", k, "\\]\\] <- c\\("), code, value = TRUE)
  stems <- regmatches(ln, gregexpr(paste0("[a-z]+(?=_", form, "\\.nam)"), ln, perl = TRUE))[[1]]
  unname(code_facets[stems])
}
load("data/pid_ffbf_domains.rda")
for (form in c("self", "acqu")) {
  for (k in 3:4) {
    row <- k + 3
    if (!setequal(dom_line(form, k), pid_ffbf_domains$primaryFacets[[row]])) {
      note("code domain4 ", form, " ", k, ": ", toString(dom_line(form, k)),
           "; pid_ffbf_domains row ", row, ": ", toString(pid_ffbf_domains$primaryFacets[[row]]))
    }
  }
}
code_da <- dom_line("self", 3)
code_ins <- dom_line("self", 4)

cat("Source: ", source_pdf, "\n", sep = "")
cat("sha256: ", source_sha, " (matches)\n", sep = "")
cat("Code: ", code_file, " (sha256 matches)\n", sep = "")
cat("Items read from the PDF: ", length(seen), "\n", sep = "")
cat("Reverse items (PDF, table, code): ", toString(pdf_reverse), "\n", sep = "")
cat("Code domain 3 (Disinhibited Aggression): ", toString(code_da), "\n", sep = "")
cat("Code domain 4 (Insecurity): ", toString(code_ins), "\n", sep = "")

if (length(bad)) {
  cat("\nFAIL (", length(bad), "):\n", sep = "")
  cat(paste0("  ", bad), sep = "\n")
  quit(status = 1)
}
cat("Unadapted items (code = Table S3 marks): ", length(unadapted$self), " self, ",
    length(unadapted$acqu), " informant\n", sep = "")
cat("\nPASS: 400 texts, 100 facets and 2 reverse items match Table S3;",
    "100 facet lists (self and informant, FFBF and original form), the",
    "recodes, the forensic domains and the unadapted items match the",
    "authors' code.\n")
