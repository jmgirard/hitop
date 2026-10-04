# Check the PID-5 Informant Form transcription against the APA key (M159, AC1)
#
# data-raw/pid_irf_items.csv was built from `pdftotext -layout` output of the
# shelf copy of the APA PID-5-IRF form and scoring key. This script reads the
# same PDF a second way, `pdftotext -raw`, which keeps reading order rather
# than page layout, and checks three things against the CSV:
#
#   1. all 218 item texts, after the CSV's normalization (no leading ellipsis,
#      no final period, ASCII quotes and apostrophes);
#   2. every item's facet, against the key's Facet Table;
#   3. the CSV's FULL column, which maps each IRF item to the self-report item
#      with the same facet (the IRF drops self-report items 96 and 177).
#
# It also prints the key's two reverse lists, Step 1 and the R marks in the
# Facet Table, which disagree on items 98 and 176. Which list governs is
# recorded in cairn/SOURCES.md.
#
# Maintainer-run, never CI: it needs the gitignored shelf and pdftotext. It
# exits non-zero on any departure, so a printed report cannot be mistaken for
# a pass. Source: cairn/references/apa2013pid5irf.md.

source_pdf <- "cairn/references/sources/apa2013pid5irf.pdf"
source_sha <- "be4a260a20706002a7ead4fd61261f86d2023cb6f221f7cfb04fb4d940f8af4d"

bad <- character(0)
note <- function(...) bad <<- c(bad, paste0(...))

sha <- system2("shasum", c("-a", "256", shQuote(source_pdf)), stdout = TRUE)
if (sub(" .*", "", sha) != source_sha) {
  stop("The shelf PDF does not match the recorded sha256.", call. = FALSE)
}

txt <- system2("pdftotext", c("-raw", shQuote(source_pdf), "-"), stdout = TRUE)
key_start <- grep("Facet and Domain Scoring", txt, fixed = TRUE)[1]
form <- paste(txt[seq_len(key_start - 1)], collapse = " ")
key <- txt[key_start:length(txt)]

# 1. Item text. Each item runs from "<n> …" to its "0 1 2 3" response row.
hits <- regmatches(
  form,
  gregexpr("\\b(\\d{1,3}) …(.*?) 0 1 2 3", form, perl = TRUE)
)[[1]]
num <- as.integer(sub("^(\\d{1,3}) .*", "\\1", hits))
pdf_text <- sub("^\\d{1,3} …(.*) 0 1 2 3$", "\\1", hits)
normalize <- function(x) {
  x <- gsub("[‘’]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- trimws(gsub("\\s+", " ", x))
  sub("\\.$", "", x)
}
pdf_text <- normalize(pdf_text)

items <- utils::read.csv("data-raw/pid_irf_items.csv", encoding = "UTF-8")
if (!identical(num, 1:218)) {
  note("PDF item numbers are not 1 to 218 in order")
}
if (!identical(items$IRF, 1:218)) {
  note("CSV item numbers are not 1 to 218 in order")
}
if (length(num) == 218 && identical(items$IRF, 1:218)) {
  diff <- which(pdf_text != items$Text)
  for (i in diff) {
    note("item ", i, ": PDF '", pdf_text[i], "' vs CSV '", items$Text[i], "'")
  }
}

# 2. Facet Table. A facet's item list can wrap onto a second line, and the
# raw text then puts the facet name alone on the line above it.
table_start <- grep("^Anhedonia ", key)[1]
table_end <- grep("^Withdrawal ", key)[1]
rows <- key[table_start:table_end]
facets <- sort(unique(items$Facet))
entries <- list()
current <- NULL
for (line in rows) {
  name <- facets[vapply(facets, function(f) startsWith(line, f), logical(1))]
  if (length(name) > 1) name <- name[which.max(nchar(name))]
  if (length(name) == 1) {
    current <- name
    line <- substring(line, nchar(name) + 1)
  }
  entries[[current]] <- paste(entries[[current]], line)
}
key_items <- lapply(entries, function(s) {
  regmatches(s, gregexpr("\\d+R?", s))[[1]]
})
if (!setequal(names(key_items), facets) || length(key_items) != 25) {
  note("Facet Table names do not match the 25 CSV facets")
}
for (f in intersect(names(key_items), facets)) {
  printed <- as.integer(sub("R", "", key_items[[f]]))
  csv <- items$IRF[items$Facet == f]
  if (!setequal(printed, csv) || anyDuplicated(printed)) {
    note("facet ", f, ": key ", toString(printed), " vs CSV ", toString(csv))
  }
}
r_marked <- sort(as.integer(sub("R", "", grep("R", unlist(key_items), value = TRUE))))

step1 <- paste(key[grep("^Step 1:", key) + 0:1], collapse = " ")
step1 <- sub(".*becomes 3\\):", "", step1)
step1 <- as.integer(regmatches(step1, gregexpr("\\d+", step1))[[1]])

# 3. FULL mapping: IRF n is self-report n through 95, n + 1 through 175, and
# n + 2 after that, and each pair shares its facet.
expected_full <- ifelse(items$IRF <= 95, items$IRF,
  ifelse(items$IRF <= 175, items$IRF + 1L, items$IRF + 2L)
)
if (!identical(as.integer(items$FULL), as.integer(expected_full))) {
  note("FULL column departs from the n / n + 1 / n + 2 mapping")
}
sr <- utils::read.csv("data-raw/pid_items.csv", encoding = "UTF-8")
sr_facet <- sr$Facet[match(items$FULL, sr$FULL)]
for (i in which(sr_facet != items$Facet)) {
  note("item ", i, ": self-report item ", items$FULL[i], " has facet ", sr_facet[i])
}
sr_reverse <- items$IRF[sr$Reverse[match(items$FULL, sr$FULL)]]

# 4. Instructions in R/sysdata.rda: the first-page and later-page texts and the
# rating prompt, normalized as the item text is (the stored text keeps its
# final period, so the comparison adds it back).
sysdata <- new.env()
load("R/sysdata.rda", envir = sysdata)
instr <- sysdata$pid_irf_instructions
flat <- normalize(form)
for (part in c("start", "continue", "prompt")) {
  if (!grepl(normalize(instr[[part]]), paste0(flat, "."), fixed = TRUE)) {
    note("instructions$", part, " is not in the PDF text")
  }
}
if (!identical(instr$options$label, c(
  "Very False or Often False", "Sometimes or Somewhat False",
  "Sometimes or Somewhat True", "Very True or Often True"
))) {
  note("instructions$options labels differ from the form's column heads")
}

cat("Source: ", source_pdf, "\n", sep = "")
cat("sha256: ", source_sha, " (matches)\n", sep = "")
cat("Items read from the PDF: ", length(num), "\n", sep = "")
cat("Step 1 reverse list (", length(step1), "): ", toString(step1), "\n", sep = "")
cat("Facet Table R marks (", length(r_marked), "): ", toString(r_marked), "\n", sep = "")
cat("Self-report reverse flags, mapped (", length(sr_reverse), "): ",
  toString(sr_reverse), "\n", sep = "")

if (length(bad)) {
  cat("\nFAIL (", length(bad), "):\n", sep = "")
  cat(paste0("  ", bad), sep = "\n")
  quit(status = 1)
}
cat("\nPASS: 218 texts, 25 facets and the FULL mapping match the key.\n")
