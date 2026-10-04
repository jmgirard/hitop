# Compare the APA PID-5 child forms (ages 11 to 17) with the package's adult
# PID-5 tables (M161, AC1)
#
# The plan scores the child forms with the existing FULL and BF versions. That
# holds only if the child forms share the adult forms' items, order and keying.
# This script reads the two shelf PDFs with `pdftotext -raw` and checks:
#
#   1. the 220 full-form and 25 brief-form item texts, in order, against
#      `pid_items$Text` by FULL and BF number. Texts must match exactly after
#      whitespace and typographic quotes are normalized and a final period is
#      dropped (`pid_items$Text` stores none);
#   2. the child full form's Step 1 reverse list and Facet Table R marks
#      against `pid_items$Reverse`;
#   3. its Facet Table item lists against `pid_scales$FULL`;
#   4. its Domain Table primary facets against `pid_domains`;
#   5. the child brief form's Domain Scoring table against `pid_scales$BF`;
#   6. the stored child instructions (`pid_child_instructions` in
#      R/sysdata.rda): each form's instruction paragraph (first item page,
#      PDF p. 2), footer notice and response labels against its PDF text.
#
# It prints every difference it finds. Maintainer-run, never CI: it needs the
# gitignored shelf and pdftotext. It exits non-zero on any difference, so a
# printed report cannot be mistaken for a pass. Results are recorded in
# cairn/references/apa2013pid5child.md.

full_pdf <- "cairn/references/sources/apa2013pid5child.pdf"
bf_pdf <- "cairn/references/sources/apa2013pid5bfchild.pdf"
full_sha <- "6015387653f59fc357ac95a389577b414da4eb1aed03b83b8967359589d4a773"
bf_sha <- "1aa54248a6d60edc2a19609427c35ccb940eed71801ea3e9043de2e8377af2db"

bad <- character(0)
note <- function(...) bad <<- c(bad, paste0(...))

check_sha <- function(path, sha) {
  got <- sub(" .*", "", system2("shasum", c("-a", "256", shQuote(path)), stdout = TRUE))
  if (got != sha) stop(path, " does not match the recorded sha256.", call. = FALSE)
}
check_sha(full_pdf, full_sha)
check_sha(bf_pdf, bf_sha)

read_raw <- function(path) {
  system2("pdftotext", c("-raw", shQuote(path), "-"), stdout = TRUE)
}
normalize <- function(x) {
  x <- gsub("[‘’]", "'", x)
  x <- gsub("[“”]", "\"", x)
  x <- trimws(gsub("\\s+", " ", x))
  sub("\\.$", "", x)
}

# Each item ends at its "0 1 2 3" response row, so the text between the
# (k - 1)th and kth response rows holds item k. Its text starts after the last
# "<k> " in that stretch, which skips page headers and other numbers.
read_items <- function(txt, n_items) {
  flat <- paste(txt, collapse = " ")
  segments <- strsplit(flat, " 0 1 2 3", fixed = TRUE)[[1]]
  if (length(segments) < n_items) {
    note("found ", length(segments), " response rows, expected ", n_items)
    return(character(0))
  }
  text <- vapply(seq_len(n_items), function(k) {
    seg <- segments[k]
    at <- gregexpr(paste0("(?<![0-9])", k, " "), seg, perl = TRUE)[[1]]
    if (at[1] < 0) {
      note("item ", k, ": its number was not found")
      return(NA_character_)
    }
    start <- at[length(at)] + nchar(k) + 1L
    substring(seg, start)
  }, character(1))
  normalize(text)
}

pid_items <- utils::read.csv("data-raw/pid_items.csv", encoding = "UTF-8")
pid_items$Text <- normalize(pid_items$Text)

# ---- Full form ---------------------------------------------------------------
full <- read_raw(full_pdf)
key_at <- grep("Facet and Domain Scoring", full, fixed = TRUE)[1]
full_text <- read_items(full[seq_len(key_at - 1)], 220)
adult_full <- pid_items$Text[match(1:220, pid_items$FULL)]
for (i in which(full_text != adult_full)) {
  note("full item ", i, ": child '", full_text[i], "' vs package '", adult_full[i], "'")
}

key <- full[key_at:length(full)]
step1 <- paste(key[grep("^Step 1:", key) + 0:1], collapse = " ")
step1 <- sub(".*becomes 3\\):", "", step1)
step1 <- as.integer(regmatches(step1, gregexpr("[0-9]+", step1))[[1]])
adult_reverse <- pid_items$FULL[pid_items$Reverse]
if (!setequal(step1, adult_reverse)) {
  note("Step 1 reverse list ", toString(step1), " vs package ", toString(sort(adult_reverse)))
}

facets <- unique(pid_items$Facet)
rows <- key[grep("^Anhedonia ", key)[1]:grep("^Withdrawal ", key)[1]]
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
facet_items <- lapply(entries, function(s) regmatches(s, gregexpr("[0-9]+R?", s))[[1]])
if (length(facet_items) != 25) note("Facet Table has ", length(facet_items), " facets")
r_marked <- sort(as.integer(sub("R", "", grep("R", unlist(facet_items), value = TRUE))))
if (!setequal(r_marked, adult_reverse)) {
  note("Facet Table R marks ", toString(r_marked), " vs package ", toString(sort(adult_reverse)))
}
scales <- local({
  e <- new.env()
  load("data/pid_scales.rda", envir = e)
  e$pid_scales
})
for (f in names(facet_items)) {
  printed <- as.integer(sub("R", "", facet_items[[f]]))
  pkg <- scales$FULL$itemNumbers[[match(f, scales$FULL$Facet)]]
  if (!setequal(printed, pkg) || anyDuplicated(printed)) {
    note("facet ", f, ": child ", toString(printed), " vs package ", toString(pkg))
  }
}

domain_lines <- key[grep("^(Negative Affect|Detachment|Antagonism|Disinhibition|Psychoticism)", key)]
domains <- local({
  e <- new.env()
  load("data/pid_domains.rda", envir = e)
  e$pid_domains
})
flat_key <- paste(key, collapse = " ")
for (d in seq_len(nrow(domains))) {
  wanted <- paste(domains$primaryFacets[[d]], collapse = ", ")
  if (!grepl(wanted, gsub("\\s+", " ", flat_key), fixed = TRUE)) {
    note("domain ", domains$Domain[d], ": primary facets '", wanted, "' not found in the Domain Table")
  }
}

# ---- Brief form --------------------------------------------------------------
bf <- read_raw(bf_pdf)
bf_key_at <- grep("Personality Trait Domain Scoring", bf, fixed = TRUE)[1]
bf_text <- read_items(bf[seq_len(bf_key_at - 1)], 25)
adult_bf <- pid_items$Text[match(1:25, pid_items$BF)]
for (i in which(bf_text != adult_bf)) {
  note("BF item ", i, ": child '", bf_text[i], "' vs package '", adult_bf[i], "'")
}
# The key prints "Negative Affect"; the package's domain is "Negative
# affectivity". Rows are matched by name, not position.
bf_names <- c(
  "Negative Affect" = "Negative affectivity", "Detachment" = "Detachment",
  "Antagonism" = "Antagonism", "Disinhibition" = "Disinhibition",
  "Psychoticism" = "Psychoticism"
)
for (k in seq_along(bf_names)) {
  line <- grep(paste0("^", names(bf_names)[k], " [0-9]"), bf[bf_key_at:length(bf)], value = TRUE)[1]
  printed <- as.integer(regmatches(line, gregexpr("[0-9]+", line))[[1]])
  pkg <- scales$BF$itemNumbers[[match(bf_names[[k]], scales$BF$Domain)]]
  if (!setequal(printed, pkg)) {
    note("BF domain ", names(bf_names)[k], ": child ", toString(printed), " vs package ", toString(pkg))
  }
}
if (any(grepl("[0-9]R\\b", bf[bf_key_at:length(bf)]))) note("BF child key marks a reverse item")

# ---- Instructions in R/sysdata.rda -------------------------------------------
# Each form's stored instruction paragraph (first item page, PDF p. 2) and
# footer notice must appear in its
# PDF text, and the response labels must be its column heads in order (raw
# mode reads the heads column by column). Quotes, apostrophes and spacing are
# normalized as for the item text, but the final period is kept, so the match
# checks it too.
sysdata <- new.env()
load("R/sysdata.rda", envir = sysdata)
ascii <- function(x) {
  x <- gsub("[‘’]", "'", x)
  x <- gsub("[“”]", "\"", x)
  trimws(gsub("\\s+", " ", x))
}
for (form in c("FULL", "BF")) {
  instr <- sysdata$pid_child_instructions[[form]]
  flat <- ascii(paste(if (form == "FULL") full else bf, collapse = " "))
  for (part in c("start", "notice")) {
    if (!grepl(ascii(instr[[part]]), flat, fixed = TRUE)) {
      note(form, " instructions$", part, " is not in the PDF text")
    }
  }
  if (!grepl(paste(instr$options$label, collapse = " "), flat, fixed = TRUE)) {
    note(form, " instructions$options labels are not the form's column heads, in order")
  }
  if (!identical(instr$options$value, 0:3)) {
    note(form, " instructions$options values are not 0 to 3")
  }
}

cat("Full form: ", full_pdf, " (sha256 matches)\n", sep = "")
cat("Brief form: ", bf_pdf, " (sha256 matches)\n", sep = "")
cat("Items read: ", length(full_text), " full, ", length(bf_text), " brief\n", sep = "")
cat("Step 1 reverse list (", length(step1), "): ", toString(step1), "\n", sep = "")

if (length(bad)) {
  cat("\nDIFFERENCES (", length(bad), "):\n", sep = "")
  cat(paste0("  ", bad), sep = "\n")
  quit(status = 1)
}
cat("\nNO DIFFERENCES: 220 + 25 texts, reverse list, 25 facets, 5 domains, 5 BF domains and both forms' instructions match the package.\n")
