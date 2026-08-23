# Parsers for the generate_* export-family tests (milestone M010).
#
# The oracle strategy for the file generators is "parse the output back and
# compare it to the SOURCE instrument datasets, derived independently" -- the
# analog of independent recomputation for scoring. These helpers turn each
# generated artifact into an inspectable R object; the source of truth for the
# expected values is always the *_items / *_instructions / hitophsum_* data,
# never the generated file itself.

# ---- Qualtrics .txt ---------------------------------------------------------
#
# Parse a Qualtrics Advanced-Format import file into its structural parts.
# Returns a list:
#   lines            character vector of all lines
#   advanced_format  logical: first non-empty line is [[AdvancedFormat]]
#   block            block name from [[Block:...]] (or NA)
#   has_instructions logical: an [[ID:start_instructions]] block is present
#   questions        data.frame(id, num, text) in file order
#   choices          data.frame(value, label) from the first question's
#                    [[AdvancedChoices]] block (identical for every item)
#   pagebreak_after  integer vector of question indices after which a
#                    [[PageBreak]] appears
read_qualtrics <- function(file) {
  lines <- readLines(file, warn = FALSE)
  non_empty <- lines[nzchar(trimws(lines))]

  block <- NA_character_
  m <- regmatches(lines, regexpr("(?<=\\[\\[Block:)[^]]+", lines, perl = TRUE))
  m <- m[lengths(m) > 0]
  if (length(m) > 0) block <- m[[1]]

  q_idx <- which(lines == "[[Question:MC:SingleAnswer]]")
  questions <- data.frame(
    id = character(0),
    num = integer(0),
    text = character(0),
    stringsAsFactors = FALSE
  )
  choices <- NULL
  for (qi in seq_along(q_idx)) {
    pos <- q_idx[qi]
    id_line <- lines[pos + 1L] # [[ID:PREFIX_NNN]]
    id <- sub("^\\[\\[ID:(.*)\\]\\]$", "\\1", id_line)
    num <- as.integer(sub("^.*_0*([0-9]+)$", "\\1", id))
    text <- lines[pos + 2L]
    questions <- rbind(
      questions,
      data.frame(id = id, num = num, text = text, stringsAsFactors = FALSE)
    )

    # Parse the [[AdvancedChoices]] block for the first question only.
    if (is.null(choices)) {
      ac <- which(lines == "[[AdvancedChoices]]")
      ac <- ac[ac > pos][1]
      if (!is.na(ac)) {
        vals <- integer(0)
        labs <- character(0)
        j <- ac + 1L
        while (j <= length(lines) && grepl("^\\[\\[Choice:", lines[j])) {
          vals <- c(vals, as.integer(sub("^\\[\\[Choice:(.*)\\]\\]$", "\\1", lines[j])))
          labs <- c(labs, lines[j + 1L])
          j <- j + 2L
        }
        choices <- data.frame(value = vals, label = labs, stringsAsFactors = FALSE)
      }
    }
  }

  # Which question index does each [[PageBreak]] follow?
  pb_idx <- which(lines == "[[PageBreak]]")
  pagebreak_after <- vapply(
    pb_idx,
    function(p) sum(q_idx < p),
    integer(1)
  )

  list(
    lines = lines,
    advanced_format = length(non_empty) > 0 && non_empty[1] == "[[AdvancedFormat]]",
    block = block,
    has_instructions = any(lines == "[[ID:start_instructions]]"),
    questions = questions,
    choices = choices,
    pagebreak_after = pagebreak_after
  )
}

# ---- REDCap .zip ------------------------------------------------------------
#
# Unzip a REDCap instrument ZIP and read its single data-dictionary CSV back as
# an all-character data.frame (empty fields stay "" so branching-logic strings
# compare exactly). check.names = FALSE preserves the REDCap column titles.
read_redcap_csv <- function(zipfile) {
  exdir <- tempfile("redcap")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(zipfile, exdir = exdir)
  csv <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  stopifnot(length(csv) == 1)
  utils::read.csv(
    csv[[1]],
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(0)
  )
}

# ---- DOCX -------------------------------------------------------------------
#
# A .docx is a ZIP; return its word/document.xml as a single string.
read_docx_xml <- function(file) {
  exdir <- tempfile("docx")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(file, files = "word/document.xml", exdir = exdir)
  paste(
    readLines(
      file.path(exdir, "word", "document.xml"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

# Extract the document header title from a .docx.
#
# build_hitop_doc() puts `title` in the section's `header_default`, so it lands
# in word/header*.xml and NOT in word/document.xml -- read_docx_xml() cannot
# see it. Every generator writes the title as the header's only content, so
# concatenating the header parts' runs returns the title verbatim.
docx_header_title <- function(file) {
  exdir <- tempfile("docx")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  headers <- grep(
    "^word/header[0-9]*\\.xml$",
    utils::unzip(file, list = TRUE)$Name,
    value = TRUE
  )
  if (length(headers) == 0L) return("")
  utils::unzip(file, files = headers, exdir = exdir)
  xml <- paste(
    unlist(lapply(
      file.path(exdir, headers),
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    )),
    collapse = "\n"
  )
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  unescape_xml(paste(gsub("<[^>]+>", "", runs), collapse = ""))
}

# Extract the concatenated text of all footer parts in a .docx.
read_docx_footer <- function(file) {
  exdir <- tempfile("docx")
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  footers <- grep(
    "^word/footer[0-9]*\\.xml$",
    utils::unzip(file, list = TRUE)$Name,
    value = TRUE
  )
  utils::unzip(file, files = footers, exdir = exdir)
  xml <- paste(
    unlist(lapply(
      file.path(exdir, footers),
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    )),
    collapse = "\n"
  )
  # Concatenate the <w:t> runs
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  paste(gsub("<[^>]+>", "", runs), collapse = "")
}

# Extract the response-option legend lines from a .docx, in document order.
#
# The legend sits in the items table's own header as one <w:t> run per line.
# Anchoring on "<value> = " is what scopes the result to the legend: item text
# runs read "12.  I ...", the option columns are bare digits, and the scoring
# table's header cells are "Scale"/"Items", so none of them can match. Returns
# character(0) when a document has no legend.
docx_legend_lines <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  grep("^[0-9]+ = ", gsub("<[^>]+>", "", runs), value = TRUE)
}

# Split a legend line back into its (value, label) pairs.
docx_legend_pairs <- function(lines) {
  pairs <- unlist(strsplit(lines, " • ", fixed = TRUE))
  data.frame(
    value = sub(" = .*$", "", pairs),
    label = sub("^[0-9]+ = ", "", pairs),
    stringsAsFactors = FALSE
  )
}

# Extract the printed item rows from a .docx, in document order.
#
# make_items_table() builds each item cell as `paste0(number, ".  ", text)`
# (R/generate_docx.R), so an item row is the only <w:t> run that opens with
# digits, a period, and two spaces. The legend runs read "0 = Never", the
# option columns are bare digits, the scoring table's Items cells read
# "1, 2(R), 3", and the crosswalk pairs are joined with an arrow, so none of
# them can match. Returns a data.frame(number, text) with zero rows when a
# document has no items table.
docx_item_rows <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  txt <- unescape_xml(gsub("<[^>]+>", "", runs))
  hits <- grep("^[0-9]+\\.  ", txt, value = TRUE)
  data.frame(
    number = sub("^([0-9]+)\\.  .*$", "\\1", hits),
    text = sub("^[0-9]+\\.  ", "", hits),
    stringsAsFactors = FALSE
  )
}

# Undo the XML entity escaping officer applies when it writes a run.
unescape_xml <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", '"', x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

# Extract the scoring table's (scale, items) pairs from a .docx.
#
# make_scoring_table() lays the scales out in two side-by-side (Scale, Items)
# column pairs, so after the header runs -- "Scale", "Items", "Scale", "Items"
# -- the body cells arrive as alternating scale-name and item-list runs in
# document order. An unpaired trailing NA cell prints as nothing at all
# (`colformat_char(na_str = "")`), so the alternation never breaks. Returns a
# data.frame(scale, items) with zero rows when a document has no scoring page.
docx_scoring_rows <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  txt <- unescape_xml(gsub("<[^>]+>", "", runs))
  hdr <- which(txt == "Items")
  empty <- data.frame(
    scale = character(0),
    items = character(0),
    stringsAsFactors = FALSE
  )
  if (length(hdr) == 0L) return(empty)
  body <- txt[seq_len(length(txt) - max(hdr)) + max(hdr)]
  n <- 2L * (length(body) %/% 2L)
  if (n == 0L) return(empty)
  data.frame(
    scale = body[seq(1L, n, by = 2L)],
    items = body[seq(2L, n, by = 2L)],
    stringsAsFactors = FALSE
  )
}

# Extract the shuffled-form crosswalk's (new, original) pairs from a .docx.
#
# generate_docx_hitopsr() prints the crosswalk as one run of arrow-joined
# pairs ahead of the scoring table. The arrow is what scopes this: the legend
# runs read "0 = Never", item rows read "12.  I ...", and the scoring table's
# Items cells read "1, 2(R), 3", so none of them can match. Returns a
# data.frame(new, original) of integers, with zero rows when a document
# carries no crosswalk.
docx_crosswalk_pairs <- function(file) {
  xml <- read_docx_xml(file)
  runs <- regmatches(xml, gregexpr("<w:t[^>]*>[^<]*</w:t>", xml))[[1]]
  txt <- unescape_xml(gsub("<[^>]+>", "", runs))
  hit <- grep("^[0-9]+ \u2192 [0-9]+", txt, value = TRUE)
  if (length(hit) == 0L) {
    return(data.frame(
      new = integer(0),
      original = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  pairs <- strsplit(hit[[1]], ", ", fixed = TRUE)[[1]]
  parts <- do.call(rbind, strsplit(pairs, " \u2192 ", fixed = TRUE))
  data.frame(
    new = as.integer(parts[, 1]),
    original = as.integer(parts[, 2]),
    stringsAsFactors = FALSE
  )
}

# Extract the (width, height) of the first <w:pgSz> in twips.
docx_page_size <- function(xml) {
  w <- as.integer(sub('.*<w:pgSz[^>]*w:w="([0-9]+)".*', "\\1", xml))
  h <- as.integer(sub('.*<w:pgSz[^>]*w:h="([0-9]+)".*', "\\1", xml))
  c(width = w, height = h)
}

# ---- Shared skip guards -----------------------------------------------------
#
# zip WRITING needs an external zip utility (unzip is internal to R);
# officer/flextable are Imports, but the guard keeps local runs graceful when
# they are absent.


skip_if_no_docx <- function() {
  testthat::skip_if_not_installed("officer")
  testthat::skip_if_not_installed("flextable")
}
