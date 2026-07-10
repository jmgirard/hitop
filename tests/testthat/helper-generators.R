# Parsers for the generate_* export-family tests (milestone M10).
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

# Extract the (width, height) of the first <w:pgSz> in twips.
docx_page_size <- function(xml) {
  w <- as.integer(sub('.*<w:pgSz[^>]*w:w="([0-9]+)".*', "\\1", xml))
  h <- as.integer(sub('.*<w:pgSz[^>]*w:h="([0-9]+)".*', "\\1", xml))
  c(width = w, height = h)
}
