# Table 1 of the HiTOP-SR introduction paper, extracted from the shelf PDF.
#
# Sourced by data-raw/verify_hitopsr_scale_name.R (which compares two committed
# scale names against their Table 1 cells) and by data-raw/verify_hitopsr_names.R
# (which reconciles every label the table prints against the two shipped name
# tables). It lives in its own file so neither verifier owns the extraction: the
# extraction never sees a committed name, which is what keeps both comparisons
# from being circular (IP2).
#
# Maintainer-run, never CI: it needs the gitignored source shelf and pdftotext.

hitopsr_source_pdf <- "cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf"

## The document D-042 admits as this package's source for HiTOP-SR scale names,
## pinned by content rather than by filename -- the shelf is gitignored, so a
## future run could otherwise be reading a different manuscript at this path.
hitopsr_source_sha256 <-
  "1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425"

# Check the preconditions and return the shelf copy's sha256. Stops rather than
# reports: a missing shelf, a missing pdftotext and a substituted document are
# all "this run proves nothing", and a printed report must never be mistakable
# for a pass.
hitopsr_source_check <- function(pdf = hitopsr_source_pdf) {
  if (!file.exists(pdf)) {
    stop("source not on the shelf: ", pdf, call. = FALSE)
  }
  if (nchar(Sys.which("pdftotext")) == 0L) {
    stop("pdftotext not found; install poppler", call. = FALSE)
  }
  sha <- sub(
    " .*$", "",
    system2("shasum", c("-a", "256", shQuote(pdf)), stdout = TRUE)
  )
  if (!identical(sha, hitopsr_source_sha256)) {
    stop("shelf PDF is not the document D-042 admits\n",
         "  expected: ", hitopsr_source_sha256, "\n  found:    ", sha,
         call. = FALSE)
  }
  sha
}

# The rows of Table 1, as a data frame of label / page / shape.
#
# Three things make the extraction more than a line split.
#
#   1. The proof is stamped with a rotated "For Peer Review" watermark, which
#      pdftotext interleaves into the text as the fragments Fo / rP / ee / rR /
#      ev / iew -- sometimes standing alone in a column gap ("Fo    3"), which
#      would otherwise read as a row labelled "Fo", and sometimes glued to a
#      number ("0.86rR", "ee0.67"). They are removed before anything else.
#
#   2. Rows come in two shapes. Most extract with the label and its numeric
#      cells on one line ("Appearance Focus   5   0.81 ..."); where the
#      watermark splits the layout, a run of labels extracts on lines of their
#      own with the numeric cells following on separate lines ("Non-suicidal
#      Self-injury"). Both shapes are kept; the shape is reported so a caller
#      can show that it exercised each.
#
#   3. The manuscript numbers its own lines in the left margin, and the table
#      is bracketed by running heads, the caption, the column header, the Note
#      and a footnote. Those are stripped by position and by pattern, never by
#      matching a scale name.
#
# The table's own section headers ("Somatoform Scales", "Superspectra and
# Spectra Scales", ...) are separated out rather than dropped, so a caller can
# see that the section structure was recognized instead of silently eaten.
# The pages Table 1 occupies, as a 1-based page index into the source. Located by
# the table captions rather than by a pinned page number, so a repaginated source
# moves the range instead of silently reading the wrong pages. Both extractors
# below take their range from here, so neither can read a page the other does not.
hitopsr_table1_pages <- function(pdf = hitopsr_source_pdf) {
  txt <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
  pages <- split(txt, cumsum(grepl("\f", txt)))
  has <- function(p, re) any(grepl(re, p))

  first <- which(vapply(pages, has, logical(1), "^ *[0-9]* *Table 1\\."))[1]
  after <- which(vapply(pages, has, logical(1), "^ *[0-9]* *Table 2\\."))[1]
  if (is.na(first) || is.na(after) || after <= first) {
    stop("could not locate Table 1's page range in the source", call. = FALSE)
  }
  seq(first, after - 1L)
}

## The rotated watermark, as the fragments pdftotext interleaves it in as. Both
## extractors below read this one list: `hitopsr_table1_rows()` strips the
## fragments out of the linearized dump, and `hitopsr_table1_cells()` decides
## through the predicate below whether a digitless token sitting in the numeric
## columns is a piece of the watermark or a cell the extraction is losing.
hitopsr_table1_watermark_fragments <- c("Fo", "rP", "ee", "rR", "ev", "iew")

## The eight scales of Table 1's last section, as the table prints them. The
## partition below reads this list rather than counting rows after the section
## header: a source revision that moved a row to the other side of that header
## would leave both block counts intact, so the positional split cannot see it.
## These are the table's own labels, never a committed name -- `Externalizing`
## and the rest are HiTOP-BR scales, and this extraction stays blind to what
## `hitopbr_scales` ships (IP2).
##
## `data-raw/verify_hitopsr_names.R` pins the same eight names again. That copy
## is deliberate and must not be collapsed into this one: it is that script's
## independent record of what M059 measured, and reading this vector instead
## would make its check a comparison against the list that produced the answer.
hitopsr_table1_superspectra_scales <- c(
  "Externalizing", "p-factor", "Internalizing", "Somatoform",
  "Detachment", "Thought Disorder", "Disinhibition", "Antagonism"
)

## A token is watermark when it is part of the phrase those fragments spell. The
## phrase is built from the vector above rather than written out a second time,
## so a source whose watermark changes moves the stripping step and this test
## together. The two join the vector differently -- the stripping step with
## `collapse = "|"` as a regex alternation, this test with `collapse = ""` as a
## literal phrase -- so that coupling holds only while the vector spells the
## watermark in order, with no fragment overlapping another and no regex
## metacharacter in any of them. A new source's fragments are listed under that
## rule or the two consumers drift apart.
##
## The accepted set reaches down to single letters, which reads loose and is not.
## `-bbox` breaks the rotated stamp wherever its own boxes fall, so the numeric
## columns of the three table pages carry it as "r P ee r R ev" (page 49),
## "r P ee ev" (50) and "r P ee" (51) -- read down each page, the stamp. Nor can
## a single letter be a cell this extraction is losing: `cells` above is defined
## as the tokens carrying a digit, so every token reaching here is by
## construction not one. What the test does reject is prose at these
## coordinates -- a label that overflowed the cut, a caption runover, a footnote
## -- which is the banding failure worth stopping on.
hitopsr_table1_is_watermark <- function(token) {
  phrase <- paste(hitopsr_table1_watermark_fragments, collapse = "")
  vapply(token, function(t) grepl(t, phrase, fixed = TRUE),
         logical(1), USE.NAMES = FALSE)
}

hitopsr_table1_rows <- function(pdf = hitopsr_source_pdf) {
  txt <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
  pages <- split(txt, cumsum(grepl("\f", txt)))
  idx <- hitopsr_table1_pages(pdf)

  page_no <- rep(idx, vapply(pages[idx], length, integer(1)))
  line <- gsub("\f", "", unlist(pages[idx], use.names = FALSE))

  ## 1. The watermark.
  wm <- paste(hitopsr_table1_watermark_fragments, collapse = "|")
  line <- gsub(paste0("(?<=[0-9])(", wm, ")"), "", line, perl = TRUE)
  line <- gsub(paste0("(", wm, ")(?=[0-9])"), "", line, perl = TRUE)
  line <- gsub(paste0("(?<![A-Za-z])(", wm, ")(?![A-Za-z])"), "", line, perl = TRUE)

  ## 3a. The manuscript's left-margin line numbers, and blank lines.
  line <- sub("^ *[0-9]{1,2}(?= {2,}| *$)", "", line, perl = TRUE)
  line <- sub("[ \t]+$", "", line)
  keep <- nzchar(trimws(line))
  line <- line[keep]
  page_no <- page_no[keep]

  ## 3b. The table body ends at the Note, which is followed by the dagger
  ## footnote. Everything after it is prose about the table, not a row.
  note <- grep("^ *Note\\.", line)
  if (length(note)) {
    line <- line[seq_len(note[1] - 1L)]
    page_no <- page_no[seq_len(note[1] - 1L)]
  }

  ## 3c. Running heads, the caption and its runover, and the column header.
  furniture <- grepl(paste0(
    "manuscriptcentral|^ *Assessment *$|Page [0-9]+ of [0-9]+|",
    "Assessing the HiTOP Model|^ *Table 1\\.|",
    "Superspectra and Spectra Scales in|^ *Scale / Subscale"
  ), line)
  line <- line[!furniture]
  page_no <- page_no[!furniture]

  ## A row must carry a letter; a cells-only continuation line does not.
  lettered <- grepl("[A-Za-z]", line)
  line <- line[lettered]
  page_no <- page_no[lettered]

  ## 2. Split the label from the numeric cells. A cell block starts at the
  ## first run of two or more spaces followed by a number, a minus sign or the
  ## Range column's opening bracket; no scale label contains one.
  label <- trimws(sub("(?<= {2})-?[0-9\\[].*$", "", line, perl = TRUE))
  shape <- ifelse(label == trimws(line), "label-only", "with-cells")

  out <- data.frame(
    label = label,
    page = page_no,
    shape = shape,
    section = grepl("\\bScales\\b", label),
    stringsAsFactors = FALSE
  )

  ## The domain this whole family quantifies over is the extracted table. A
  ## pdftotext or pagination change could empty it or drop a shape silently,
  ## and every caller's report would then pass by having nothing to compare.
  if (nrow(out) == 0L) {
    stop("Table 1 extracted no rows", call. = FALSE)
  }
  if (!all(c("label-only", "with-cells") %in% out$shape)) {
    stop("Table 1 extracted only one row shape (", 
         paste(unique(out$shape), collapse = ", "),
         "); the extraction no longer exercises both", call. = FALSE)
  }
  if (!any(out$section)) {
    stop("Table 1 extracted no section headers", call. = FALSE)
  }
  out
}

# The cells of Table 1, read from the words' page coordinates rather than from a
# linearized text dump.
#
# `hitopsr_table1_rows()` above reads labels out of `pdftotext -layout`, which is
# enough for a name comparison. The numeric cells need more: on the two page
# blocks the watermark splits, that dump emits the block column-major -- a run of
# labels, then a run of item counts, then a run of alphas -- so pairing a label
# with its own cells means guessing how this particular proof happens to break.
# The words' coordinates do not have that problem. Every word carries its page
# box, so a row is the words sharing a vertical band and a column is a horizontal
# one, which reads the split blocks and the clean ones through the same code.
#
# It also recovers what the layout dump throws away: Table 1 marks a subscale
# only by indenting its label, so the label's left edge is the source's own
# statement of whether a row is a scale or a subscale.
#
# Returns one row per data row of the table, in printed order:
#   label, indent, page, nItems, alpha, mean, sd, rangeLo, rangeHi,
#   skewness, kurtosis, block
# where `block` is "primary" for the 93 primary-scale and subscale rows and
# "superspectra" for the 8 HiTOP-BR rows of the table's last section.
#
# Maintainer-run, never CI: it needs the gitignored source shelf and pdftotext.

## Where the columns fall on the page, in PDF points. The manuscript's own
## left-margin line numbers sit at x ~ 8; every label starts at x = 43 (a scale)
## or x = 61 (a subscale); the first numeric column starts at x ~ 211. The two
## cuts below sit in the empty space between those groups.
hitopsr_table1_margin_x <- 15
hitopsr_table1_cell_x <- 175

## `xml` is the `pdftotext -bbox` dump the extraction reads, defaulted to the
## one this run produces. It is an argument so that a control can feed in a
## mutated extraction -- rows moved, a header renamed -- and see the guards
## below stop on it; production callers pass nothing and read the source.
hitopsr_table1_cells <- function(pdf = hitopsr_source_pdf, xml = NULL) {
  idx <- hitopsr_table1_pages(pdf)
  if (is.null(xml)) {
    xml <- system2(
      "pdftotext",
      c("-bbox", "-f", idx[1], "-l", idx[length(idx)], shQuote(pdf), "-"),
      stdout = TRUE
    )
  }
  is_word <- grepl("<word ", xml)
  if (!any(is_word)) {
    stop("pdftotext -bbox returned no words for Table 1's pages", call. = FALSE)
  }
  w <- xml[is_word]
  coord <- function(attr) {
    as.numeric(sub(paste0('.*', attr, '="([0-9.-]+)".*'), "\\1", w))
  }
  word <- data.frame(
    page = cumsum(grepl("<page ", xml))[is_word] + idx[1] - 1L,
    x = coord("xMin"),
    y = coord("yMin"),
    text = sub(".*>(.*)</word>", "\\1", w),
    stringsAsFactors = FALSE
  )

  ## The manuscript's left-margin line numbers.
  word <- word[word$x >= hitopsr_table1_margin_x, ]

  ## Band the words into rows. Successive printed rows are ~12 points apart and
  ## the words of one row share a baseline to within a point, so any cut between
  ## those two figures separates rows; 5 is used. A band that merged two rows
  ## would carry 16 cells and be rejected by the shape check below.
  bands <- list()
  for (p in unique(word$page)) {
    q <- word[word$page == p, ]
    q <- q[order(q$y, q$x), ]
    for (b in split(q, cumsum(c(1, diff(q$y) > 5)))) {
      label_part <- b[b$x < hitopsr_table1_cell_x, ]
      right <- b$text[b$x >= hitopsr_table1_cell_x]
      if (nrow(label_part) == 0L) next
      ## Every cell of the numeric columns carries a digit -- a count, a decimal,
      ## a minus sign or one half of the Range column's bracketed pair -- so the
      ## digitless tokens there are the rotated "For Peer Review" watermark, which
      ## pdftotext interleaves into the same coordinates.
      bands[[length(bands) + 1L]] <- list(
        page = p,
        label = paste(label_part$text, collapse = " "),
        indent = min(label_part$x),
        cells = right[grepl("[0-9]", right)],
        digitless = right[!grepl("[0-9]", right)]
      )
    }
  }

  ## A data row prints seven numeric columns, of which Range is two tokens
  ## ("[1.0," and "4.0]"), so exactly eight. Every other band -- the caption, the
  ## column header, a section header, the Note, the footnote, a running head --
  ## carries none. Anything in between is a row the split did not resolve, and it
  ## is reported rather than dropped.
  ## Strip the furniture, by the same patterns `hitopsr_table1_rows()` uses and
  ## never by matching a scale name: the running heads, the caption and its
  ## runover, the column header, and -- from the Note onward -- the prose about
  ## the table. Matched against the whole band, label and numeric columns
  ## together, because the caption's runover puts half of itself in each.
  full <- vapply(bands, function(b) {
    trimws(paste(c(b$label, b$digitless, b$cells), collapse = " "))
  }, character(1))
  note <- which(grepl("^Note\\.", full))
  if (length(note)) {
    keep <- seq_len(note[1] - 1L)
    bands <- bands[keep]
    full <- full[keep]
  }
  furniture <- grepl(paste0(
    "manuscriptcentral|^Assessment$|Page [0-9]+ of [0-9]+|",
    "Assessing the HiTOP Model|^Table 1\\.|",
    "Superspectra and Spectra Scales in|^Scale / Subscale"
  ), full)
  bands <- bands[!furniture]

  ## A data row prints seven numeric columns, of which Range is two tokens
  ## ("[1.0," and "4.0]"), so exactly eight; a section header has none. With the
  ## furniture gone nothing else remains, so a band carrying some other count is
  ## a row the banding failed to resolve, and it is reported rather than dropped.
  n_cells <- vapply(bands, function(b) length(b$cells), integer(1))
  odd <- n_cells != 0L & n_cells != 8L
  if (any(odd)) {
    stop("Table 1 bands with a partial cell block: ",
         paste(vapply(bands[odd], function(b) b$label, character(1)),
               collapse = " | "),
         call. = FALSE)
  }

  ## On a data row, the only thing that may be discarded from the numeric columns
  ## is a piece of the watermark phrase. Anything else there is a cell the
  ## extraction would be losing in silence, so the run stops instead of diffing a
  ## short row. Checked on the data rows only: the caption, the Note and the
  ## running heads have prose at those coordinates and no cells at all.
  stray <- unlist(lapply(bands[n_cells == 8L], function(b) {
    b$digitless[!hitopsr_table1_is_watermark(b$digitless)]
  }))
  if (length(stray)) {
    stop("non-watermark text in Table 1's numeric columns: ",
         paste(unique(stray), collapse = ", "), call. = FALSE)
  }

  ## The table's last section is the eight HiTOP-BR scales. It is separated
  ## rather than dropped, so a caller sees the partition instead of a silently
  ## shorter table. Which rows belong to it is read off the pinned scale names
  ## above; the section header is still located, but only so the two statements
  ## of the partition can be compared.
  header <- which(n_cells == 0L & grepl("^Superspectra and Spectra Scales$",
                                        vapply(bands, function(b) b$label,
                                               character(1))))
  if (length(header) != 1L) {
    stop("Table 1's Superspectra and Spectra section header was not found once",
         call. = FALSE)
  }

  data_at <- which(n_cells == 8L)
  data_label <- vapply(bands[data_at], function(b) b$label, character(1))
  by_name <- data_label %in% hitopsr_table1_superspectra_scales
  by_header <- data_at > header

  ## Both ways of stating the partition have to agree. A pinned name that
  ## reached no data row, or a row whose name and printed position disagree, is
  ## a revision a person adjudicates -- not something a run absorbs by counting
  ## eight rows after the header and calling them the right eight.
  absent <- setdiff(hitopsr_table1_superspectra_scales, data_label)
  if (length(absent)) {
    stop("Table 1's Superspectra partition could not place: ",
         paste(absent, collapse = " | "),
         " -- pinned as a Superspectra scale, extracted as no data row",
         call. = FALSE)
  }
  displaced <- which(by_name != by_header)
  if (length(displaced)) {
    stop("Table 1's Superspectra partition could not place: ",
         paste(data_label[displaced], collapse = " | "),
         " -- the pinned scale names and the section header disagree on which ",
         "block each row belongs to", call. = FALSE)
  }

  out <- do.call(rbind, lapply(seq_along(data_at), function(k) {
    b <- bands[[data_at[k]]]
    num <- function(k) as.numeric(gsub("[^0-9.-]", "", b$cells[[k]]))
    data.frame(
      label = b$label,
      indent = b$indent,
      page = b$page,
      nItems = as.integer(num(1)),
      alpha = num(2),
      mean = num(3),
      sd = num(4),
      rangeLo = num(5),
      rangeHi = num(6),
      skewness = num(7),
      kurtosis = num(8),
      block = if (by_name[k]) "superspectra" else "primary",
      stringsAsFactors = FALSE
    )
  }))

  ## The domain this extraction quantifies over is the table itself. A pdftotext
  ## or pagination change could empty it, lose a section, or collapse the two
  ## indent levels, and every caller's diff would then pass by comparing nothing.
  if (nrow(out) != 101L) {
    stop("Table 1 extracted ", nrow(out), " data rows, expected 101",
         call. = FALSE)
  }
  if (sum(out$block == "primary") != 93L) {
    stop("Table 1 extracted ", sum(out$block == "primary"),
         " primary-scale and subscale rows, expected 93", call. = FALSE)
  }
  if (length(unique(round(out$indent))) < 2L) {
    stop("Table 1's label indentation resolved to one level, so the extraction ",
         "no longer distinguishes a subscale from a scale", call. = FALSE)
  }
  if (any(is.na(out[c("nItems", "alpha", "mean", "sd")]))) {
    stop("Table 1 extracted a non-numeric cell in a shipped column",
         call. = FALSE)
  }
  out
}

# Which rows Table 1 indents. The table has exactly two label positions, so the
# split is the midpoint between them rather than a pinned coordinate.
hitopsr_table1_is_subscale <- function(cells) {
  levels <- sort(unique(round(cells$indent)))
  cells$indent > (min(levels) + max(levels)) / 2
}
