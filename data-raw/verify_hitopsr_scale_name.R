# Verify two committed HiTOP-SR scale names against the published source
# (M058 AC1, generalized by M059 AC3)
#
# The HiTOP-SR introduction paper is the naming authority for a HiTOP-SR scale
# (D-018's per-content-type rule; the allowance in D-041, widened to this
# document's scale names by D-042). This script reads that document from the
# gitignored source shelf and does two things the milestone files cannot do for
# themselves:
#
#   1. Inventories every rendering of each name the paper prints, with the page
#      of each. For one of the two scales the paper disagrees with itself -- its
#      tables and its prose capitalize differently -- and IP1 requires such a
#      discrepancy to stay visible rather than be silently resolved, so the
#      inventory is regenerated here rather than transcribed into
#      cairn/SOURCES.md by hand. The OQ-3 entry in that file quotes this
#      script's output.
#
#   2. Compares each string committed in data-raw/hitopsr_items.csv character
#      for character against the label Table 1 prints. The comparison reads the
#      source at run time; it never compares one transcription against another,
#      which is why neither milestone file nor SOURCES.md is an input here.
#
# Two things changed at M059, when a second scale was renamed against the same
# source. The committed side is now identified by each scale's pinned item
# numbers rather than by grepping item text, because a sanctioned item-text
# edit would break a text grep while leaving the keying correct (M058 finding
# 12). And the source side no longer assumes a name stands alone on its
# extracted line: that holds for `Non-suicidal Self-injury`, whose Table 1 row
# is split from its numeric cells by the review watermark, but not for
# `Appearance Focus`, whose row extracts with its cells on the same line. Both
# shapes are handled by data-raw/hitopsr_table1.R, which does the extraction
# without ever seeing a committed name.
#
# Maintainer-run, never CI: it needs both the shelf PDF (gitignored) and
# pdftotext. Exits non-zero on any mismatch, so a printed report cannot be
# mistaken for a pass.

source("data-raw/hitopsr_table1.R")

## The scales this package has renamed against this source, each pinned by its
## item numbers. Item numbers, not names: the name is what is under test, so
## naming the scale here would make the comparison circular.
pinned <- list(
  list(milestone = "M058", items = c(46L, 215L, 235L, 298L, 387L, 404L)),
  list(milestone = "M059", items = c(16L, 79L, 201L, 335L, 350L))
)

items <- read.csv("data-raw/hitopsr_items.csv", stringsAsFactors = FALSE)

for (i in seq_along(pinned)) {
  hit <- items$Scale[items$HSR %in% pinned[[i]]$items]
  stopifnot(
    "a pinned item number is not in data-raw/hitopsr_items.csv" =
      length(hit) == length(pinned[[i]]$items),
    "the pinned items no longer identify a single scale" =
      length(unique(hit)) == 1L
  )
  pinned[[i]]$committed <- unique(hit)
}

sha <- hitopsr_source_check()
rows <- hitopsr_table1_rows()

txt <- system2(
  "pdftotext",
  c("-layout", shQuote(hitopsr_source_pdf), "-"),
  stdout = TRUE
)
pages <- split(txt, cumsum(grepl("\f", txt)))

## Per page, collapsed to one string. The proof interleaves the manuscript's
## line numbers into the text, so a hyphenated name broken across a typeset
## line arrives as "Self-" + a line number + the remainder; that noise is
## removed, ordinary column wraps are not (they are reported separately below).
flat <- vapply(pages, function(p) {
  x <- gsub("\f", " ", paste(p, collapse = " "))
  x <- gsub("[ \t]+", " ", x)
  gsub("([A-Za-z])-( ?[0-9]+ ?)+ ?", "\\1-", x)
}, character(1))

## A rendering pattern built from the committed name: each letter matched in
## either case, each hyphen matched with or without its hyphen, and a trailing
## noun allowed to pluralize. It is a family of spellings, not the committed
## string -- the point of the inventory is to find the renderings that differ
## from what is committed.
rendering_pattern <- function(name) {
  esc <- gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", name, perl = TRUE)
  esc <- gsub("-", "-?", esc, fixed = TRUE)
  esc <- gsub("([A-Za-z])", "[\\L\\1\\U\\1]", esc, perl = TRUE)
  paste0(esc, "(y|ies)?")
}

cat("Source: ", hitopsr_source_pdf, "\n", sep = "")
cat("sha256: ", sha, " (matches)\n", sep = "")
cat("Table 1 extracted ", nrow(rows), " rows (",
    sum(rows$section), " section headers, ",
    sum(!rows$section), " labels; ",
    sum(rows$shape == "with-cells"), " with their numeric cells on the same ",
    "line, ", sum(rows$shape == "label-only"), " with the label alone).\n",
    sep = "")

bad <- character(0)

for (p in pinned) {
  committed <- p$committed
  cat("\n----- ", p$milestone, ": committed name ",
      encodeString(committed, quote = '"'), " -----\n", sep = "")

  pattern <- rendering_pattern(committed)
  found <- do.call(rbind, lapply(seq_along(flat), function(i) {
    m <- regmatches(flat[[i]], gregexpr(pattern, flat[[i]], perl = TRUE))[[1]]
    if (length(m)) data.frame(page = i, variant = m, stringsAsFactors = FALSE)
  }))

  ## A cell that wraps mid-name inside a table puts the two halves either side
  ## of a neighbouring column's text, so the pattern above cannot span it.
  ## Those pages are reported rather than counted, so the inventory never
  ## silently undercounts.
  ##
  ## A name can break at any of its internal hyphens or spaces, and the two
  ## breaks leave different evidence. A hyphen break keeps the hyphen on the
  ## first half, so the halves need only both appear on the page. A space break
  ## leaves nothing distinctive, so requiring the halves to merely co-occur
  ## would flag any page using both words in ordinary prose -- checked on this
  ## document, where p. 66 uses "focus" and "appearances" in two unrelated
  ## definitions. A space break is therefore recognized only where one
  ## extracted line ends with the first half and a later one begins with the
  ## second, which is what a wrapped table cell actually looks like.
  breaks <- gregexpr("[ -]", committed)[[1]]
  wrapped <- integer(0)
  for (b in breaks) {
    hyphen <- substr(committed, b, b) == "-"
    lhs <- substr(committed, 1L, if (hyphen) b else b - 1L)
    rhs <- substr(committed, b + 1L, nchar(committed))
    lhs_pat <- rendering_pattern(lhs)
    rhs_pat <- rendering_pattern(rhs)
    for (i in seq_along(pages)) {
      if (i %in% found$page || i %in% wrapped) next
      if (hyphen) {
        if (grepl(lhs_pat, flat[[i]], perl = TRUE) &&
            grepl(rhs_pat, flat[[i]], perl = TRUE)) {
          wrapped <- c(wrapped, i)
        }
      } else {
        ends <- grep(paste0(lhs_pat, " *$"), pages[[i]], perl = TRUE)
        starts <- grep(paste0("^ *", rhs_pat), pages[[i]], perl = TRUE)
        if (length(ends) && any(starts > min(ends))) wrapped <- c(wrapped, i)
      }
    }
  }
  wrapped <- sort(unique(wrapped))

  cat("Renderings printed by the paper, by page:\n")
  if (is.null(found)) {
    cat("  (none)\n")
  } else {
    tab <- table(found$variant)
    for (v in names(tab)) {
      pp <- sort(unique(found$page[found$variant == v]))
      cat(sprintf("  %-26s %d occurrence%s on page%s %s\n",
                  v, tab[[v]], if (tab[[v]] == 1) "" else "s",
                  if (length(pp) == 1) "" else "s", paste(pp, collapse = ", ")))
    }
  }
  if (length(wrapped)) {
    cat(sprintf("  %-26s page%s %s (name wraps across a table cell; read by eye)\n",
                "[wrapped occurrence]", if (length(wrapped) == 1) "" else "s",
                paste(wrapped, collapse = ", ")))
  }

  ## The comparison. hitopsr_table1_rows() extracted every label the table
  ## prints without consulting any committed name, so asking whether the
  ## committed string is among them is a comparison against the source and not
  ## against a second transcription of it.
  match_rows <- rows[!rows$section & rows$label == committed, , drop = FALSE]
  cat("Table 1 label matching it character for character: ",
      if (nrow(match_rows)) {
        paste(sprintf("%s (p. %d, %s row)",
                      match_rows$label, match_rows$page, match_rows$shape),
              collapse = " / ")
      } else {
        "(none)"
      },
      "\n", sep = "")

  if (nrow(match_rows) != 1L) {
    near <- rows$label[
      !rows$section &
        tolower(gsub("[^A-Za-z]", "", rows$label)) ==
          tolower(gsub("[^A-Za-z]", "", committed))
    ]
    bad <- c(bad, sprintf(
      "%s: expected one Table 1 label equal to %s, found %d%s",
      p$milestone, encodeString(committed, quote = '"'), nrow(match_rows),
      if (length(near)) {
        paste0(" (differing only in case or punctuation: ",
               paste(encodeString(near, quote = '"'), collapse = ", "), ")")
      } else {
        ""
      }
    ))
  }
}

## Both row shapes must have been exercised by the pinned scales themselves,
## not merely by the table as a whole: a change that made every pinned scale
## extract the same way would leave one branch of the comparison untested and
## still report a clean pass.
exercised <- unique(rows$shape[
  !rows$section & rows$label %in% vapply(pinned, `[[`, character(1), "committed")
])
cat("\nRow shapes exercised by the pinned scales: ",
    paste(sort(exercised), collapse = ", "), "\n", sep = "")
if (!all(c("label-only", "with-cells") %in% exercised)) {
  bad <- c(bad, paste0(
    "the pinned scales exercise only the ", paste(exercised, collapse = ", "),
    " row shape; M059's AC3 requires both"
  ))
}

cat("\n")
if (!length(bad)) {
  cat("RESULT: every committed scale name matches its source label character ",
      "for character.\n", sep = "")
} else {
  cat("RESULT: ", length(bad), " discrepancy/discrepancies\n\n", sep = "")
  for (b in bad) cat("  - ", b, "\n", sep = "")
  stop(length(bad), " discrepancy/discrepancies against the source", call. = FALSE)
}
