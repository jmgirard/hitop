# Shared rendering helpers for the instrument download pages
# (vignettes/articles/download-*.Rmd). Sourced by each page's setup chunk;
# not part of the package namespace. Styling lives in pkgdown/extra.css.
#
# All version information is drawn from the hitop_artifacts manifest at
# render time (D-016: the manifest is the single source of truth); pages
# carry their artifact URLs explicitly so the download-page href lock in
# tests/testthat/test-artifacts.R keeps scanning page sources.

# Emit HTML wrapped in a pandoc raw-html fence so markdown inside the
# emitted text (e.g. "_1.0_" in manifest change notes) is never re-parsed.
.emit_html <- function(...) {
  cat("\n```{=html}\n", ..., "\n```\n", sep = "")
}

.esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

.manifest <- function(instrument) {
  m <- hitop::hitop_artifacts
  m <- m[m$instrument == instrument, , drop = FALSE]
  m[order(m$file, m$build_date), , drop = FALSE]
}

.current_builds <- function(instrument) {
  m <- .manifest(instrument)
  m[!duplicated(m$file, fromLast = TRUE), , drop = FALSE]
}

.format_label <- function(x) {
  labels <- c(
    docx_a4 = "DOCX (A4 paper)",
    docx_us = "DOCX (US paper)",
    qualtrics = "Qualtrics import",
    redcap = "REDCap import"
  )
  out <- unname(labels[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

.badge <- function(date) {
  sprintf(
    '<span class="hitop-build-badge" title="Build date">%s</span>',
    .esc(date)
  )
}

# A download link backed by a manifest-tracked artifact (gets a build badge).
dl_link <- function(label, href) {
  list(label = label, href = href, artifact = TRUE)
}

# A plain reference link (e.g. import instructions); no badge.
ref_link <- function(label, href) {
  list(label = label, href = href, artifact = FALSE)
}

# One download card: an emoji icon, a title, a short description, and links.
dl_card <- function(icon, title, desc, ...) {
  list(icon = icon, title = title, desc = desc, links = list(...))
}

# Render the "Ready-to-Use Downloads" card row. Build-date badges are looked
# up in the manifest by the basename of each dl_link() href; a dl_link whose
# file is missing from the manifest is an error (stale page or manifest).
download_cards <- function(instrument, cards) {
  current <- .current_builds(instrument)
  dates <- stats::setNames(current$build_date, current$file)

  render_button <- function(link) {
    file <- basename(link$href)
    if (is.na(dates[file])) {
      stop("No hitop_artifacts row for '", file, "' (", instrument, ")")
    }
    sprintf(
      '<a href="%s" class="btn btn-outline-primary hitop-dl-btn"><span>%s</span>%s</a>',
      .esc(link$href), .esc(link$label), .badge(dates[[file]])
    )
  }

  # ref_link()s render as quiet text links under the download buttons,
  # not as peer buttons (they are supporting material, not downloads).
  render_ref <- function(link) {
    sprintf(
      '<div class="text-center mt-2"><a href="%s" class="hitop-ref-link small">%s</a></div>',
      .esc(link$href), .esc(link$label)
    )
  }

  render_card <- function(card) {
    is_artifact <- vapply(card$links, function(l) isTRUE(l$artifact), logical(1))
    buttons <- vapply(card$links[is_artifact], render_button, character(1))
    refs <- vapply(card$links[!is_artifact], render_ref, character(1))
    sprintf(
      paste0(
        '<div class="col-md-4 mb-4">\n',
        '<div class="card h-100 shadow-sm">\n',
        '<div class="card-body">\n',
        '<h5 class="card-title">%s %s</h5>\n',
        '<p class="card-text text-muted">%s</p>\n',
        '<div class="d-grid gap-2">\n%s\n</div>\n%s',
        '</div>\n</div>\n</div>'
      ),
      card$icon, .esc(card$title), .esc(card$desc),
      paste(buttons, collapse = "\n"),
      if (length(refs)) paste0(paste(refs, collapse = "\n"), "\n") else ""
    )
  }

  .emit_html(
    '<div class="row mt-4 hitop-downloads">\n',
    paste(vapply(cards, render_card, character(1)), collapse = "\n"),
    "\n</div>"
  )
}

# Render the "Versions" section: a one-line explanation plus a collapsible
# panel holding the current-builds manifest and the full version history.
versions_section <- function(instrument) {
  current <- .current_builds(instrument)
  history <- .manifest(instrument)
  history <- history[order(history$build_date, decreasing = TRUE), , drop = FALSE]

  current_rows <- sprintf(
    "<tr><td><code>%s</code></td><td>%s</td><td>%s</td><td>%s</td></tr>",
    .esc(current$file),
    .esc(.format_label(current$format)),
    .esc(current$instrument_version),
    vapply(current$build_date, .badge, character(1))
  )

  # Group history rows sharing a build date and change description.
  key <- paste(history$build_date, history$changes, sep = "\r")
  groups <- split(history, factor(key, levels = unique(key)))
  history_entries <- vapply(groups, function(g) {
    sprintf(
      paste0(
        '<div class="hitop-history-entry">%s<div>%s<br>',
        '<span class="hitop-history-files">%s</span></div></div>'
      ),
      .badge(g$build_date[1]),
      .esc(g$changes[1]),
      paste0("<code>", .esc(g$file), "</code>", collapse = ", ")
    )
  }, character(1))

  .emit_html(
    '<h3 class="mt-4 mb-3">Versions</h3>\n',
    '<p class="text-muted">Every download button above shows its file’s ',
    "build date; a new build date means the distributed file changed. ",
    "The instrument itself is version ",
    .esc(current$instrument_version[1]),
    ".</p>\n",
    '<details class="hitop-versions">\n',
    "<summary>Current builds &amp; version history</summary>\n",
    '<div class="hitop-versions-body">\n',
    "<h4>Current builds</h4>\n",
    '<div class="table-responsive">\n',
    '<table class="table table-sm">\n',
    "<thead><tr><th>File</th><th>Format</th><th>Instrument version</th>",
    "<th>Build date</th></tr></thead>\n",
    "<tbody>\n", paste(current_rows, collapse = "\n"), "\n</tbody>\n",
    "</table>\n</div>\n",
    '<p class="text-muted small">If your downloaded file shows an older ',
    "build date, simply re-download it to get the latest build. The full ",
    "build manifest (including file checksums) ships in the package as ",
    "<code>hitop_artifacts</code>.</p>\n",
    "<h4>Version history</h4>\n",
    paste(history_entries, collapse = "\n"), "\n",
    "</div>\n</details>"
  )
}
