## Regenerate the committed distribution artifacts in inst/extdata/ and
## update the hitop_artifacts manifest (D-016).
##
## Usage: set `rebuild_stems` to the instrument(s) whose content changed and
## `rebuild_formats` to the affected output format(s) (either NULL for all),
## edit `build_notes` to describe what changed in this build, then source the
## whole script from the package root. The selected
## artifacts are rebuilt from the current keying tables; the manifest gains a
## new row for each file whose checksum changed (the full row history is kept,
## so the manifest doubles as a changelog). Both settings are left as the last
## build ran them, so the script records what it last did.
##
## The HSUM Qualtrics QSF is NOT rebuilt here — it is a genuine Qualtrics
## export built via the API by devel/qualtrics_hitophsum.R (M19). Its
## manifest row is computed from the committed file on disk; when that file
## is replaced, set `qsf_build_date`/`qsf_note` accordingly.

devtools::load_all()

extdata <- "inst/extdata"

## Restrict the rebuild to specific instrument stems, e.g. c("pid5bf"); NULL
## rebuilds every artifact. A targeted rebuild is usually what you want: DOCX
## footers carry a Sys.Date() build stamp and REDCap zips embed member mtimes
## (LESSONS M20), so rebuilding wholesale changes EVERY artifact's checksum --
## and therefore appends a manifest row for every file -- even when only one
## instrument's content actually changed. Only the rebuild loops honor this;
## the manifest section below still reads all files from disk and appends a row
## only where the checksum moved, so unrebuilt artifacts stay silent on their own.
rebuild_stems <- c("pid5", "pid5sf", "pid5bf")

## Restrict the rebuild to specific output formats, e.g. c("docx"); NULL rebuilds
## every format for the selected stems. Format matters independently of stem: a
## content change usually reaches only some of an instrument's artifacts (the BF
## total changed the DOCX scoring table but nothing the Qualtrics or REDCap
## exports emit), and because DOCX/zip rebuilds are not byte-deterministic, a
## needless rebuild churns a checksum and records a manifest revision that isn't
## one. Restrict to the format whose content actually changed.
rebuild_formats <- c("docx")

## Both filters are plain string matches, so a typo ("pid5_bf"), a case slip
## ("DOCX"), or the manifest's own format vocabulary ("docx_us") would match
## nothing, rebuild nothing, and exit successfully -- leaving a committed
## artifact stale against the keying tables with no error and no failing test
## (test-artifacts.R locks file<->manifest, and the generator tests parse fresh
## tempfiles, never the committed files). So record what each filter actually
## matched and abort below if anything went unmatched.
matched_stems <- character(0)
matched_formats <- character(0)

keep_specs <- function(specs, format) {
  if (!is.null(rebuild_formats) && !(format %in% rebuild_formats)) {
    return(list())
  }
  matched_formats <<- union(matched_formats, format)
  keep <- if (is.null(rebuild_stems)) {
    specs
  } else {
    Filter(function(s) s$stem %in% rebuild_stems, specs)
  }
  matched_stems <<- union(
    matched_stems,
    vapply(keep, function(s) s$stem, character(1))
  )
  keep
}

check_filters_matched <- function() {
  all_stems <- sort(unique(vapply(
    c(docx_specs, qualtrics_specs, redcap_specs),
    function(s) s$stem,
    character(1)
  )))
  all_formats <- c("docx", "qualtrics", "redcap")

  bad <- function(what, unknown, known) {
    stop(
      "`", what, "` has no such value: ", paste(sQuote(unknown), collapse = ", "),
      ".\n  Valid values: ", paste(sQuote(known), collapse = ", "),
      ".\n  Nothing was rebuilt -- fix the value and re-source.",
      call. = FALSE
    )
  }

  ## Formats are checked first on purpose: an unrecognized format rejects every
  ## spec, which leaves the stems unmatched too and would otherwise blame
  ## `rebuild_stems` for a `rebuild_formats` typo.
  if (!is.null(rebuild_formats)) {
    unknown <- setdiff(rebuild_formats, all_formats)
    if (length(unknown)) bad("rebuild_formats", unknown, all_formats)
  }
  if (!is.null(rebuild_stems)) {
    unknown <- setdiff(rebuild_stems, all_stems)
    if (length(unknown)) bad("rebuild_stems", unknown, all_stems)
  }

  ## Both filters can be individually valid and still select nothing together
  ## (e.g. stem "hitophsum" with format "qualtrics" -- the HSUM QSF is built by
  ## devel/qualtrics_hitophsum.R, not here), which rebuilds nothing just as
  ## silently as a typo would.
  if (!length(matched_stems)) {
    stop(
      "`rebuild_stems` and `rebuild_formats` select no artifact in combination:",
      "\n  stems = ", paste(sQuote(rebuild_stems), collapse = ", "),
      ", formats = ", paste(sQuote(rebuild_formats), collapse = ", "),
      "\n  Nothing was rebuilt -- widen one of them and re-source.",
      call. = FALSE
    )
  }

  message(
    "Rebuilt stems: ", paste(sort(matched_stems), collapse = ", "),
    " | formats: ", paste(sort(matched_formats), collapse = ", ")
  )
}

## One note per build run, applied to every artifact rebuilt below. For the
## QSF (not rebuilt here), set qsf_* only when the committed file changes.
build_notes <- paste(
  "Response-option legend split across two header lines (values 0 and 1 on",
  "the first, 2 and 3 on the second) so no option phrase breaks mid-phrase;",
  "legend wording, values, and labels are unchanged."
)
qsf_build_date <- as.Date("2026-07-16")
qsf_note <- paste(
  "Rebuilt from the corrected item data via the Qualtrics API: fixes",
  "duplicated questions and the empty cigar-quantity dropdown; SurveyName",
  "carries the build stamp."
)

# ------------------------------------------------------------------------------
## Rebuild the artifacts

docx_specs <- list(
  list(fn = generate_docx_pid5, stem = "pid5", instrument = "PID-5"),
  list(fn = generate_docx_pid5sf, stem = "pid5sf", instrument = "PID-5-SF"),
  list(fn = generate_docx_pid5bf, stem = "pid5bf", instrument = "PID-5-BF"),
  list(fn = generate_docx_hitopsr, stem = "hitopsr", instrument = "HiTOP-SR"),
  list(fn = generate_docx_hitopbr, stem = "hitopbr", instrument = "HiTOP-BR"),
  list(
    fn = generate_docx_hitophsum,
    stem = "hitophsum",
    instrument = "HiTOP-HSUM"
  )
)

for (spec in keep_specs(docx_specs, "docx")) {
  for (ps in c("us", "a4")) {
    spec$fn(
      file = file.path(extdata, paste0(spec$stem, "_", toupper(ps), ".docx")),
      papersize = ps
    )
  }
}

qualtrics_specs <- list(
  list(fn = generate_qualtrics_pid5, stem = "pid5", instrument = "PID-5"),
  list(fn = generate_qualtrics_pid5sf, stem = "pid5sf", instrument = "PID-5-SF"),
  list(fn = generate_qualtrics_pid5bf, stem = "pid5bf", instrument = "PID-5-BF"),
  list(fn = generate_qualtrics_hitopsr, stem = "hitopsr", instrument = "HiTOP-SR"),
  list(fn = generate_qualtrics_hitopbr, stem = "hitopbr", instrument = "HiTOP-BR")
)

for (spec in keep_specs(qualtrics_specs, "qualtrics")) {
  spec$fn(file = file.path(extdata, paste0(spec$stem, "_qualtrics.txt")))
}

redcap_specs <- list(
  list(fn = generate_redcap_pid5, stem = "pid5", instrument = "PID-5"),
  list(fn = generate_redcap_pid5sf, stem = "pid5sf", instrument = "PID-5-SF"),
  list(fn = generate_redcap_pid5bf, stem = "pid5bf", instrument = "PID-5-BF"),
  list(fn = generate_redcap_hitopsr, stem = "hitopsr", instrument = "HiTOP-SR"),
  list(fn = generate_redcap_hitopbr, stem = "hitopbr", instrument = "HiTOP-BR"),
  list(
    fn = generate_redcap_hitophsum,
    stem = "hitophsum",
    instrument = "HiTOP-HSUM"
  )
)

for (spec in keep_specs(redcap_specs, "redcap")) {
  spec$fn(file = file.path(extdata, paste0(spec$stem, "_redcap.zip")))
}

## Every requested stem/format matched something above, or stop before the
## manifest section records a build that did not happen.
check_filters_matched()

# ------------------------------------------------------------------------------
## Rebuild the manifest

new_rows <- list()
add_row <- function(file, instrument, format, build_date, changes) {
  path <- file.path(extdata, file)
  stopifnot(file.exists(path))
  tibble::tibble(
    file = file,
    instrument = instrument,
    format = format,
    instrument_version = "1.0",
    build_date = as.Date(build_date),
    md5 = unname(tools::md5sum(path)),
    changes = changes
  )
}

today <- Sys.Date()
for (spec in docx_specs) {
  for (ps in c("US", "A4")) {
    f <- paste0(spec$stem, "_", ps, ".docx")
    new_rows[[f]] <- add_row(
      f,
      spec$instrument,
      paste0("docx_", tolower(ps)),
      today,
      build_notes
    )
  }
}
for (spec in qualtrics_specs) {
  f <- paste0(spec$stem, "_qualtrics.txt")
  new_rows[[f]] <- add_row(f, spec$instrument, "qualtrics", today, build_notes)
}
for (spec in redcap_specs) {
  f <- paste0(spec$stem, "_redcap.zip")
  new_rows[[f]] <- add_row(f, spec$instrument, "redcap", today, build_notes)
}
new_rows[["hitophsum_qualtrics.qsf"]] <- add_row(
  "hitophsum_qualtrics.qsf",
  "HiTOP-HSUM",
  "qualtrics",
  qsf_build_date,
  qsf_note
)
new_rows <- do.call(rbind, new_rows)

## Keep prior rows as history; append a new row only where the checksum
## changed (so re-sourcing the script without artifact changes is a no-op).
if (file.exists("data/hitop_artifacts.rda")) {
  load("data/hitop_artifacts.rda")
  history <- hitop_artifacts
  latest <- history[!duplicated(history$file, fromLast = TRUE), ]
  changed <- new_rows$file[
    !(new_rows$file %in% latest$file) |
      new_rows$md5 != latest$md5[match(new_rows$file, latest$file)]
  ]
  hitop_artifacts <- rbind(history, new_rows[new_rows$file %in% changed, ])
} else {
  hitop_artifacts <- new_rows
}

hitop_artifacts <- hitop_artifacts[
  order(hitop_artifacts$file, hitop_artifacts$build_date),
]
rownames(hitop_artifacts) <- NULL

usethis::use_data(hitop_artifacts, overwrite = TRUE)
