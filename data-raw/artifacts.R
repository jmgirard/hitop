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
rebuild_stems <- c("pid5bf")

## Restrict the rebuild to specific output formats, e.g. c("docx"); NULL rebuilds
## every format for the selected stems. Format matters independently of stem: a
## content change usually reaches only some of an instrument's artifacts (the BF
## total changed the DOCX scoring table but nothing the Qualtrics or REDCap
## exports emit), and because DOCX/zip rebuilds are not byte-deterministic, a
## needless rebuild churns a checksum and records a manifest revision that isn't
## one. Restrict to the format whose content actually changed.
rebuild_formats <- c("docx")

keep_specs <- function(specs, format) {
  if (!is.null(rebuild_formats) && !(format %in% rebuild_formats)) {
    return(list())
  }
  if (is.null(rebuild_stems)) {
    return(specs)
  }
  Filter(function(s) s$stem %in% rebuild_stems, specs)
}

## One note per build run, applied to every artifact rebuilt below. For the
## QSF (not rebuilt here), set qsf_* only when the committed file changes.
build_notes <- paste(
  "Scoring table gains a Total row (all 25 items): the PID-5-BF total score,",
  "the item-level mean over all 25 items per Markon et al. (2024, p. 23)."
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
