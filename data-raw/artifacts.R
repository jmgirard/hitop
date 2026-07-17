## Regenerate the committed distribution artifacts in inst/extdata/ and
## update the hitop_artifacts manifest (D-016).
##
## Usage: edit `build_notes` below to describe what changed in this build,
## then source the whole script from the package root. Every DOCX, Qualtrics
## .txt, and REDCap .zip artifact is rebuilt from the current keying tables;
## the manifest gains a new row for each file whose checksum changed (the
## full row history is kept, so the manifest doubles as a changelog).
##
## The HSUM Qualtrics QSF is NOT rebuilt here — it is a genuine Qualtrics
## export built via the API by devel/qualtrics_hitophsum.R (M19). Its
## manifest row is computed from the committed file on disk; when that file
## is replaced, set `qsf_build_date`/`qsf_note` accordingly.

devtools::load_all()

extdata <- "inst/extdata"

## One note per build run, applied to every artifact rebuilt below. For the
## QSF (not rebuilt here), set qsf_* only when the committed file changes.
build_notes <- paste(
  "Versioning system introduced (D-016): renamed from the _1.0_ filename",
  "scheme; DOCX footers gain a build stamp."
)
qsf_build_date <- as.Date("2026-07-16")
qsf_note <- paste(
  "Rebuilt from the corrected item data via the Qualtrics API (M19): fixes",
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

for (spec in docx_specs) {
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

for (spec in qualtrics_specs) {
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

for (spec in redcap_specs) {
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
