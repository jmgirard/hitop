# Verify that the NSSI rename moved what it had to and nothing else (M058, AC2/AC3)
#
# Three checks that need something the test suite cannot reach -- the git
# history, and a second build of the package -- so they live here rather than in
# tests/testthat/. The shipped-object half of AC2 and AC3 is
# tests/testthat/test-scale-name-hitopsr.R, which runs everywhere.
#
#   1. Working-tree sweep. No case-insensitive `nssi` survives outside an
#      allow-list written below, where the old name is deliberate history.
#   2. Keying-table invariance. The four HiTOP-SR tables are compared against
#      the same objects loaded from a git worktree of the merge-base of this
#      branch and the default branch, and must be identical except in the cells
#      that held the old name.
#   3. Scored-output invariance. score_hitopsr(sim_hitopsr, calc_se = TRUE) is
#      compared against the merge-base build's result over the whole returned
#      tibble, and must be equal once the two renamed columns are renamed back.
#
# The merge-base build is a reference for *invariance only*. It certifies that
# this milestone changed nothing it did not intend; it never certifies that a
# value is correct. The name itself is verified against the published source by
# data-raw/verify_hitopsr_scale_name.R, which is the only oracle for that.
#
# Maintainer-run, never CI: it shells out to git and builds the package twice.
# Exits non-zero on any discrepancy.

old_pattern <- "nssi"

## Where the old name is history rather than a leftover. Anchored at the repo
## root and matched against paths, so a new file cannot inherit an exemption by
## resembling one of these.
allow <- c(
  "^NEWS\\.md$",              # the rename is announced there by name
  "^cairn/",                  # tracking: the milestone, decisions, reviews
  "^data-raw/verify_hitopsr_", # these verifiers name the old spelling
  "^tests/testthat/test-scale-name-hitopsr\\.R$" # the sweep names what it forbids
)

fail <- character(0)
note <- function(...) fail <<- c(fail, paste0(...))

repo <- normalizePath(".")
stopifnot("run from the package root" = file.exists(file.path(repo, "DESCRIPTION")))

## ---------------------------------------------------------------- 1. tree ---

tracked <- system2("git", c("ls-files"), stdout = TRUE)
exempt <- Reduce(`|`, lapply(allow, function(a) grepl(a, tracked)))
candidates <- tracked[!exempt]

## Every character vector reachable inside an R object, list names included --
## a named list keyed by scale stem can hold the old spelling with every value
## clean.
character_leaves <- function(x) {
  if (is.character(x)) return(x)
  if (is.factor(x)) return(as.character(x))
  if (!is.list(x)) return(character(0))
  c(if (!is.null(names(x))) names(x) else character(0),
    unlist(lapply(x, character_leaves), use.names = FALSE))
}

## A compressed file cannot be searched as bytes: `.rda` is gzip, and `.docx`
## and `.zip` are zip containers, so a raw scan reads them as noise and passes
## whatever they contain. Both are surfaces this rename reaches -- R/sysdata.rda
## holds the administration text, and the Word forms print the scale name -- so
## each is opened in its own format rather than sniffed.
scan_file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  if (ext %in% c("rda", "rdata")) {
    e <- new.env()
    load(f, envir = e)
    return(character_leaves(as.list(e)))
  }
  if (ext == "rds") {
    return(character_leaves(readRDS(f)))
  }
  if (ext %in% c("docx", "zip", "xlsx", "qsf")) {
    members <- tryCatch(utils::unzip(f, list = TRUE)$Name, error = function(e) NULL)
    if (is.null(members)) {
      raw <- readBin(f, "raw", file.size(f))
      txt <- rawToChar(raw[raw != as.raw(0)])
      Encoding(txt) <- "bytes"
      return(c(basename(f), txt))
    }
    inner <- unlist(lapply(members, function(m) {
      con <- unz(f, m, open = "rb")
      on.exit(close(con))
      raw <- readBin(con, "raw", 50e6)
      txt <- rawToChar(raw[raw != as.raw(0)])
      Encoding(txt) <- "bytes"
      txt
    }), use.names = FALSE)
    return(c(members, inner))
  }
  raw <- readBin(f, "raw", file.size(f))
  txt <- rawToChar(raw[raw != as.raw(0)])
  Encoding(txt) <- "bytes"
  txt
}

hits <- character(0)
for (f in candidates) {
  if (grepl(old_pattern, f, ignore.case = TRUE)) {
    hits <- c(hits, paste0(f, " (filename)"))
    next
  }
  strings <- tryCatch(scan_file(f), error = function(e) {
    note("could not read ", f, ": ", conditionMessage(e))
    character(0)
  })
  if (any(grepl(old_pattern, strings, ignore.case = TRUE, useBytes = TRUE))) {
    hits <- c(hits, f)
  }
}

cat("1. Working-tree sweep over ", length(candidates), " tracked files ",
    "(", length(tracked) - length(candidates), " allow-listed)\n", sep = "")
if (length(hits)) {
  note(length(hits), " file(s) still carry \"", old_pattern, "\": ",
       paste(hits, collapse = ", "))
  for (h in hits) cat("   - ", h, "\n", sep = "")
} else {
  cat("   clean\n")
}

## ------------------------------------------------------------ 2/3. base ---

base <- system2("git", c("merge-base", "HEAD", "origin/main"), stdout = TRUE)
if (!length(base) || !nzchar(base)) stop("could not resolve the merge-base", call. = FALSE)
cat("\nMerge-base with origin/main: ", base, "\n", sep = "")

wt <- file.path(tempdir(), paste0("m058-base-", substr(base, 1, 8)))
if (dir.exists(wt)) system2("git", c("worktree", "remove", "--force", shQuote(wt)))
st <- system2("git", c("worktree", "add", "--detach", shQuote(wt), base),
              stdout = TRUE, stderr = TRUE)
if (!dir.exists(wt)) stop("git worktree add failed: ", paste(st, collapse = " "), call. = FALSE)
on.exit(system2("git", c("worktree", "remove", "--force", shQuote(wt))), add = TRUE)

keying <- c("hitopsr_items", "hitopsr_scales", "hitopsr_subscales", "hitopsr_definitions")

base_env <- new.env()
for (nm in keying) load(file.path(wt, "data", paste0(nm, ".rda")), envir = base_env)
here_env <- new.env()
for (nm in keying) load(file.path(repo, "data", paste0(nm, ".rda")), envir = here_env)

## Blank every cell that held the old name on the base side and its counterpart
## on this side, then require what is left to be identical. Any other movement
## -- an item text, a reverse flag, a row order, an item count -- survives the
## blanking and fails the comparison.
## Both spellings, so the same pattern blanks both sides. Blanking each side
## with only its own spelling would leave a table that already carried the new
## name -- hitopsr_definitions did -- blanked on one side and not the other, and
## report a difference that is an artifact of the comparison.
renamed_cell <- "nssi|Non-suicidal Self-injury|nonSuicidalSelfInjury"

## Structure-preserving: a data frame comes back a data frame with the same
## class and attributes, so the comparison below still sees a tibble.
blank_renamed <- function(x, pattern = renamed_cell) {
  if (is.character(x)) {
    x[grepl(pattern, x, ignore.case = TRUE)] <- NA_character_
    return(x)
  }
  if (is.list(x)) {
    if (!is.null(names(x))) {
      names(x)[grepl(pattern, names(x), ignore.case = TRUE)] <- NA_character_
    }
    for (i in seq_along(x)) x[[i]] <- blank_renamed(x[[i]], pattern)
    return(x)
  }
  x
}

## Renaming a scale moves its row, because the tables are sorted by name and
## `dplyr::arrange()` sorts in the C locale: `NSSI` sorted among the uppercase
## names, `Non-suicidal Self-injury` sorts after `Non-planfulness`. That is a
## consequence of the rename, not a second change, so rows are matched by
## identity rather than by position and any move is reported rather than failed.
row_key <- function(df) {
  if ("HSR" %in% names(df)) return(as.character(df$HSR))
  if ("itemNumbers" %in% names(df)) {
    return(vapply(df$itemNumbers, function(x) paste(x, collapse = ","), character(1)))
  }
  ## hitopsr_definitions keys on what each row defines, minus the renamed stem.
  apply(as.data.frame(lapply(df[intersect(c("Scale", "Subscale"), names(df))],
                             function(col) blank_renamed(as.character(col)))),
        1, paste, collapse = "|")
}

cat("\n2. Keying-table invariance against the merge-base\n")
for (nm in keying) {
  b <- get(nm, envir = base_env)
  h <- get(nm, envir = here_env)
  if (!identical(dim(b), dim(h))) {
    note(nm, ": dimensions moved, ", paste(dim(b), collapse = "x"), " -> ",
         paste(dim(h), collapse = "x"))
    cat("   ", nm, ": FAIL (dimensions)\n", sep = "")
    next
  }
  bk <- row_key(b)
  hk <- row_key(h)
  if (!setequal(bk, hk) || anyDuplicated(bk) || anyDuplicated(hk)) {
    note(nm, ": the set of rows changed, or a row key is not unique")
    cat("   ", nm, ": FAIL (row set)\n", sep = "")
    next
  }
  moved <- which(bk != hk)
  bb <- blank_renamed(b)
  hh <- blank_renamed(h)[match(bk, hk), ]
  ## Row names carry the pre-reorder positions after the match, and are not data.
  attr(bb, "row.names") <- attr(hh, "row.names") <- seq_len(nrow(b))
  if (!identical(bb, hh)) {
    note(nm, ": differs from the merge-base outside the renamed cells")
    cat("   ", nm, ": FAIL\n", sep = "")
  } else if (length(moved)) {
    cat("   ", nm, ": identical outside the renamed cells; the renamed row moves ",
        "from position ", which(grepl(old_pattern, do.call(paste, c(b, sep = " ")), ignore.case = TRUE))[1],
        " to ", which(grepl("Non-suicidal Self-injury", do.call(paste, c(h, sep = " ")), fixed = TRUE))[1],
        " (sort order, expected)\n", sep = "")
  } else {
    cat("   ", nm, ": identical outside the renamed cells\n", sep = "")
  }
}

## ------------------------------------------------------------ 3. scored ---

score_in <- function(pkg_dir, out) {
  code <- sprintf(
    'suppressMessages(devtools::load_all(%s, quiet = TRUE)); saveRDS(score_hitopsr(sim_hitopsr, items = 1:405, calc_se = TRUE), %s)',
    shQuote(pkg_dir), shQuote(out))
  res <- system2("Rscript", c("-e", shQuote(code)), stdout = TRUE, stderr = TRUE)
  if (!file.exists(out)) stop("scoring failed in ", pkg_dir, ":\n", paste(res, collapse = "\n"), call. = FALSE)
  readRDS(out)
}

cat("\n3. Scored-output invariance against the merge-base\n")
base_scored <- score_in(wt, file.path(tempdir(), "m058-base-scored.rds"))
here_scored <- score_in(repo, file.path(tempdir(), "m058-here-scored.rds"))

## Written literally, never re-derived by snakecase::to_any_case() -- that is
## the function the rename itself used.
rename_map <- c(hsr_nssi = "hsr_nonSuicidalSelfInjury",
                hsr_nssi_se = "hsr_nonSuicidalSelfInjury_se")

missing_old <- setdiff(names(rename_map), names(base_scored))
missing_new <- setdiff(unname(rename_map), names(here_scored))
if (length(missing_old)) note("merge-base output lacks: ", paste(missing_old, collapse = ", "))
if (length(missing_new)) note("current output lacks: ", paste(missing_new, collapse = ", "))

if (!length(missing_old) && !length(missing_new)) {
  renamed <- base_scored
  names(renamed)[match(names(rename_map), names(renamed))] <- unname(rename_map)

  ## The column set and every column's values must be identical. Position is
  ## checked separately and reported rather than failed: the scored columns
  ## follow `hitopsr_scales`'s row order, which the rename moves for the same
  ## sort reason the table itself moves, so a position change is a consequence
  ## of the rename rather than a second change. A value change is not.
  if (!setequal(names(renamed), names(here_scored))) {
    note("scored output's column set changed: ",
         "added ", paste(setdiff(names(here_scored), names(renamed)), collapse = ", "), "; ",
         "dropped ", paste(setdiff(names(renamed), names(here_scored)), collapse = ", "))
    cat("   FAIL (column set)\n")
  } else {
    moved <- names(here_scored)[!vapply(names(here_scored), function(k)
      identical(renamed[[k]], here_scored[[k]]), logical(1))]
    if (length(moved)) {
      note("scored values changed in: ", paste(moved, collapse = ", "))
      cat("   FAIL (values in ", paste(moved, collapse = ", "), ")\n", sep = "")
    } else {
      cat("   every column identical in value once the two columns are renamed\n")
      if (!identical(names(renamed), names(here_scored))) {
        from <- which(names(renamed) == unname(rename_map)[[1]])
        to <- which(names(here_scored) == unname(rename_map)[[1]])
        cat("   column order moves: ", unname(rename_map)[[1]], " from position ",
            from, " to ", to, " (follows hitopsr_scales' sort order, expected)\n", sep = "")
        others <- setdiff(names(renamed), unname(rename_map))
        if (!identical(others[order(match(others, names(renamed)))],
                       others[order(match(others, names(here_scored)))])) {
          note("scored output reorders columns other than the renamed pair")
          cat("   FAIL (other columns reordered)\n")
        }
      }
    }
  }
}

cat("\n")
if (!length(fail)) {
  cat("RESULT: the rename reached every place the old name lived and moved nothing else.\n")
} else {
  cat("RESULT: ", length(fail), " discrepancy/discrepancies\n\n", sep = "")
  for (f in fail) cat("  - ", f, "\n", sep = "")
  stop(length(fail), " discrepancy/discrepancies", call. = FALSE)
}
