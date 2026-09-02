## The second of the two procedures M080 uses to show no PID-5 item column is
## named by the old `pid_<n>` pattern anywhere the package ships. The first is a
## text search over tracked files; this one reads what a text search cannot see,
## the binary artifacts: every object in `data/` and `R/sysdata.rda`, and both
## the member list and the member contents of every archive in `inst/extdata/`
## (the REDCap archives hold an `instrument.csv` naming every field, which a
## member listing alone does not reach).
##
## It walks attributes as well as element names and character values, because a
## readr tibble records the column names it was read under one attribute down,
## in `spec$cols` — where `names()` alone does not reach. That is exactly where
## a stale set of old short-form names survived the first pass of this rename.
##
## Run from the package root. Prints one line per object carrying a hit, then a
## count of what it scanned, so a silent run is distinguishable from an empty
## domain. Exits non-zero on any hit.

pattern <- "pid_[0-9]"

collect <- function(x, acc = character(0), depth = 0L) {
  if (depth > 12L) return(acc)
  a <- attributes(x)
  if (!is.null(a)) {
    acc <- c(acc, names(a))
    for (el in a) acc <- collect(el, acc, depth + 1L)
  }
  if (is.character(x)) acc <- c(acc, x)
  if (is.list(x)) for (el in x) acc <- collect(el, acc, depth + 1L)
  acc
}

hits <- 0L

files <- c(list.files("data", pattern = "[.]rda$", full.names = TRUE), "R/sysdata.rda")
for (f in files) {
  e <- new.env()
  load(f, envir = e)
  for (nm in ls(e)) {
    bad <- unique(grep(pattern, collect(e[[nm]]), value = TRUE))
    if (length(bad)) {
      hits <- hits + length(bad)
      cat(sprintf("%s:%s -> %s\n", f, nm, paste(bad, collapse = ", ")))
    }
  }
}

# Member contents are read as raw bytes and matched with `grepRaw()`, so a
# member holding embedded nulls (a .docx carries compressed images among its
# parts) is searched rather than skipped.
raw_member <- function(z, m) {
  con <- unz(z, m, open = "rb")
  on.exit(close(con))
  out <- raw(0)
  repeat {
    chunk <- readBin(con, "raw", n = 1e6)
    if (length(chunk) == 0L) break
    out <- c(out, chunk)
  }
  out
}

archives <- list.files("inst/extdata", full.names = TRUE)
members <- 0L
read_members <- 0L
for (z in archives) {
  mem <- tryCatch(as.character(utils::unzip(z, list = TRUE)$Name),
                  error = function(e) character(0))
  members <- members + length(mem)
  bad <- grep(pattern, mem, value = TRUE)
  if (length(bad)) {
    hits <- hits + length(bad)
    cat(sprintf("%s -> %s\n", z, paste(bad, collapse = ", ")))
  }
  for (m in mem) {
    bytes <- raw_member(z, m)
    read_members <- read_members + 1L
    if (length(grepRaw(pattern, bytes, all = FALSE))) {
      hits <- hits + 1L
      cat(sprintf("%s::%s -> content matches %s\n", z, m, pattern))
    }
  }
}

cat(sprintf(paste0("scanned %d object files and %d members across %d archives ",
                   "(%d member bodies read); %d hits\n"),
            length(files), members, length(archives), read_members, hits))
if (hits > 0L) stop("old-pattern PID-5 item names are still shipped")
