## The second of the two procedures M080 uses to show no PID-5 item column is
## named by the old `pid_<n>` pattern anywhere the package ships. The first is a
## text search over tracked files; this one reads what a text search cannot see,
## the binary artifacts: every object in `data/` and `R/sysdata.rda`, and the
## member list of every archive in `inst/extdata/`.
##
## It walks attributes as well as element names and character values, because a
## readr tibble records the column names it was read under one attribute down,
## in `spec$cols` — where `names()` alone does not reach. That is exactly where
## a stale `pid_1`..`pid_100` survived the first pass of the M080 rename.
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

archives <- list.files("inst/extdata", full.names = TRUE)
members <- 0L
for (z in archives) {
  mem <- tryCatch(as.character(utils::unzip(z, list = TRUE)$Name),
                  error = function(e) character(0))
  members <- members + length(mem)
  bad <- grep(pattern, mem, value = TRUE)
  if (length(bad)) {
    hits <- hits + length(bad)
    cat(sprintf("%s -> %s\n", z, paste(bad, collapse = ", ")))
  }
}

cat(sprintf("scanned %d object files and %d members across %d archives; %d hits\n",
            length(files), members, length(archives), hits))
if (hits > 0L) stop("old-pattern PID-5 item names are still shipped")
