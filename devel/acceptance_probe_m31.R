# AC4 differential acceptance probe: does any converted argument accept or
# reject something different from what it did before?
#
# Usage: Rscript accept.R <pkg-dir> <out.rds>
# Run once against the exported `main` tree and once against the branch; the
# two accept/reject vectors must be identical. Only accept-vs-reject is
# compared, never the message text, which is exactly what this milestone means
# to change.

pkg <- commandArgs(trailingOnly = TRUE)[[1]]
out <- commandArgs(trailingOnly = TRUE)[[2]]
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

## A battery of odd values per argument type. `verdict()` records only whether
## the call aborted, so a changed message reads as no change.
verdict <- function(expr) {
  r <- try(suppressWarnings(suppressMessages(force(expr))), silent = TRUE)
  if (inherits(r, "try-error")) "reject" else "accept"
}

strings <- list(
  ok = "x_", empty = "", num = 1, num_str = "1", na = NA_character_,
  na_lgl = NA, null = NULL, len2 = c("a", "b"), len0 = character(0),
  fct = factor("x_"), lst = list("a"), tru = TRUE
)
flags <- list(
  t = TRUE, f = FALSE, na = NA, one = 1, zero = 0, str = "TRUE",
  null = NULL, len2 = c(TRUE, FALSE), len0 = logical(0), fct = factor("TRUE")
)
counts <- list(
  one = 1, two = 2, dbl = 2.0, frac = 2.5, zero = 0, neg = -1, big = 999,
  str = "2", na = NA_integer_, null = NULL, len2 = c(1, 2), len0 = integer(0),
  tru = TRUE
)
dirs <- list(
  high = "high", low = "low", fct_high = factor("high"), abbrev = "l",
  typo = "hihg", num = 1, na = NA_character_, null = NULL,
  len2 = c("high", "low"), len0 = character(0), upper = "HIGH"
)
frames <- list(
  df = data.frame(a = 1:2), tib = tibble::tibble(a = 1:2), mat = matrix(1:4, 2),
  lst = list(a = 1:2), vec = 1:5, null = NULL, na = NA
)

res <- list()
rec <- function(key, v) res[[key]] <<- v

d5 <- sim_pid5bf
scored <- suppressWarnings(suppressMessages(
  score_pid5(d5, items = 1:25, version = "BF", append = FALSE)
))
sn <- names(scored)

for (nm in names(strings)) {
  v <- strings[[nm]]
  rec(paste0("score_pid5/prefix/", nm),
      verdict(score_pid5(d5, items = 1:25, version = "BF", prefix = v)))
  rec(paste0("validity_pid5/prefix/", nm),
      verdict(validity_pid5(sim_pid5, items = 1:220, version = "FULL", prefix = v)))
  rec(paste0("norm_pid5/prefix/", nm),
      verdict(norm_pid5(scored, scores = sn, version = "BF", prefix = v)))
  rec(paste0("rank_scales/prefix/", nm),
      verdict(rank_scales(scored, scales = sn, prefix = v)))
  rec(paste0("rank_scales/name/", nm),
      verdict(rank_scales(scored, scales = sn, name = v)))
  rec(paste0("label_hitopsr/prefix/", nm),
      verdict(label_hitopsr(sim_hitopsr, prefix = v)))
}

for (nm in names(flags)) {
  v <- flags[[nm]]
  rec(paste0("score_pid5/append/", nm),
      verdict(score_pid5(d5, items = 1:25, version = "BF", append = v)))
  rec(paste0("score_pid5/calc_se/", nm),
      verdict(score_pid5(d5, items = 1:25, version = "BF", calc_se = v)))
  rec(paste0("validity_pid5/append/", nm),
      verdict(validity_pid5(sim_pid5, items = 1:220, version = "FULL", append = v)))
  rec(paste0("norm_pid5/append/", nm),
      verdict(norm_pid5(scored, scores = sn, version = "BF", append = v)))
  rec(paste0("rank_scales/append/", nm),
      verdict(rank_scales(scored, scales = sn, append = v)))
  rec(paste0("reliability_pid5/alpha/", nm),
      verdict(reliability_pid5(d5, items = 1:25, version = "BF", alpha = v, omega = FALSE)))
  rec(paste0("reliability_pid5/omega/", nm),
      verdict(reliability_pid5(d5, items = 1:25, version = "BF", omega = v)))
}

for (nm in names(counts)) {
  rec(paste0("rank_scales/top/", nm),
      verdict(rank_scales(scored, scales = sn, top = counts[[nm]])))
}

for (nm in names(dirs)) {
  rec(paste0("rank_scales/dir/", nm),
      verdict(rank_scales(scored, scales = sn, dir = dirs[[nm]])))
}

for (nm in names(frames)) {
  v <- frames[[nm]]
  rec(paste0("label_hitopsr/data/", nm), verdict(label_hitopsr(v)))
  rec(paste0("label_hitopbr/data/", nm), verdict(label_hitopbr(v)))
  rec(paste0("rename_hitopsr_items/data/", nm), verdict(rename_hitopsr_items(v)))
}

saveRDS(unlist(res), out)
cat("probed", length(res), "argument/value pairs\n")
