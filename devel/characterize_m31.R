# AC7 characterization harness for M31.
#
# Captures every returned value of the five functions M31 touches, across the
# versions and `missing` modes the suite exercises, into one .rds. Run once on
# the pre-milestone tree and once at the end; the two must be identical().
#
# Usage: Rscript devel/characterize_m31.R <out.rds>
#
# Not part of the package (devel/ is .Rbuildignored); it exists so the AC7
# evidence is reproducible rather than a one-off scratch run.

suppressMessages(devtools::load_all(quiet = TRUE))

out_path <- commandArgs(trailingOnly = TRUE)[[1]]

quietly <- function(expr) {
  suppressWarnings(suppressMessages(try(expr, silent = TRUE)))
}

cases <- list(
  list(data = sim_pid5, n = 220, version = "FULL"),
  list(data = sim_pid5sf, n = 100, version = "SF"),
  list(data = sim_pid5bf, n = 25, version = "BF")
)

res <- list()

for (k in cases) {
  items <- seq_len(k$n)
  v <- k$version

  for (m in c("apa", "available", "complete")) {
    for (se in c(FALSE, TRUE)) {
      key <- paste("score", v, m, se, sep = "/")
      res[[key]] <- quietly(score_pid5(
        k$data, items = items, version = v, missing = m,
        calc_se = se, append = FALSE
      ))
    }
  }

  ## Validity screens run off the raw item data.
  res[[paste("validity", v, sep = "/")]] <- quietly(
    validity_pid5(k$data, items = items, version = v, append = FALSE)
  )

  res[[paste("reliability", v, sep = "/")]] <- quietly(
    reliability_pid5(k$data, items = items, version = v, omega = FALSE)
  )

  ## Norming takes scored output; also exercise a shifted coding.
  scored <- quietly(score_pid5(k$data, items = items, version = v))
  if (!inherits(scored, "try-error")) {
    res[[paste("norm", v, sep = "/")]] <- quietly(
      norm_pid5(scored, scores = grep("^pid_", names(scored), value = TRUE),
                version = v, append = FALSE)
    )
    res[[paste("rank", v, sep = "/")]] <- quietly(
      rank_scales(scored, scales = grep("^pid_", names(scored), value = TRUE),
                  prefix = "pid_", top = 3, dir = "high")
    )
  }

  shifted <- k$data
  shifted[items] <- k$data[items] + 1
  sc2 <- quietly(score_pid5(shifted, items = items, version = v, srange = c(1, 4)))
  if (!inherits(sc2, "try-error")) {
    res[[paste("norm-shift", v, sep = "/")]] <- quietly(
      norm_pid5(sc2, scores = grep("^pid_", names(sc2), value = TRUE),
                version = v, srange = c(1, 4), append = FALSE)
    )
  }
}

saveRDS(res, out_path)
cat("captured", length(res), "configs\n")
cat("errored:", sum(vapply(res, inherits, logical(1), "try-error")), "\n")
for (nm in names(res)) {
  if (inherits(res[[nm]], "try-error")) cat("  ERR", nm, "\n")
}
