# The `version` argument of the seven PID-5 functions (M170, D-094): full
# names only, in any letter case, and a classed refusal for anything else.

six <- c("FULL", "SF", "BF", "BFPM", "IRF", "FFBF")
three <- c("FULL", "SF", "BF")

# Input that passes every check each function makes, for one version.
version_input <- function(v) {
  switch(
    v,
    FULL = list(data = sim_pid5[1:20, ], n = 220),
    SF = list(data = sim_pid5sf[1:20, ], n = 100),
    BF = list(data = sim_pid5bf[1:20, ], n = 25),
    BFPM = list(data = fx_pid5bfpm(), n = 36),
    IRF = list(data = fx_pid5irf(), n = 218),
    FFBF = list(data = fx_pid5ffbf(), n = 100)
  )
}

domain_cols <- paste0("pid_", pid_domains$camelCase)

normed_input <- function(v) {
  inp <- version_input(v)
  scored <- score_pid5(inp$data[1, ], items = seq_len(inp$n), version = v)
  # The BF domain profile also plots the BF total.
  cols <- if (v == "BF") c(domain_cols, "pid_total") else domain_cols
  suppressWarnings(norm_pid5(scored, scores = cols, version = v))
}

# One call per function. `s` is the spelling passed as `version`; `v` is the
# version it must resolve to, which picks valid input. A missing `s` omits
# the argument.
version_calls <- list(
  score_pid5 = function(s, v) {
    inp <- version_input(v)
    if (missing(s)) {
      return(score_pid5(inp$data, items = seq_len(inp$n), append = FALSE))
    }
    score_pid5(inp$data, items = seq_len(inp$n), version = s, append = FALSE)
  },
  reliability_pid5 = function(s, v) {
    inp <- version_input(v)
    suppressWarnings(if (missing(s)) {
      reliability_pid5(inp$data, items = seq_len(inp$n), omega = FALSE)
    } else {
      reliability_pid5(inp$data, items = seq_len(inp$n), version = s, omega = FALSE)
    })
  },
  rename_pid5_items = function(s, v) {
    df <- data.frame(pid_1 = 1, pid_2 = 2, pid_3 = 3)
    suppressWarnings(if (missing(s)) {
      rename_pid5_items(df)
    } else {
      rename_pid5_items(df, version = s)
    })
  },
  label_pid5 = function(s, v) {
    df <- suppressWarnings(
      rename_pid5_items(data.frame(pid_1 = 1, pid_2 = 2, pid_3 = 3), version = v)
    )
    if (missing(s)) label_pid5(df) else label_pid5(df, version = s)
  },
  validity_pid5 = function(s, v) {
    inp <- version_input(v)
    suppressMessages(if (missing(s)) {
      validity_pid5(inp$data, items = seq_len(inp$n), append = FALSE)
    } else {
      validity_pid5(inp$data, items = seq_len(inp$n), version = s, append = FALSE)
    })
  },
  norm_pid5 = function(s, v) {
    inp <- version_input(v)
    scored <- score_pid5(inp$data, items = seq_len(inp$n), version = v)
    suppressWarnings(if (missing(s)) {
      norm_pid5(scored, scores = domain_cols, append = FALSE)
    } else {
      norm_pid5(scored, scores = domain_cols, version = s, append = FALSE)
    })
  },
  plot_pid5 = function(s, v) {
    normed <- normed_input(v)
    p <- if (missing(s)) plot_pid5(normed) else plot_pid5(normed, version = s)
    ggplot2::ggplot_build(p)$data
  }
)

choices_of <- function(fn) {
  if (fn %in% c("validity_pid5", "norm_pid5", "plot_pid5")) three else six
}

needs_ggplot <- function(fn) {
  fn == "plot_pid5" && !rlang::is_installed("ggplot2", version = "3.4.0")
}

test_that("each PID-5 function refuses a value that is not one full name, by class", {
  refused <- list("XYZ", "S", "F", "B", "FF", "", NA, NULL, 1, c("FULL", "SF"))
  for (fn in names(version_calls)) {
    if (needs_ggplot(fn)) next
    choices <- choices_of(fn)
    for (value in refused) {
      label <- paste(fn, deparse1(value))
      cnd <- rlang::catch_cnd(version_calls[[fn]](value, "FULL"), classes = "error")
      expect_s3_class(cnd, "hitop_unknown_version")
      expect_identical(rlang::call_name(cnd$call), fn, label = label)
      msg <- conditionMessage(cnd)
      expect_true(grepl("version", msg, fixed = TRUE), label = label)
      for (choice in choices) {
        expect_true(grepl(sprintf('"%s"', choice), msg, fixed = TRUE), label = paste(label, choice))
      }
    }
  }
})

test_that("each PID-5 function resolves a full name in any case, and an omitted version as FULL", {
  mixed <- c(FULL = "Full", SF = "sF", BF = "bF", BFPM = "bFpM", IRF = "IrF", FFBF = "fFbF")
  for (fn in names(version_calls)) {
    if (needs_ggplot(fn)) next
    call <- version_calls[[fn]]
    for (v in choices_of(fn)) {
      ref <- call(v, v)
      for (s in c(tolower(v), mixed[[v]])) {
        expect_identical(call(s, v), ref, label = paste(fn, s))
      }
    }
    full <- call("FULL", "FULL")
    expect_identical(call(, "FULL"), full, label = paste(fn, "omitted"))
    expect_identical(call(tolower(choices_of(fn)), "FULL"), full, label = paste(fn, "default vector"))
  }
})
