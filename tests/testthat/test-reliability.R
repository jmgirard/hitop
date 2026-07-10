# Ground-truth oracle tests for the reliability functions (milestone M5):
# calc_alpha() (covariance-based Cronbach's alpha) and calc_omega()
# (omega-total via a one-factor lavaan CFA).

test_that("calc_alpha() matches a hand-computed reference value", {
  # Tiny 3-item, 4-observation fixture; alpha worked out by hand below.
  #   X1 = 1,2,3,4   Var = 5/3   (cov() uses the n-1 denominator)
  #   X2 = 2,2,4,4   Var = 4/3
  #   X3 = 1,3,3,5   Var = 8/3
  #   sum of item variances       = 17/3
  #   T = X1+X2+X3 = 4,7,10,13     Var(T) = 45/3 = 15
  #   alpha = k/(k-1) * (1 - sumVar/VarT)
  #         = 3/2 * (1 - (17/3)/15) = 3/2 * (28/45) = 42/45 = 0.9333...
  df <- data.frame(
    X1 = c(1, 2, 3, 4),
    X2 = c(2, 2, 4, 4),
    X3 = c(1, 3, 3, 5)
  )
  expect_equal(calc_alpha(df), 42 / 45)
})

test_that("calc_alpha() handles NA via pairwise deletion", {
  df <- data.frame(
    X1 = c(1, 2, 3, 4),
    X2 = c(2, 2, 4, 4),
    X3 = c(1, 3, 3, 5)
  )
  df_na <- df
  df_na[1, "X1"] <- NA
  # Pairwise-complete covariances still return a finite alpha (not NA).
  expect_true(is.finite(calc_alpha(df_na)))
})

test_that("calc_alpha() rejects degenerate input", {
  expect_error(calc_alpha(data.frame(a = 1:4)), "two items")            # k < 2
  expect_error(calc_alpha(data.frame(a = 1, b = 2)), "two observations") # n < 2
  expect_error(
    calc_alpha(data.frame(a = c(1, 1, 1), b = c(2, 3, 4))),
    "positive variance"
  )
  expect_error(calc_alpha(1:4), "data frame or matrix")
})

test_that("calc_omega() matches a direct lavaan CFA on the same data", {
  skip_if_not_installed("lavaan")

  # Build a clean one-factor dataset (all positive loadings -> converges).
  set.seed(1)
  n <- 300
  f <- stats::rnorm(n)
  df <- data.frame(
    i1 = 0.8 * f + stats::rnorm(n),
    i2 = 0.7 * f + stats::rnorm(n),
    i3 = 0.6 * f + stats::rnorm(n),
    i4 = 0.75 * f + stats::rnorm(n),
    i5 = 0.65 * f + stats::rnorm(n)
  )

  # Independent reference: fit the same model directly and apply the omega
  # formula ourselves, without going through calc_omega()'s code path.
  fit <- lavaan::cfa(
    "f =~ i1 + i2 + i3 + i4 + i5",
    data = df, std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  pe <- lavaan::parameterEstimates(fit)
  lambda <- pe$est[pe$op == "=~"]
  theta <- pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs != "f"]
  ref <- sum(lambda)^2 / (sum(lambda)^2 + sum(theta))

  expect_equal(calc_omega(df), ref)
  expect_true(ref > 0 && ref < 1)
})

test_that("calc_omega() warns on mixed-sign loadings", {
  skip_if_not_installed("lavaan")

  set.seed(2)
  n <- 300
  f <- stats::rnorm(n)
  df <- data.frame(
    i1 = 0.8 * f + stats::rnorm(n),
    i2 = 0.7 * f + stats::rnorm(n),
    i3 = 0.6 * f + stats::rnorm(n),
    i4 = -0.75 * f + stats::rnorm(n)  # reversed -> mixed-sign loadings
  )
  expect_warning(calc_omega(df), "[Mm]ixed-sign")
})

test_that("calc_omega() rejects degenerate input", {
  skip_if_not_installed("lavaan")
  expect_error(calc_omega(data.frame(a = 1:4)), "two items")
  expect_error(calc_omega("nope"), "data frame or matrix")
})
