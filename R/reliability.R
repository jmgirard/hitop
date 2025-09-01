#' Compute Cronbach's Coefficient Alpha
#'
#' @description
#' `calc_alpha()` computes Cronbach's coefficient alpha for a set of items from
#' the **unstandardized covariance** matrix. Items are coerced to numeric before
#' calculation. Missing data can be handled via **pairwise** deletion.
#'
#' @details
#' Let \eqn{k} be the number of items and let \eqn{X_1,\ldots,X_k} denote the item
#' scores. Define the total score \eqn{T = \sum_{i=1}^k X_i}. Cronbach's alpha can be
#' written without matrix notation as
#' \deqn{\alpha \;=\; \frac{k}{k-1}\left(1 - \frac{\sum_{i=1}^k \mathrm{Var}(X_i)}{\mathrm{Var}(T)}\right).}
#' In words: take the sum of the item variances and divide by the variance of the
#' total score; subtract this ratio from 1 and multiply by \eqn{k/(k-1)}.
#'
#' Both \eqn{\mathrm{Var}(X_i)} and \eqn{\mathrm{Var}(T)} are
#' computed using pairwise-complete observations.
#' @section Coercion:
#' Each column is coerced via `as.numeric()`. This means:
#' - factors are converted to **integer level codes** (not labels);
#' - characters that are not parsable as numbers become `NA`;
#' - dates/times become their underlying numeric representations.
#' Ensure your columns are truly numeric item responses or pre-process as needed.
#'
#' @param df A data frame or matrix with one row per observation and one column per item.
#'
#' @return A numeric scalar giving Cronbach's alpha (typically in \[0, 1], but
#' can be outside this range for problematic data).
#'
#' @references
#' - Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
#'   tests. *Psychometrika, 16*(3), 297–334.
#' - Sijtsma, K. (2009). On the use, the misuse, and the very limited usefulness
#'   of Cronbach's alpha. *Psychometrika, 74*(1), 107–120.
#' @export
calc_alpha <- function(df) {
  # Validate df arg
  cli_assert(
    condition = is.data.frame(df) || is.matrix(df),
    message = "`df` must be a data frame or matrix."
  )

  # Coerce items to numeric data
  X <- as.data.frame(df, stringsAsFactors = FALSE)
  X[] <- lapply(X, as.numeric)

  # Calculate and validate number of items
  k <- ncol(X)
  cli_assert(
    condition = k > 1,
    message = "At least two items are required."
  )

  # Calculate and validate pairwise covariance matrix
  S <- stats::cov(X, use = "pairwise.complete.obs")
  cli_assert(
    condition = all(!is.na(S)),
    message = "Missing values in cov; insufficient pairwise overlap?"
  )
  cli_assert(
    condition = all(diag(S) > 0),
    message = "All items must have positive variance."
  )
  cli_assert(
    condition = sum(S) > 0,
    message = "Data must have positive total variance."
  )

  # Calculate and return coefficient alpha
  alpha <- (k / (k - 1)) * (1 - sum(diag(S)) / sum(S))
  alpha
}

#' Compute McDonald's Omega (total) via CFA
#'
#' @description
#' `calc_omega()` computes McDonald's \eqn{\omega_\mathrm{total}} for a
#' unidimensional scale using a one-factor confirmatory factor analysis
#' (CFA) fit with **lavaan** using MLR estimation. Missing data can be
#' handled via full information maximum likelihood (FIML).
#'
#' @details
#' The model is `f =~ item1 + item2 + ... + itemK` with `std.lv = TRUE`
#' (i.e., \eqn{\mathrm{Var}(f)=1}).
#'
#'   \eqn{\omega = \frac{(\sum_i \lambda_i)^2}
#'   {(\sum_i \lambda_i)^2 + \sum_i \theta_i}}, where \eqn{\lambda_i} are
#'   *unstandardized* loadings and \eqn{\theta_i} are *unstandardized* residual
#'   variances of the indicators.
#'
#' The function validates inputs and fails with informative messages if the CFA
#' does not converge or if implied residual variances are invalid. Mixed-sign
#' loadings trigger a warning (often indicates reverse-keying is needed).
#'
#' @param df A data frame or matrix with one row per observation and one column
#'   per item.
#'
#' @return A numeric scalar giving McDonald;s \eqn{\omega_\mathrm{total}}
#'   (typically in \[0, 1\]; small estimation anomalies can yield values
#'   slightly outside this range).
#'
#' @references
#' - McDonald, R. P. (1999). *Test Theory: A Unified Treatment*. Lawrence
#'   Erlbaum.
#' - Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
#'   structural equation modeling: An alternative to coefficient alpha.
#'   *Psychometrika, 74*(1), 155–167.
#' - Zinbarg, R. E., Revelle, W., Yovel, I., & Li, W. (2005). Cronbach's α,
#'   Revelle's β, and McDonald's ωH. *Psychometrika, 70*(1), 123–133.
#' @export
calc_omega <- function(df) {
  # Check for dependencies
  cli_assert(
    condition = rlang::is_installed("lavaan"),
    message = "Package `lavaan` is required. Install it with install.packages('lavaan')."
  )

  # Validate df arg
  cli_assert(
    condition = is.data.frame(df) || is.matrix(df),
    message = "`df` must be a data frame or matrix."
  )

  # Coerce items to numeric data
  X <- as.data.frame(df, stringsAsFactors = FALSE)
  X[] <- lapply(X, as.numeric)

  # Calculate and validate number of items
  k <- ncol(X)
  cli_assert(
    condition = k > 1,
    message = "At least two items are required."
  )

  # Build unidimensional CFA
  item_names <- make.names(colnames(X), unique = TRUE)
  colnames(X) <- item_names
  model_syntax <- paste("f =~", paste(item_names, collapse = " + "))

  # Estimate CFA with MLR and FIML
  fit <- tryCatch(
    lavaan::cfa(
      model = model_syntax,
      data = X,
      std.lv = TRUE,
      estimator = "MLR",
      missing = "fiml"
    ),
    error = function(e) e
  )
  cli_assert(
    condition = !inherits(fit, "error"),
    message = "Continuous CFA failed to converge."
  )

  # Get and validate factor loadings
  params <- lavaan::parameterEstimates(fit, standardized = FALSE)
  lambda <- params$est[params$op == "=~" & params$lhs == "f"]
  cli_assert(
    condition = all(is.finite(lambda)),
    message = "Invalid loadings returned by CFA."
  )
  if (any(lambda > 0) && any(lambda < 0)) {
    warning(
      "Mixed-sign loadings detected; reverse scoring may have failed."
    )
  }

  # Get and validate item residual variances
  theta <- params$est[
    (params$op == "~~") &
      (params$lhs == params$rhs) &
      (params$lhs %in% item_names)
  ]
  cli_assert(
    condition = all(theta >= 0) && all(is.finite(theta)),
    message = "Negative or non-finite residual variances detected."
  )

  # Calculate and validate variance estimates
  explained_var <- (sum(lambda))^2
  total_var <- explained_var + sum(theta)
  cli_assert(
    condition = total_var > 0,
    message = "Non-positive total variance implied by the continuous CFA."
  )

  # Calculate and return continuous omega
  omega <- explained_var / total_var
  omega
}
