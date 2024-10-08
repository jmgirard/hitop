% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pid5_fsf.R
\name{score_pid5fsf}
\alias{score_pid5fsf}
\title{Score the PID-5-FSF Domain and Facet Scales}
\usage{
score_pid5fsf(
  .data,
  items = NULL,
  id = NULL,
  scales = c("domains", "facets"),
  tibble = FALSE
)
}
\arguments{
\item{.data}{A data frame containing all PID-5-FSF items (numerically
scored).}

\item{items}{An optional vector of column names (as strings) or numbers (as
integers) corresponding to the 100 PID-5-FSF items in order. If set to
\code{NULL} (the default), all non-\code{id} columns will be assumed to be the
\code{items} in order.}

\item{id}{An optional vector of column names (as strings) or numbers (as
integers) corresponding to variables from \code{.data} to keep in the output. If
set to \code{NULL} (the default), no columns will be retained.}

\item{scales}{An optional character vector indicating whether to calculate
domain scores, facets scores, or both. Matching allows users to specify
partial arguments such as "d" or "f" (default is both).}

\item{tibble}{An optional logical indicating whether the output should be
converted to a \code{tibble::tibble()}.}
}
\value{
A data frame containing any \code{id} variables as well any requested
\code{scale} scores, calculated using the DSM-5 algorithm.
}
\description{
Create a data frame with scores on the PID-5 Faceted Short Form domain and
facet scales.
}
\references{
Maples, J. L., Carter, N. T., Few, L. R., Crego, C., Gore, W.
L., Samuel, D. B., Williamson, R. L., Lynam, D. R., Widiger, T. A., Markon,
K. E., Krueger, R. F., & Miller, J. D. (2015). Testing whether the DSM-5
personality disorder trait model can be measured with a reduced set of
items: An item response theory investigation of the personality inventory
for DSM-5. \emph{Psychological Assessment, 27}(4), 1195-1210.
\url{https://doi.org/10.1037/pas0000120}
}
