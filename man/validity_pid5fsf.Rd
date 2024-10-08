% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pid5_fsf.R
\name{validity_pid5fsf}
\alias{validity_pid5fsf}
\title{Score the PID-5-FSF Validity Scales}
\usage{
validity_pid5fsf(
  .data,
  items = NULL,
  id = NULL,
  scales = c("PNA", "INCS", "ORSS", "PRDS", "SDTDS"),
  srange = c(0, 3),
  tibble = FALSE
)
}
\arguments{
\item{.data}{A data frame containing all PID-5 items (numerically scored).}

\item{items}{An optional vector of column names (as strings) or numbers (as
integers) corresponding to the 100 PID-5 items in order. If set to \code{NULL}
(the default), all non-\code{id} columns will be assumed to be the \code{items} in
order.}

\item{id}{An optional vector of column names (as strings) or numbers (as
integers) corresponding to variables from \code{.data} to keep in the output. If
set to \code{NULL} (the default), no columns will be retained.}

\item{scales}{An optional character vector indicating which of the following
validity scales to calculate: percent of missing items (\code{"PNA"}), response
inconsistency scale short form (\code{"INCS"}), over-reporting scale short form
(\code{"ORSS"}), positive impression management response distortion scale short
form (\code{"PRDS"}), and social desirability-total denial short form
(\code{"SDTDS"}). See details below for interpretation guidance}

\item{srange}{An optional numeric vector specifying the minimum and maximum
values of the PID-5 items, used for reverse-coding. (default = \code{c(0, 3)})}

\item{tibble}{An optional logical indicating whether the output should be
converted to a \code{tibble::tibble()}.}
}
\value{
A data frame containing any \code{id} variables as well any requested
\code{scale} scores.
}
\description{
Create a data frame with scores on the PID-5 Faceted Short Form validity
scales.
}
\details{
Scores of 8 or more on the INC-S are indicative of inconsistent
responding. Cut-scores for the ORS-S, PRD-S, and SD-TD-S have not yet been
validated.
}
\references{
\itemize{
\item Lowmaster, S. E., Hartman, M. J., Zimmermann, J., Baldock, Z.
C., & Kurtz, J. E. (2020). Further Validation of the Response Inconsistency
Scale for the Personality Inventory for DSM-5. \emph{Journal of Personality
Assessment, 102}(6), 743–750.
\cite{https://doi.org/10.1080/00223891.2019.1674320}
}
}
