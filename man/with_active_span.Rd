% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{with_active_span}
\alias{with_active_span}
\title{Evaluate R code with an active OpenTelemetry span}
\usage{
with_active_span(span, expr)
}
\arguments{
\item{span}{The OpenTelemetry span to activate.}

\item{expr}{R expression to evaluate.}
}
\value{
The return value of \code{expr}.
}
\description{
Evaluate R code with an active OpenTelemetry span
}
