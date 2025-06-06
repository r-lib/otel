% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny.R
\name{start_shiny_session}
\alias{start_shiny_session}
\title{Start tracing a Shiny session}
\usage{
start_shiny_session(session, attributes = NULL, options = NULL, ...)
}
\arguments{
\item{session}{Shiny session object.}

\item{attributes, options, ...}{Additional arguments are passed to
\verb{$start_span()} for the \code{session} span.}
}
\value{
The OpenTelemetry span corresponding to the Shiny session,
invisibly.
}
\description{
Call this function from the Shiny server function, at the beginning.
}
