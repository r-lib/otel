% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{start_span}
\alias{start_span}
\title{Start a new OpenTelemetry span, using the default tracer}
\usage{
start_span(name = NULL, session = NULL, ..., scope = parent.frame())
}
\arguments{
\item{name}{Name of the span.}

\item{session}{Optionally, an OpenTelemetry session to activate before
starting the span. It can also be a Shiny session (\code{ShinySession}
object), that was previously used as an argument to
\code{\link[=start_shiny_session]{start_shiny_session()}}.}

\item{..., scope}{Additional arguments are passed to the default tracer's
\code{start_span()} method.}
}
\value{
The new Opentelemetry span object, invisibly.
}
\description{
Start a new OpenTelemetry span, using the default tracer
}
