% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{get_active_span_context}
\alias{get_active_span_context}
\title{Returns the active span context}
\usage{
get_active_span_context()
}
\value{
The active span context, an \link{otel_span_context} object.
If there is no active span context, then an invalid span context is
returned, i.e. \code{spc$is_valid()} will be \code{FALSE} for the returned \code{spc}.
}
\description{
This is sometimes useful for logs or metrics, to associate
logging and metrics reporting with traces.
}
\details{
Note that logs and metrics instruments automatically use the current
span context, so often you don't need to call this function explicitly.
}
\examples{
fun <- function() {
  otel::start_local_active_span("fun")
  fun2()
}
fun2 <- function() {
  otel::log("Log message", span_context = otel::get_active_span_context())
}
fun()
}
