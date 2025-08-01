% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{is_tracing_enabled}
\alias{is_tracing_enabled}
\title{Check if tracing is active}
\usage{
is_tracing_enabled(tracer = NULL)
}
\arguments{
\item{tracer}{Tracer object (\link{otel_tracer}). It can also be a tracer
name, the instrumentation scope, or \code{NULL} for determining the tracer
name automatically. Passed to \code{\link[=get_tracer]{get_tracer()}} if not a tracer object.}
}
\value{
\code{TRUE} is OpenTelemetry tracing is active, \code{FALSE} otherwise.
}
\description{
Checks whether OpenTelemetry tracing is active. This can be useful
to avoid unnecessary computation when tracing is inactive.
}
\details{
It calls \code{\link[=get_tracer]{get_tracer()}} with \code{name} and then it calls the tracer's
\verb{$is_enabled()} method.
}
\examples{
fun <- function() {
  if (otel::is_tracing_enabled()) {
    xattr <- calculate_some_extra_attributes()
    otel::start_local_active_span("fun", attributes = xattr)
  }
  # ...
}
}
\seealso{
Other OpenTelemetry trace API: 
\code{\link{Zero Code Instrumentation}},
\code{\link{end_span}()},
\code{\link{local_active_span}()},
\code{\link{start_local_active_span}()},
\code{\link{start_span}()},
\code{\link{tracing-constants}},
\code{\link{with_active_span}()}
}
\concept{OpenTelemetry trace API}
