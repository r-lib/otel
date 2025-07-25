% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{local_active_span}
\alias{local_active_span}
\title{Activate an OpenTelemetry span for an R scope}
\usage{
local_active_span(span, end_on_exit = FALSE, activation_scope = parent.frame())
}
\arguments{
\item{span}{The OpenTelemetry span to activate.}

\item{end_on_exit}{Whether to end the span when exiting the activation
scope.}

\item{activation_scope}{The scope to activate the span for, defaults to
the caller frame.}
}
\value{
Nothing.
}
\description{
Activates the span for the caller (or other) frame.

Usually you need this function for spans created with \code{\link[=start_span]{start_span()}},
which does not activate the new span. Usually you don't need it for
spans created with \code{\link[=start_local_active_span]{start_local_active_span()}}, because it activates
the new span automatically.
}
\details{
When the frame ends, the span is deactivated and the previously active
span will be active again, if there was any.

It is possible to activate the same span for multiple R frames.
}
\examples{
fun <- function() {
  # start span, do not activate
  spn <- otel::start_span("myfun")
  # do not leak resources
  on.exit(otel::end_span(spn), add = TRUE)
  myfun <- function() {
     # activate span for this function
     otel::local_active_span(spn)
     # create child span
     spn2 <- otel::start_local_active_span("myfun/2")
  }

  myfun2 <- function() {
    # activate span for this function
    otel::local_active_span(spn)
    # create child span
    spn3 <- otel::start_local_active_span("myfun/3")
  }
  myfun()
  myfun2()
  end_span(spn)
}
fun()
}
\seealso{
Other OpenTelemetry trace API: 
\code{\link{Zero Code Instrumentation}},
\code{\link{end_span}()},
\code{\link{is_tracing_enabled}()},
\code{\link{start_local_active_span}()},
\code{\link{start_span}()},
\code{\link{tracing-constants}},
\code{\link{with_active_span}()}

Other tracing for concurrent code: 
\code{\link{with_active_span}()}
}
\concept{OpenTelemetry trace API}
\concept{tracing for concurrent code}
