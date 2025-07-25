% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{counter_add}
\alias{counter_add}
\title{Increase an OpenTelemetry counter}
\usage{
counter_add(name, value = 1L, attributes = NULL, context = NULL, meter = NULL)
}
\arguments{
\item{name}{Name of the counter.}

\item{value}{Value to add to the counter, defaults to 1.}

\item{attributes}{Additional attributes to add.}

\item{context}{Span context. If missing the active context is used,
if any.}

\item{meter}{Meter object (\link{otel_meter}). Otherwise it is passed to
\code{\link[=get_meter]{get_meter()}} to get a meter.}
}
\value{
The counter object (\link{otel_counter}), invisibly.
}
\description{
Increase an OpenTelemetry counter
}
\examples{
otel::counter_add("total-session-count", 1)
}
\seealso{
Other OpenTelemetry metrics instruments: 
\code{\link{gauge_record}()},
\code{\link{histogram_record}()},
\code{\link{up_down_counter_add}()}

Other OpenTelemetry metrics API: 
\code{\link{gauge_record}()},
\code{\link{histogram_record}()},
\code{\link{is_measuring_enabled}()},
\code{\link{up_down_counter_add}()}
}
\concept{OpenTelemetry metrics API}
\concept{OpenTelemetry metrics instruments}
