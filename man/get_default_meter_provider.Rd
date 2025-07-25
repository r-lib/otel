% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defaults.R
\name{get_default_meter_provider}
\alias{get_default_meter_provider}
\alias{setup_default_meter_provider}
\title{Get the default meter provider}
\usage{
get_default_meter_provider()
}
\value{
The default meter provider, an \link{otel_meter_provider}
object.
}
\description{
The meter provider defines how metrics are exported when collecting
telemetry data. It is unlikely that you need to call this function
directly, but read on to learn how to configure which exporter to
use.
}
\details{
If there is no default set currently, then it creates and sets a
default.

The default meter provider is created based on the
OTEL_R_METRICS_EXPORTER environment variable. This
environment variable is specifically for R applications with
OpenTelemetry support.

If this is not set, then the generic OTEL_METRICS_EXPORTER
environment variable is used. This applies to all applications that
support OpenTelemetry and use the OpenTelemetry SDK.

The following values are allowed:
\itemize{
\item \code{none}: no metrics are exported.
\item \code{stdout} or \code{console}: uses \link[otelsdk:meter_provider_stdstream]{otelsdk::meter_provider_stdstream},
to write metrics to the standard output.
\item \code{stderr}: uses \link[otelsdk:meter_provider_stdstream]{otelsdk::meter_provider_stdstream}, to write
metrics to the standard error.
\item \code{http} or \code{otlp}: uses \link[otelsdk:meter_provider_http]{otelsdk::meter_provider_http}, to send
metrics through HTTP, using the OpenTelemetry Protocol (OTLP).
\item \code{otlp/file} uses \link[otelsdk:meter_provider_file]{otelsdk::meter_provider_file} to write metrics
to a JSONL file.
\item \verb{<package>::<provider>}: will select the \verb{<provider>} object from
the \verb{<package>} package to use as a meter provider. It calls
\verb{<package>::<provider>$new()} to create the new meter provider.
If this fails for some reason, e.g. the package is not installed,
then it throws an error.
}
}
\examples{
get_default_meter_provider()
}
\seealso{
Other low level metrics API: 
\code{\link{get_meter}()},
\code{\link{meter_provider_noop}},
\code{\link{otel_counter}},
\code{\link{otel_gauge}},
\code{\link{otel_histogram}},
\code{\link{otel_meter}},
\code{\link{otel_meter_provider}},
\code{\link{otel_up_down_counter}}
}
\concept{low level metrics API}
