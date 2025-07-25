% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defaults.R
\name{get_default_logger_provider}
\alias{get_default_logger_provider}
\alias{setup_default_logger_provider}
\title{Get the default logger provider}
\usage{
get_default_logger_provider()
}
\value{
The default logger provider, an \link{otel_logger_provider}
object.
}
\description{
The logger provider defines how logs are exported when collecting
telemetry data. It is unlikely that you need to call this function
directly, but read on to learn how to configure which exporter to
use.
}
\details{
If there is no default set currently, then it creates and sets a
default.

The default logger provider is created based on the
OTEL_R_LOGS_EXPORTER environment variable. This
environment variable is specifically for R applications with
OpenTelemetry support.

If this is not set, then the generic OTEL_LOGS_EXPORTER
environment variable is used. This applies to all applications that
support OpenTelemetry and use the OpenTelemetry SDK.

The following values are allowed:
\itemize{
\item \code{none}: no traces are exported.
\item \code{stdout} or \code{console}: uses \link[otelsdk:logger_provider_stdstream]{otelsdk::logger_provider_stdstream},
to write traces to the standard output.
\item \code{stderr}: uses \link[otelsdk:logger_provider_stdstream]{otelsdk::logger_provider_stdstream}, to write traces
to the standard error.
\item \code{http} or \code{otlp}: uses \link[otelsdk:logger_provider_http]{otelsdk::logger_provider_http}, to send
traces through HTTP, using the OpenTelemetry Protocol (OTLP).
\item \code{otlp/file} uses \link[otelsdk:logger_provider_file]{otelsdk::logger_provider_file} to write logs
to a JSONL file.
\item \verb{<package>::<provider>}: will select the \verb{<provider>} object from
the \verb{<package>} package to use as a logger provider. It calls
\verb{<package>::<provider>$new()} to create the new logger provider.
If this fails for some reason, e.g. the package is not installed,
then it throws an error.
}
}
\examples{
get_default_logger_provider()
}
\seealso{
Other low level logs API: 
\code{\link{get_logger}()},
\code{\link{logger_provider_noop}},
\code{\link{otel_logger}},
\code{\link{otel_logger_provider}}
}
\concept{low level logs API}
