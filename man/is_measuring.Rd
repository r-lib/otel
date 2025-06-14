% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{is_measuring}
\alias{is_measuring}
\title{Check whether OpenTelemetry metrics collection is active}
\usage{
is_measuring()
}
\value{
\code{TRUE} is OpenTelemetry metrics collection  is active,
\code{FALSE} otherwise.
}
\description{
Check whether OpenTelemetry metrics collection is active
}
\seealso{
Other OpenTelemetry metrics: 
\code{\link{counter_add}()},
\code{\link{gauge_record}()},
\code{\link{get_meter}()},
\code{\link{histogram_record}()},
\code{\link{up_down_counter_add}()}
}
\concept{OpenTelemetry metrics}
