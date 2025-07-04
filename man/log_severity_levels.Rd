% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\docType{data}
\name{log_severity_levels}
\alias{log_severity_levels}
\title{OpenTelemetry log severity levels}
\format{
An object of class \code{integer} of length 24.
}
\usage{
log_severity_levels
}
\description{
A named integer vector, the severity levels in numeric form.
The names are the severity levels in text form. otel functions accept
both forms as severity levels, but the text form is more readable.
}
\seealso{
Other OpenTelemetry logging: 
\code{\link{get_logger}()},
\code{\link{is_logging}()},
\code{\link{log}()}
}
\concept{OpenTelemetry constants}
\concept{OpenTelemetry logging}
\keyword{datasets}
