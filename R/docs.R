#' @name Getting Started
#' @title Getting Started
#' @rdname gettingstarted
#' @aliases gettingstarted
#' @description
#' This page is about instrumenting you R package or project for
#' OpenTelemetry. If you want to start collecting OpenTelemetry data
#' for instrumented packages, see \link[otelsdk:Collecting Telemetry Data]{
#' Collecting Telemetry Data} in the otelsdk package.
#'
#' @details
#' ```{r child = "tools/dox/gettingstarted.Rmd"}
#' ```
#'
#' @examples
#' # See above
NULL

doc_evs_exporter <- function() {
  c(
    default_traces_exporter_envvar,
    default_traces_exporter_envvar_r,
    default_logs_exporter_envvar,
    default_logs_exporter_envvar_r,
    default_metrics_exporter_envvar,
    default_metrics_exporter_envvar_r
  )
}

doc_evs_suppress <- function() {
  c(
    otel_emit_scopes_envvar,
    otel_suppress_scopes_envvar
  )
}

doc_evs <- function() {
  paste(
    collapse = " ",
    c(
      doc_evs_exporter(),
      doc_evs_suppress()
    )
  )
}

#' Environment variables to configure otel
#' @name Environment Variables
#' @rdname environmentvariables
#' @eval paste("@aliases", "OTEL_ENV", doc_evs())
#'
#' @description
#' This manual page contains the environment variables you can use to
#' configure the otel package.
#'
#' See also the \link[otelsdk:Environment Variables]{Environment Variables}
#' in the otelsdk package, which is charge of the data collection
#' configuration.
#'
#' You need set these environment variables when configuring the
#' collection of telemetry data, unless noted otherwise.
#'
#' # Production or Development Environment
#'
#' * `OTEL_ENV`
#'
#'   By default otel runs in production mode. In production mode otel
#'   functions never error. Errors in the telemetry code will not stop
#'   the monitored application.
#'
#'   This behavior is not ideal for development, where one would prefer
#'   to catch errors early. Set
#'   ```
#'   OTEL_ENV=dev
#'   ```
#'   to run otel in development mode, where otel functions fail on error,
#'   make it easier to fix errors.
#'
#' ```{r child = system.file(package = "otel", "dox/ev-exporters.Rmd")}
#' ```
#'
#' ```{r child = system.file(package = "otel", "dox/ev-suppress.Rmd")}
#' ```
#'
#' ```{r child = system.file(package = "otel", "dox/ev-zci.Rmd")}
#' ```
#'
#' ```{r child = system.file(package = "otel", "dox/ev-others.Rmd")}
#' ```
#'
#' @return Not applicable.
#' @seealso \link[otelsdk:Environment Variables]{Environment Variables} in
#' otelsdk
#' @examples
#' # To start an R session using the OTLP exporter:
#' # OTEL_TRACES_EXPORTER=http R -q -f script.R
NULL

#' Zero Code Instrumentation
#'
#' otel supports zero-code instrumentation (ZCI) via the
#' `OTEL_INSTRUMENT_R_PKGS` environment variable. Set this to a comma
#' separated list of package names, the packages that you want to
#' instrument. Then otel will hook up [base::trace()] to produce
#' OpenTelemetry output from every function of these packages.
#'
#' By default all functions of the listed packages are instrumented. To
#' instrument a subset of all functions set the
#' `OTEL_INSTRUMENT_R_PKGS_<PKG>_INCLUDE` environment variable to a list of
#' glob expressions. `<PKG>` is the package name in all capital letters.
#' Only functions that match to at least one glob expression will be
#' instrumented.
#'
#' To exclude functions from instrumentation, set the
#' `OTEL_INSTRUMENT_R_PKGS_<PKG>_EXCLUDE` environment variable to a list of
#' glob expressions. `<PKG>` is the package name in all capital letters.
#' Functions that match to at least one glob expression will not be
#' instrumented. Inclusion globs are applied before exclusion globs.
#'
#' ## Caveats
#'
#' If the user calls [base::trace()] on an instrumented function, that
#' deletes the instrumentation, since the second [base::trace()] call
#' overwrites the first.
#'
#' @name Zero Code Instrumentation
#' @rdname zci
#' @family OpenTelemetry trace API
#' @seealso [Environment Variables]
#' @return Not applicable.
#' @aliases OTEL_R_INSTRUMENT_PKGS
#' @examples
#' # To run an R script with ZCI:
#' # OTEL_TRACES_EXPORTER=http OTEL_INSTRUMENT_R_PKGS=dplyr,tidyr R -q -f script.R
NULL

doc_arg <- function() {
  list(
    "span-name" = "Name of the span. If not specified it will be `\"<NA>\"`.",

    links = "A named list of links to other spans. Every link must be an
     OpenTelemetry span ([otel_span]) object, or a list with a span
     object as the first element and named span attributes as the rest.",

    attributes = glue::glue(
      trim = FALSE,
      "Span attributes. OpenTelemetry supports the following
     R types as attributes: `{paste0(otel_attr_types, collapse = \", \")}.
     You may use [as_attributes()] to convert other R types to
     OpenTelemetry attributes."
    ),

    # need to use \itemize, markdown does not put this list under the other
    # one for otel_tracer method arguments
    "span-options" = glue::glue(
      trim = FALSE,
      "A named list of span options. May include: \\itemize{{
       \\item `start_system_time`: Start time in system time.
       \\item `start_steady_time`: Start time using a steady clock.
       \\item `parent`: A parent span or span context. If it is `NA`, then the
       span has no parent and it will be a root span. If it is `NULL`, then
       the current context is used, i.e. the active span, if any.
       \\item `kind`: Span kind, one of [span_kinds]:
       {paste(dQuote(span_kinds, q = FALSE), collapse = ', ')}.}}"
    ),

    "unit" = "Optional measurement unit. If specified, it should use
      units from [Unified Code for Units of Measure](https://ucum.org/),
      according to the [OpenTelemetry semantic conventions](
      https://opentelemetry.io/docs/specs/semconv/general/metrics/#instrument-units)."
  )
}
