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

#' Environment variables to configure otel
#' @name Environment Variables
#' @rdname environmentvariables
#'
#' @description
#' This manual page contains the environment variables you can use to
#' configure the otel package. Start with the 'Selecting exporters' section
#' below if you want to produce telemetry data for an instrumented R
#' package.
#'
#' See also the \link[otelsdk:Environment Variables]{Environment Variables}
#' in the otelsdk package, which is charge of the data collection
#' configuration.
#'
#' @details
#' You need set these environment variables when configuring the
#' collection of telemetry data, unless noted otherwise.
#'
#' ## Production or Development Environment
#'
#' ### `OTEL_ENV`
#'
#' By default otel runs in production mode. In production mode otel
#' functions never error. Errors in the telemetry code will not stop
#' the monitored application.
#'
#' This behavior is not ideal for development, where one would prefer
#' to catch errors early. Set
#' ```
#' OTEL_ENV=dev
#' ```
#' to run otel in development mode, where otel functions fail on error,
#' make it easier to fix errors.
#'
#' ## Selecting Exporters
#'
#' otel is responsible for selecting the providers to use for traces,
#' logs and metrics. You can use the environment variables below to
#' point the otel functions to the desired providers.
#'
#' If none of these environment variables are set, then otel will not
#' emit any telemetry data.
#'
#' See the [otelsdk](https://github.com/r-lib/otelsdk) package for
#' configuring the selected providers.
#'
#' ### ``r default_traces_exporter_envvar``
#'
#' The name of the selected tracer provider. See
#' [get_default_tracer_provider()] for the possible values.
#'
#' ### ``r default_traces_exporter_envvar_r``
#'
#' R specific version of ``r default_traces_exporter_envvar``.
#'
#' ### ``r default_logs_exporter_envvar``
#'
#' The name of the selected logger provider. See
#' [get_default_logger_provider()] for the possible values.
#'
#' ### ``r default_logs_exporter_envvar_r``
#'
#' R specific version of ``r default_logs_exporter_envvar``.
#'
#' ### ``r default_metrics_exporter_envvar``
#'
#' The name of the selected meter provider. See
#' [get_default_meter_provider()] for the possible values.
#'
#' ### ``r default_metrics_exporter_envvar_r``
#'
#' R specific version of ``r default_metrics_exporter_envvar``.
#'
#' ## Suppressing Instrumentation Scopes (R Packages)
#'
#' otel has two environment variables to fine tune which instrumentation
#' scopes (i.e. R packages, typically) emit telemetry data. By default,
#' i.e. if neither of these are set, all packages emit telemetry data.
#'
#' ### ``r otel_emit_scopes_envvar``
#'
#' Set this environment variable to a comma separated string of
#' instrumentation scope names or R package names to restrict telemetry to
#' these packages only. The name of the instrumentation scope is the same
#' as the name of the tracer, logger or meter, see [default_tracer_name()].
#'
#' You can mix package names and instrumentation scope names and you can
#' also use wildcards (globbing). For example the value
#'
#' `r paste0(otel_emit_scopes_envvar, '="org.r-lib.*,dplyr"')`
#'
#' selects all packages with an instrumentation scope that starts with
#' `org.r-lib.` and also dplyr.
#'
#' ### ``r otel_suppress_scopes_envvar``
#'
#' Set this environment variable to a comma separated string of
#' instrumentation scope names or R package names to suppress telemetry
#' data from these packages. The name of the instrumentation scope is the same
#' as the name of the tracer, logger or meter, see [default_tracer_name()].
#'
#' You can mix package names and instrumentation scope names and you can
#' also use wildcards (globbing). For example the value
#'
#' `r paste0(otel_suppress_scopes_envvar, '="org.r-lib.*,dplyr"')`
#'
#' excludes packages with an instrumentation scope that starts with
#' `org.r-lib.` and also dplyr.
#'
#' ## [Zero Code Instrumentation]
#'
#' otel can instrument R packages for OpenTelemetry data collection
#' without changing their source code. This relies on changing the code
#' of the R functions manually using `base::trace()` and can be configured
#' using environment variables.
#'
#' ### `OTEL_R_INSTRUMENT_PKGS`
#'
#' Set `OTEL_R_INSTRUMENT_PKGS` to a comma separated list of packages to
#' instrument. The automatic instrumentation happens when the otel package
#' is loaded, so in general it is best to set this environment variable
#' before loading R.
#'
#' ### `OTEL_R_INSTRUMENT_PKGS_<pkg>_INCLUDE`
#'
#' For an automatically instrumented package, set this environment variable
#' to only instrument a subset of its functions. It is parsed as a comma
#' separated string of function names, which may also include `?` and `*`
#' wildcards (globbing).
#'
#' ### `OTEL_R_INSTRUMENT_PKGS_<pkg>_EXCLUDE`
#'
#' For an automatically instrumented package, set this environment variable
#' to exclude some functions from instrumentation. It has the same syntax
#' as its `*_INCLUDE` pair. If both are set, then inclusion is applied
#' and the exclusion.
#'
#' ## Others
#'
#' ### ``r otel_attr_cnt_limit_var``
#'
#' Set this environment variable to limit the number of attributes for a
#' single span, log record, metric measurement, etc. If unset, the default
#' limit is `r otel_attr_cnt_limit_dflt` attributes.
#'
#' ### ``r otel_attr_val_lth_limit_var``
#'
#' Set this environment variable to limit the length of vectors in
#' attributes for a single span, log record, metric measurement, etc.
#' If unset, there is no limit on the lengths of vectors in attributes.
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
