default_tracer_provider_option <- "opentelemetry.default_tracer_provider"
default_tracer_exporter_envvar <- "OTEL_TRACES_EXPORTER"
default_tracer_exporter_envvar_r <-
  paste0("R_", default_tracer_exporter_envvar)

#' Set the default tracer provider
#' @param tracer_provider An OpenTelemetry tracer provider
#'   (`opentelemetry_tracer_provider` object) to set as default.
#' @return The previously set default tracer provider, or `NULL`, if no
#'   default was set before.
#'
#' @export

set_default_tracer_provider <- function(tracer_provider) {
  if (!inherits(tracer_provider, "opentelemetry_tracer_provider")) {
    stop(
      "Cannot set default OpenTelemetry tracer provider, not an ",
      "opentelemetry_tracer_provider object"
    )
  }
  old <- getOption(default_tracer_provider_option)
  options(structure(
    list(tracer_provider),
    names = default_tracer_provider_option
  ))
  invisible(old)
}

#' Get the default tracer provider
#'
#' If there is no default set currently, then it creates and sets a
#' default.
#'
#' The default tracer provider is created based on the
#' `r default_tracer_exporter_envvar_r` environment variable. This
#' environment variable is specifically for R applications with
#' OpenTelemetry support.
#'
#' If this is not set, then the generic `r default_tracer_exporter_envvar`
#' environment variable is used. This applies to all applications that
#' support OpenTelemetry and use the OpenTelemetry SDK.
#'
#' The following values are allowed:
#' - `none`: no traces are exported.
#' - `stdout` or `console`: uses [otelsdk::tracer_provider_stdstream],
#'   to write traces to the standard output.
#' - `stderr`: uses [otelsdk::tracer_provider_stdstream], to write traces
#'   to the standard error.
#' - `http` or `otlp`: uses [otelsdk::tracer_provider_http], to send
#'   traces through HTTP, using the OpenTelemetry Protocol (OTLP).
#' - `<package>::<provider>`: will select the `<provider>` object from
#'   the `<package>` package to use as a trace exporter. It calls
#'   `<package>::<provider>$new()` to create the new traver provider.
#'   If this fails for some reason, e.g. the package is not installed,
#'   then it throws an error.
#'
#' @return The default tracer provider, an `opentelemetry_tracer_provider`
#'   object.
#' @export

get_default_tracer_provider <- function() {
  tp <- getOption(default_tracer_provider_option)
  if (is.null(tp)) {
    setup_default_tracer_provider()
  }
  getOption(default_tracer_provider_option)
}

setup_default_tracer_provider <- function() {
  evar <- default_tracer_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_tracer_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    tracer_provider_noop$new()
  } else if (grepl("::", ev)) {
    evx <- strsplit(ev, "::", fixed = TRUE)[[1]]
    pkg <- evx[1]
    prv <- evx[2]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Cannot set trace exporter ", ev, " from ", evar,
        " environment variable, cannot load package ", pkg, "."
      )
    }
    if (!prv %in% names(asNamespace(pkg))) {
      stop(
        "Cannot set trace exporter ", ev, " from ", evar,
        " environment variable, cannot find provider ", prv,
        " in package ", pkg, "."
      )
    }
    tp <- asNamespace(pkg)[[prv]]
    if ((!is.list(tp) && !is.environment(tp)) || !"new" %in% names(tp)) {
      stop(
        "Cannot set trace exporter ", ev, " from ", evar,
        " environment variable, it is not a list or environment with ",
        "a 'new' member."
      )
    }
    tp$new()

  } else {
    switch(
      ev,
      "none" = {
        tracer_provider_noop$new()
      },
      "console" = ,
      "stdout" = {
        otelsdk::tracer_provider_stdstream$new("stdout")
      },
      "stderr" = {
        otelsdk::tracer_provider_stdstream$new("stderr")
      },
      "otlp" = ,
      "http" = {
        otelsdk::tracer_provider_http$new()
      },
      "jaeger" = {
        warning("OpenTelemetry: Jaeger trace exporter is not supported yet")
        tracer_provider_noop$new()
      },
      "zipkin" = {
        warning("OpenTelemetry: Zipkin trace exporter is not supported yet")
        tracer_provider_noop$new()
      },
      stop(
        "Unknown OpenTelemetry exporter from ", evar,
        " environment variable: ", ev
      )
    )
  }

  options(structure(list(tp), names = default_tracer_provider_option))
  invisible(tp)
}
