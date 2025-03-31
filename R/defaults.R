default_tracer_exporter_envvar <- "OTEL_TRACES_EXPORTER"
default_tracer_exporter_envvar_r <-
  paste0("R_", default_tracer_exporter_envvar)

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
#' @return The default tracer provider, an `otel_tracer_provider`
#'   object.
#' @export

# safe start
get_default_tracer_provider <- function() {
  tryCatch({                                                         # safe
    if (is.null(the$tracer_provider)) {
      setup_default_tracer_provider()
    }
    the$tracer_provider
  }, error = function(err) {                                         # safe
    msg("OpenTelemetry error: ", conditionMessage(err))              # safe
    tracer_provider_noop$new()                                       # safe
  })                                                                 # safe
}
# safe end

get_default_tracer_provider_safe <- get_default_tracer_provider

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

  the$tracer_provider <- tp
  invisible(tp)
}
