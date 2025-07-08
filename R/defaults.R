# To ensure consistent naming across projects, this specification
# recommends that language specific environment variables are formed using
# the following convention:
# ```
# OTEL_{LANGUAGE}_{FEATURE}
# ```
# https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/#language-specific-environment-variables

default_traces_exporter_envvar <- "OTEL_TRACES_EXPORTER"
default_traces_exporter_envvar_r <- "OTEL_R_TRACES_EXPORTER"

default_logs_exporter_envvar <- "OTEL_LOGS_EXPORTER"
default_logs_exporter_envvar_r <- "OTEL_R_LOGS_EXPORTER"

default_metrics_exporter_envvar <- "OTEL_METRICS_EXPORTER"
default_metrics_exporter_envvar_r <- "OTEL_R_METRICS_EXPORTER"

#' Get the default tracer provider
#'
#' If there is no default set currently, then it creates and sets a
#' default.
#'
#' The default tracer provider is created based on the
#' `r default_traces_exporter_envvar_r` environment variable. This
#' environment variable is specifically for R applications with
#' OpenTelemetry support.
#'
#' If this is not set, then the generic `r default_traces_exporter_envvar`
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
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    tracer_provider_noop$new()                                       # safe
  })                                                                 # safe
}
# safe end

set_default_service_name <- function() {
  if (Sys.getenv("OTEL_SERVICE_NAME") == "") {
    Sys.setenv("OTEL_SERVICE_NAME" = "R")
  }
}

get_default_tracer_provider_safe <- get_default_tracer_provider

setup_default_tracer_provider <- function() {
  evar <- default_traces_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_traces_exporter_envvar
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
    set_default_service_name()
    tp$new()

  } else {
    set_default_service_name()
    switch(
      ev,
      "none" = {
        tracer_provider_noop$new()
      },
      "console" = ,
      "stdout" = {
        otelsdk::tracer_provider_stdstream$new(list(output = "stdout"))
      },
      "stderr" = {
        otelsdk::tracer_provider_stdstream$new(list(output = "stderr"))
      },
      "otlp" = ,
      "http" = {
        otelsdk::tracer_provider_http$new()
      },
      "otlp/file" = {
        otelsdk::tracer_provider_file$new()
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

#' Get the default tracer provider
#' TODO
#'
#' @export

# safe start
get_default_logger_provider <- function() {
  tryCatch({                                                         # safe
    if (is.null(the$logger_provider)) {
      setup_default_logger_provider()
    }
    the$logger_provider
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_provider_noop$new()                                       # safe
  })                                                                 # safe
}
# safe end

setup_default_logger_provider <- function() {
  evar <- default_logs_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_logs_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    logger_provider_noop$new()
  } else if (grepl("::", ev)) {
    evx <- strsplit(ev, "::", fixed = TRUE)[[1]]
    pkg <- evx[1]
    prv <- evx[2]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Cannot set logs exporter ", ev, " from ", evar,
        " environment variable, cannot load package ", pkg, "."
      )
    }
    if (!prv %in% names(asNamespace(pkg))) {
      stop(
        "Cannot set logs exporter ", ev, " from ", evar,
        " environment variable, cannot find provider ", prv,
        " in package ", pkg, "."
      )
    }
    tp <- asNamespace(pkg)[[prv]]
    if ((!is.list(tp) && !is.environment(tp)) || !"new" %in% names(tp)) {
      stop(
        "Cannot set logs exporter ", ev, " from ", evar,
        " environment variable, it is not a list or environment with ",
        "a 'new' member."
      )
    }
    set_default_service_name()
    tp$new()

  } else {
    set_default_service_name()
    switch(
      ev,
      "none" = {
        logger_provider_noop$new()
      },
      "console" = ,
      "stdout" = {
        otelsdk::logger_provider_stdstream$new(list(output = "stdout"))
      },
      "stderr" = {
        otelsdk::logger_provider_stdstream$new(list(output = "stderr"))
      },
      "otlp" = ,
      "http" = {
        otelsdk::logger_provider_http$new()
      },
      "otlp/file" = {
        otelsdk::logger_provider_file$new()
      },
      stop(
        "Unknown OpenTelemetry exporter from ", evar,
        " environment variable: ", ev
      )
    )
  }

  the$logger_provider <- tp
  invisible(tp)
}

#' Get the default metrics provider
#' TODO
#'
#' @export

# safe start
get_default_meter_provider <- function() {
  tryCatch({                                                         # safe
    if (is.null(the$meter_provider)) {
      setup_default_meter_provider()
    }
    the$meter_provider
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    meter_provider_noop$new()                                       # safe
  })                                                                 # safe
}
# safe end

setup_default_meter_provider <- function() {
  evar <- default_metrics_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_metrics_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    meter_provider_noop$new()
  } else if (grepl("::", ev)) {
    evx <- strsplit(ev, "::", fixed = TRUE)[[1]]
    pkg <- evx[1]
    prv <- evx[2]
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Cannot set metrics exporter ", ev, " from ", evar,
        " environment variable, cannot load package ", pkg, "."
      )
    }
    if (!prv %in% names(asNamespace(pkg))) {
      stop(
        "Cannot set metrics exporter ", ev, " from ", evar,
        " environment variable, cannot find provider ", prv,
        " in package ", pkg, "."
      )
    }
    tp <- asNamespace(pkg)[[prv]]
    if ((!is.list(tp) && !is.environment(tp)) || !"new" %in% names(tp)) {
      stop(
        "Cannot set metrics exporter ", ev, " from ", evar,
        " environment variable, it is not a list or environment with ",
        "a 'new' member."
      )
    }
    set_default_service_name()
    tp$new()

  } else {
    set_default_service_name()
    switch(
      ev,
      "none" = {
        meter_provider_noop$new()
      },
      "console" = ,
      "stdout" = {
        otelsdk::meter_provider_stdstream$new(list(output = "stdout"))
      },
      "stderr" = {
        otelsdk::meter_provider_stdstream$new(list(output = "stderr"))
      },
      "otlp" = ,
      "http" = {
        otelsdk::meter_provider_http$new()
      },
      "otlp/file" = {
        otelsdk::meter_provider_file$new()
      },
      "prometheus" = {
        warning("OpenTelemetry: Prometheus trace exporter is not supported yet")
        meter_provider_noop$new()
      },
      stop(
        "Unknown OpenTelemetry exporter from ", evar,
        " environment variable: ", ev
      )
    )
  }

  the$meter_provider <- tp
  invisible(tp)
}
