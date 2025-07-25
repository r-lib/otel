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
#' The tracer provider defines how traces are exported when collecting
#' telemetry data. It is unlikely that you need to call this function
#' directly, but read on to learn how to configure which exporter to
#' use.
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
#' @return The default tracer provider, an [otel_tracer_provider]
#'   object. See [otel_tracer_provider] for its methods.
#'
#' @family low level trace API
#' @examples
#' get_default_tracer_provider()
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

#' @rdname get_default_tracer_provider
#' @usage NULL
#' @details
#' The following values are allowed:

setup_default_tracer_provider <- function() {
  evar <- default_traces_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_traces_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    tracer_provider_noop$new()
  } else if (!grepl("::", ev)) {
    set_default_service_name()
    switch(
      ev,
      #' - `none`: no traces are exported.
      "none" = {
        tracer_provider_noop$new()
      },
      #' - `stdout` or `console`: uses [otelsdk::tracer_provider_stdstream],
      #'   to write traces to the standard output.
      "console" = ,
      "stdout" = {
        otelsdk::tracer_provider_stdstream$new(list(output = "stdout"))
      },
      #' - `stderr`: uses [otelsdk::tracer_provider_stdstream], to write traces
      #'   to the standard error.
      "stderr" = {
        otelsdk::tracer_provider_stdstream$new(list(output = "stderr"))
      },
      #' - `http` or `otlp`: uses [otelsdk::tracer_provider_http], to send
      #'   traces through HTTP, using the OpenTelemetry Protocol (OTLP).
      "otlp" = ,
      "http" = {
        otelsdk::tracer_provider_http$new()
      },
      #' - `otlp/file` uses [otelsdk::tracer_provider_file] to write traces
      #'    to a JSONL file.
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
  } else {
    #' - `<package>::<provider>`: will select the `<provider>` object from
    #'   the `<package>` package to use as a tracer provider. It calls
    #'   `<package>::<provider>$new()` to create the new tracer provider.
    #'   If this fails for some reason, e.g. the package is not installed,
    #'   then it throws an error.
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
  }

  the$tracer_provider <- tp
  invisible(tp)
}

#' Get the default logger provider
#'
#' The logger provider defines how logs are exported when collecting
#' telemetry data. It is unlikely that you need to call this function
#' directly, but read on to learn how to configure which exporter to
#' use.
#'
#' If there is no default set currently, then it creates and sets a
#' default.
#'
#' The default logger provider is created based on the
#' `r default_logs_exporter_envvar_r` environment variable. This
#' environment variable is specifically for R applications with
#' OpenTelemetry support.
#'
#' If this is not set, then the generic `r default_logs_exporter_envvar`
#' environment variable is used. This applies to all applications that
#' support OpenTelemetry and use the OpenTelemetry SDK.
#'
#' @return The default logger provider, an [otel_logger_provider]
#'   object.
#' @family low level logs API
#' @examples
#' get_default_logger_provider()
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

get_default_logger_provider_safe <- get_default_logger_provider

#' @rdname get_default_logger_provider
#' @usage NULL
#' @details
#' The following values are allowed:

setup_default_logger_provider <- function() {
  evar <- default_logs_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_logs_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    logger_provider_noop$new()
  } else if (!grepl("::", ev)) {
    set_default_service_name()
    switch(
      ev,
      #' - `none`: no traces are exported.
      "none" = {
        logger_provider_noop$new()
      },
      #' - `stdout` or `console`: uses [otelsdk::logger_provider_stdstream],
      #'   to write traces to the standard output.
      "console" = ,
      "stdout" = {
        otelsdk::logger_provider_stdstream$new(list(output = "stdout"))
      },
      #' - `stderr`: uses [otelsdk::logger_provider_stdstream], to write traces
      #'   to the standard error.
      "stderr" = {
        otelsdk::logger_provider_stdstream$new(list(output = "stderr"))
      },
      #' - `http` or `otlp`: uses [otelsdk::logger_provider_http], to send
      #'   traces through HTTP, using the OpenTelemetry Protocol (OTLP).
      "otlp" = ,
      "http" = {
        otelsdk::logger_provider_http$new()
      },
      #' - `otlp/file` uses [otelsdk::logger_provider_file] to write logs
      #'    to a JSONL file.
      "otlp/file" = {
        otelsdk::logger_provider_file$new()
      },
      stop(
        "Unknown OpenTelemetry exporter from ", evar,
        " environment variable: ", ev
      )
    )
  } else {
    #' - `<package>::<provider>`: will select the `<provider>` object from
    #'   the `<package>` package to use as a logger provider. It calls
    #'   `<package>::<provider>$new()` to create the new logger provider.
    #'   If this fails for some reason, e.g. the package is not installed,
    #'   then it throws an error.
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
  }

  the$logger_provider <- tp
  invisible(tp)
}

#' Get the default meter provider
#'
#' The meter provider defines how metrics are exported when collecting
#' telemetry data. It is unlikely that you need to call this function
#' directly, but read on to learn how to configure which exporter to
#' use.
#'
#' If there is no default set currently, then it creates and sets a
#' default.
#'
#' The default meter provider is created based on the
#' `r default_metrics_exporter_envvar_r` environment variable. This
#' environment variable is specifically for R applications with
#' OpenTelemetry support.
#'
#' If this is not set, then the generic `r default_metrics_exporter_envvar`
#' environment variable is used. This applies to all applications that
#' support OpenTelemetry and use the OpenTelemetry SDK.
#'
#' @return The default meter provider, an [otel_meter_provider]
#'   object.
#' @family low level metrics API
#' @export
#' @examples
#' get_default_meter_provider()

# safe start
get_default_meter_provider <- function() {
  tryCatch({                                                         # safe
    if (is.null(the$meter_provider)) {
      setup_default_meter_provider()
    }
    the$meter_provider
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    meter_provider_noop$new()                                        # safe
  })                                                                 # safe
}
# safe end

get_default_meter_provider_safe <- get_default_meter_provider

#' @rdname get_default_meter_provider
#' @usage NULL
#' @details
#' The following values are allowed:

setup_default_meter_provider <- function() {
  evar <- default_metrics_exporter_envvar_r
  ev <- Sys.getenv(evar, NA_character_)
  if (is.na(ev)) {
    evar <- default_metrics_exporter_envvar
    ev <- Sys.getenv(evar, NA_character_)
  }
  tp <-  if (is.na(ev)) {
    meter_provider_noop$new()
  } else if (!grepl("::", ev)) {
    set_default_service_name()
    switch(
      ev,
      #' - `none`: no metrics are exported.
      "none" = {
        meter_provider_noop$new()
      },
      #' - `stdout` or `console`: uses [otelsdk::meter_provider_stdstream],
      #'   to write metrics to the standard output.
      "console" = ,
      "stdout" = {
        otelsdk::meter_provider_stdstream$new(list(output = "stdout"))
      },
      #' - `stderr`: uses [otelsdk::meter_provider_stdstream], to write
      #'   metrics to the standard error.
      "stderr" = {
        otelsdk::meter_provider_stdstream$new(list(output = "stderr"))
      },
      #' - `http` or `otlp`: uses [otelsdk::meter_provider_http], to send
      #'   metrics through HTTP, using the OpenTelemetry Protocol (OTLP).
      "otlp" = ,
      "http" = {
        otelsdk::meter_provider_http$new()
      },
      #' - `otlp/file` uses [otelsdk::meter_provider_file] to write metrics
      #'    to a JSONL file.
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
  } else {
    #' - `<package>::<provider>`: will select the `<provider>` object from
    #'   the `<package>` package to use as a meter provider. It calls
    #'   `<package>::<provider>$new()` to create the new meter provider.
    #'   If this fails for some reason, e.g. the package is not installed,
    #'   then it throws an error.
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
  }

  the$meter_provider <- tp
  invisible(tp)
}
