
#' Get a tracer from the default tracer provider
#'
#' Calls [get_default_tracer_provider()] to get the default tracer
#' provider. Then calls its `$get_tracer()` method to create a new tracer.
#'
#' Calling `get_tracer()` multiple times with the same `name` (or same
#' auto-deduced name) will return the same (internal) tracer object.
#' (Even if the R external pointer objects representing them are
#' different.)
#'
#' A tracer is only destroyed if its tracer provider is destroyed.
#'
#' @param name Name of the new tracer. If missing, then deduced
#'   automatically using [default_tracer_name()]. Make sure you read
#'   the manual page of [default_tracer_name()] before using this argument.
#' @param version Optional. Specifies the version of the instrumentation
#'   scope if the scope has a version (e.g. R package version).
#'   Example value: `"1.0.0"`.
#' @param schema_url Optional. Specifies the Schema URL that should be
#'   recorded in the emitted telemetry.
#' @param attributes Optional. Specifies the instrumentation scope
#'   attributes to associate with emitted telemetry.
#' @param ... Additional arguments are passed to the `get_tracer()`
#'   method of the provider.
#' @param provider Tracer provider to use. If `NULL`, then it uses
#'   [get_default_tracer_provider()] to get a tracer provider.
#'
#' @return An OpenTelemetry tracer, an `otel_tracer` object.
#' @export
#' @family OpenTelemetry tracing

# safe start
get_tracer <- function(
  name = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
  tryCatch({                                                         # safe
    # does setup if necessary
    provider <- provider %||% get_default_tracer_provider()
    trc <- provider$get_tracer(name, version, schema_url, attributes, ...)
    invisible(trc)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    tracer_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

get_tracer_safe <- get_tracer

#' Get a logger from the default logger provider
#'
#' @param name Name of the new tracer. If missing, then deduced automatically.
#' @param minimum_severity A log level, the minimum severity log messages
#'   to log. See [log_severity_levels].
#' @param ... Additional arguments are passed to the `get_logger()`
#'   method of the provider.
#' @param provider Tracer provider to use. If `NULL`, then it uses
#'   [get_default_tracer_provider()] to get a tracer provider.
#' @inheritParams get_tracer
#' @export
#' @family OpenTelemetry logging

# safe start
get_logger <- function(
  name = NULL,
  minimum_severity = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
  tryCatch({                                                         # safe
    # does setup if necessary
    provider <- provider %||% get_default_logger_provider()
    lgr <- provider$get_logger(
      name,
      minimum_severity,
      version,
      schema_url,
      attributes,
      ...
    )
    invisible(lgr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

get_logger_safe <- get_logger

#' Get a meter from the default meter provider
#'
#' @param name Name of the new tracer. If missing, then deduced automatically.
#' @param ... Additional arguments are passed to the `get_meter()`
#'   method of the provider.
#' @param provider Meter provider to use. If `NULL`, then it uses
#'   [get_default_meter_provider()] to get a tracer provider.
#' @inheritParams get_tracer
#' @export
#' @family OpenTelemetry metrics

# safe start
get_meter <- function(
  name = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
  tryCatch({                                                         # safe
    # does setup if necessary
    provider <- provider %||% get_default_meter_provider()
    mtr <- provider$get_meter(name, version, schema_url, attributes, ...)
    invisible(mtr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    meter_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

get_meter_safe <- get_meter

#' Activate an OpenTelemetry span for an R scope
#'
#' @param span The OpenTelemetry span to activate.
#' @param end_on_exit Whether to end the span when exiting the activation
#'   scope.
#' @param activation_scope The scope to activate the span for, defaults to
#'   the caller frame.
#'
#' @export

# safe start
local_active_span <- function(
  span,
  end_on_exit = FALSE,
  activation_scope = parent.frame()
) {
  tryCatch({                                                         # safe
    invisible(span$activate(activation_scope, end_on_exit = end_on_exit))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
  })                                                                 # safe
}
# safe end

local_active_span_safe <- local_active_span

#' Evaluate R code with an active OpenTelemetry span
#'
#' @param span The OpenTelemetry span to activate.
#' @param expr R expression to evaluate.
#' @param end_on_exit Whether to end after evaluating the R expression.
#' @return The return value of `expr`.
#' @export

# safe start
with_active_span <- function(span, expr, end_on_exit = FALSE) {
  local({
    tryCatch({                                                       # safe
      invisible(span$activate(end_on_exit = end_on_exit))
    }, error = function(err) {                                       # safe
      errmsg("OpenTelemetry error: ", conditionMessage(err))         # safe
    })                                                               # safe
    expr
  })
}
# safe end

with_active_span_safe <- with_active_span

#' OpenTelemetry log severity levels
#'
#' A named integer vector, the severity levels in numeric form.
#' The names are the severity levels in text form. otel functions accept
#' both forms as severity levels, but the text form is more readable.
#'
#' @family OpenTelemetry constants
#' @export
#' @family OpenTelemetry logging

log_severity_levels <- c(
  "trace" = 1L,
  "trace2" = 2L,
  "trace3" = 3L,
  "trace4" = 4L,
  "debug" = 5L,
  "debug2" = 6L,
  "debug3" = 7L,
  "debug4" = 8L,
  "info" = 9L,
  "info2" = 10L,
  "info3" = 11L,
  "info4" = 12L,
  "warn" = 13L,
  "warn2" = 14L,
  "warn3" = 15L,
  "warn4" = 16L,
  "error" = 17L,
  "error2" = 18L,
  "error3" = 19L,
  "error4" = 20L,
  "fatal" = 21L,
  "fatal2" = 22L,
  "fatal3" = 23L,
  "fatal4" = 24L,
  NULL
)

md_log_severity_levels <- paste0(
  "\"",
  log_severity_levels,
  "\"",
  collapse = ", "
)

#' Returns the active span context
#'
#' This is sometimes useful when writing loggers or meters, to associate
#' logging and metrics reporting with traces.
#'
#' @return The active span context. If these is no active span context,
#' then an invalid span context is returned, i.e. `spc$is_valid()` will be
#' `FALSE` for the returned `spc`.
#'
#' @export
#' @family OpenTelemetry tracing

# safe start
get_active_span_context <- function() {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    trc$get_active_span_context()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    span_context_noop$new(NA_character_)                             # safe
  })                                                                 # safe
}
# safe end

get_active_span_context_safe <- get_active_span_context

#' Pack the currently active span context into standard HTTP OpenTelemetry
#' headers
#'
#' The returned headers can be sent over HTTP, or set as environment
#' variables for subprocesses.
#'
#' @return A named character vector, with lowercase names.
#'
#' @export

# safe start
pack_http_context <- function() {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    trc$get_active_span_context()$to_http_headers()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    structure(character(), names = character())                      # safe
  })                                                                 # safe
}
# safe end

pack_http_context_safe <- pack_http_context

#' Extract a span context from HTTP headers received from a client
#'
#' The return value can be used as the `parent` option when starting
#' a span.
#'
#' @param headers A named list with one or two strings: `traceparent` is
#' mandatory, and `tracestate` is optional.
#'
#' @export
#' @family OpenTelemetry tracing

# safe start
extract_http_context <- function(headers) {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    trc$extract_http_context(headers)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    span_context_noop$new(NA_character_)                             # safe
  })                                                                 # safe
}
# safe end

extract_http_context_safe <- extract_http_context

#' Start an OpenTelemetry span.
#'
#' Creates a new OpenTelemetry span and starts it, without activating it.
#'
#' Usually you want [start_local_active_span()] instead of `start_span`.
#' [start_local_active_span() also activates the span for the caller frame,
#' and ends the span when the called frame exits.
#'
#' You must end the span by calling [end_span()]. Alternatively you
#' can also end it with [local_active_span()] or [with_active_span()] by
#' setting `end_on_exit = TRUE`.
#'
#' @param name Name of the span. If not specified it will be `"<NA>"`.
#' @param attributes Span attributes. You may use [as_attributes()] to
#'   convert R objects to OpenTelemetry attributes. OpenTelemetry supports
#'   the following R types as attributes:
#'   `r paste0(otel_attr_types, collapse = ", ")`.
#' @param links `r doc_arg()[["links"]]`
#' @param options `r doc_arg()[["span-options"]]`
#' @param ... Additional arguments are passed to the `get_tracer()` method
#'   of the tracer.
#' @param tracer A tracer object or the name of the tracer to use, see
#'   [get_tracer()].
#' @return An OpenTelemetry span (`otel_span`).
#'
#' @family OpenTelemetry tracing
#' @export

# safe start
start_span <- function(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    invisible(tracer$start_span(name, attributes, links, options, ...))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    span_noop$new()                                                  # safe
  })                                                                 # safe
}
# safe end

start_span_safe <- start_span

#' End an OpenTelemetry span
#'
#' You must end every span, by calling `end_span`, or using the `end_on_exit`
#' argument of [local_active_span()] or [with_active_span()].
#'
#' @param span The span to end.
#' @return `NULL`.
#'
#' @export

# safe start
end_span <- function(span) {
  tryCatch({                                                         # safe
    span$end()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(NULL)                                                  # safe
  })                                                                 # safe
}
# safe end

end_span_safe <- end_span

#' Start and activate a span
#'
#' Creates, starts and activates an OpenTelemetry span.
#'
#' Usually you want this functions instead of [start_local())], which
#' does not activate the new span.
#'
#' If `end_on_exit` is `TRUE` (the default), then it also ends the span
#' when the activation scope finishes.
#'
#' @param activation_scope The R scope to activate the span for. Defaults
#'   to the called frame.
#' @param end_on_exit Whether to also end the span when the activation scope
#'   exits.
#' @inheritParams start_span
#' @return The new OpenTelemetry span object (of class `otel_span`),
#'   invisibly. See [otel_span] for information about the returned object.
#' @family OpenTelemetry tracing
#' @export

# safe start
start_local_active_span <- function(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL,
  activation_scope = parent.frame(),
  end_on_exit = TRUE
) {
  tryCatch({                                                         # safe
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    span <- tracer$start_span(name, attributes, links, options, ...)
    span$activate(
      activation_scope = activation_scope, end_on_exit = end_on_exit)
    invisible(span)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    span_noop$new()                                                  # safe
  })                                                                 # safe
}
# safe end

start_local_active_span_safe <- start_local_active_span

#' Check whether OpenTelemetry tracing is active
#'
#' This is useful for avoiding computation when tracing is inactive.
#'
#' It calls [get_tracer()] with `name` and then it calls the tracer's
#' `$is_enabled()` method.
#'
#' @param tracer Tracer object (`otel_tracer`), or a tracer name, the
#'   instrumentation scope, to pass to [get_tracer()].
#' @return `TRUE` is OpenTelemetry tracing is active, `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry tracing
#' TODO

# safe start
is_tracing <- function(tracer = NULL) {
  tryCatch({                                                         # safe
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    tracer$is_enabled()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_tracing_safe <- is_tracing

#' Check whether OpenTelemetry logging is active
#'
#' This is useful for avoiding computation when logging is inactive.
#'
#' It calls [get_logger()] with `name` and then it calls the logger's
#' `$is_enabled()` method.
#'
#' @param logger Logger object (`otel_logger`), or a logger name, the
#'   instrumentation scope, to pass to [get_logger()].
#' @return `TRUE` is OpenTelemetry logging is active, `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry logging

# safe start
is_logging <- function(logger = NULL) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_tracer")) {
      logger <- get_logger(logger)
    }
    logger$is_enabled()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_logging_safe <- is_logging

#' Check whether OpenTelemetry metrics collection is active
#'
#' This is useful for avoiding computation when metrics collection is inactive.
#'
#' It calls [get_meter()] with `name` and then it calls the meter's
#' `$is_enabled()` method.
#'
#' @param meter Meter object (`otel_meter`), or a meter name, the
#'   instrumentation scope, to pass to [get_meter()].
#' @return `TRUE` is OpenTelemetry metrics collection  is active,
#' `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry metrics

# safe start
is_measuring <- function(meter = NULL) {
  tryCatch({                                                         # safe
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter(meter)
    }
    meter$is_enabled()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_measuring_safe <- is_measuring

#' Log an OpenTelemetry log message
#'
#' @param msg Log message, may contain R expressions to evaluate within
#'   braces.
#' @param severity Log severity, a string, one of
#'   `r md_log_severity_levels`.
#' @param ... Additional arguments are passed to the `$log()` method of
#'   the default logger.
#' @param .envir Environment to evaluate the interpolated  expressions of
#'   the log message in.
#' @param logger Logger to use. If not an OpenTelemetry logger object
#'   (`otel_logger`), then it passed to [get_logger()] to get a logger.
#' @return The logger, invisibly.
#'
#' @export
#' @family OpenTelemetry logging

# safe start
log <- function(
  msg,
  ...,
  severity = "info",
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, severity, ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_safe <- log

#' @details `log_trace()` is the same as `log()` with `severity_level`
#'   "trace".
#' @rdname log
#' @export

# safe start
log_trace <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "trace", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_trace_safe <- log_trace

#' @details `log_debug()` is the same as `log()` with `severity_level`
#'   "debug".
#' @rdname log
#' @export

# safe start
log_debug <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "debug", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_debug_safe <- log_debug

#' @details `log_info()` is the same as `log()` with `severity_level`
#'   "info".
#' @rdname log
#' @export

# safe start
log_info <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "info", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_info_safe <- log_info

#' @details `log_warn()` is the same as `log()` with `severity_level`
#'   "warn".
#' @rdname log
#' @export

# safe start
log_warn <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "warn", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_warn_safe <- log_warn

#' @details `log_error)` is the same as `log()` with `severity_level`
#'   "error".
#' @rdname log
#' @export

# safe start
log_error <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "error", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_error_safe <- log_error

#' @details `log_fatal()` is the same as `log()` with `severity_level`
#'   "fatal".
#' @rdname log
#' @export

# safe start
log_fatal <- function(
  msg,
  ...,
  .envir = parent.frame(),
  logger = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "fatal", ..., .envir = .envir)
    invisible(logger)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_fatal_safe <- log_fatal

#' Increase an OpenTelemetry counter
#'
#' @param name Name of the counter.
#' @param value Value to add to the counter, defaults to 1.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#' @param meter Meter object (`otel_meter`). Otherwise it is passed to
#'   [get_meter()] to get a meter.
#'
#' @return The counter object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @family OpenTelemetry metrics
#' @export

# safe start
counter_add <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    counter_noop$new()                                               # safe
  })                                                                 # safe
}
# safe end

counter_add_safe <- counter_add

#' Increase or decrease an OpenTelemetry up-down counter
#'
#' @param name Name of the up-down counter.
#' @param value Value to add to or subtract from the counter, defaults
#'   to 1.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#' @param meter Meter object (`otel_meter`). Otherwise it is passed to
#'   [get_meter()] to get a meter.
#'
#' @return The up-down counter object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @family OpenTelemetry metrics
#' @export

# safe start
up_down_counter_add <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_up_down_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    up_down_counter_noop$new()                                               # safe
  })                                                                 # safe
}
# safe end

up_down_counter_add_safe <- up_down_counter_add

#' Record a value of an OpenTelemetry histogram
#'
#' @param name Name of the histogram.
#' @param value Value to record.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#' @param meter Meter object (`otel_meter`). Otherwise it is passed to
#'   [get_meter()] to get a meter.
#'
#' @return The histogram object, invisibly.
#'
#' @export
#' @family OpenTelemetry metrics instruments
#' @family OpenTelemetry metrics

# safe start
histogram_record <- function(
  name,
  value,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_histogram(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    histogram_noop$new()                                             # safe
  })                                                                 # safe
}
# safe end

histogram_record_safe <- histogram_record

#' Record a value of an OpenTelemetry gauge
#'
#' @param name Name of the gauge
#' @param value Value to record.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#' @param meter Meter object (`otel_meter`). Otherwise it is passed to
#'   [get_meter()] to get a meter.
#'
#' @return The gauge object, invisibly.
#'
#' @export
#' @family OpenTelemetry metrics instruments
#' @family OpenTelemetry metrics

# safe start
gauge_record <- function(
  name,
  value,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
  tryCatch({                                                         # safe
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_gauge(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    gauge_noop$new()                                                 # safe
  })                                                                 # safe
}
# safe end

gauge_record_safe <- gauge_record
