# -------------------------------------------------------------------------
# Simplified API

#' Check whether OpenTelemetry tracing is active
#'
#' @return `TRUE` is OpenTelemetry tracing is active, `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry tracing

# safe start
is_tracing <- function() {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    trc$is_enabled()
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_tracing_safe <- is_tracing

#' Check whether OpenTelemetry logging is active
#'
#' @return `TRUE` is OpenTelemetry logging is active, `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry logging

# safe start
is_logging <- function() {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    !inherits(lgr, "otel_logger_noop")
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_logging_safe <- is_logging

#' Check whether OpenTelemetry metrics collection is active
#'
#' @return `TRUE` is OpenTelemetry metrics collection  is active,
#' `FALSE` otherwise.
#'
#' @export
#' @family OpenTelemetry metrics

# safe start
is_measuring <- function() {
  tryCatch({                                                         # safe
    mtr <- get_meter()
    !inherits(mtr, "otel_meter_noop")
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    FALSE                                                            # safe
  })                                                                 # safe
}
# safe end

is_measuring_safe <- is_measuring

#' Get a tracer from the default tracer provider
#'
#' Calls [get_default_tracer_provider()] to get the default tracer
#' provider. Then calls its `$get_tracer()` method to create a new tracer.
#'
#' @param name Name of the new tracer. If missing, then deduced automatically.
#' @return An OpenTelemetry tracer, an `otel_tracer` object.
#' @export
#' @family OpenTelemetry tracing

# safe start
get_tracer <- function(name = NULL) {
  tryCatch({                                                         # safe
    # does setup if necessary
    tp <- get_default_tracer_provider()
    trc <- tp$get_tracer(name)
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
#'
#' @export
#' @family OpenTelemetry logging

# safe start
get_logger <- function(name = NULL) {
  tryCatch({                                                         # safe
    # does setup if necessary
    tp <- get_default_logger_provider()
    trc <- tp$get_logger(name)
    invisible(trc)
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
#' @export
#' @family OpenTelemetry metrics

# safe start
get_meter <- function(name = NULL) {
  tryCatch({                                                         # safe
    # does setup if necessary
    tp <- get_default_meter_provider()
    trc <- tp$get_meter(name)
    invisible(trc)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    meter_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

get_meter_safe <- get_meter

#' Start a new OpenTelemetry span, using the default tracer
#'
#' @param name Name of the span.
#' @param tracer_name The name of the tracer to use, see [get_tracer()].
#' @param ...,scope,activation_scope Additional arguments are passed to
#'   the default tracer's `start_span()` method.
#' @return The new OpenTelemetry span object, invisibly.
#'
#' @export
#' @family OpenTelemetry tracing

# safe start
start_span <- function(
  name = NULL,
  tracer_name = NULL,
  ...,
  scope = parent.frame(),
  activation_scope = parent.frame()
) {
  tryCatch({                                                         # safe
    trc <- get_tracer(tracer_name)
    invisible(trc$start_span(
      name = name, ...,
      scope = scope,
      activation_scope = activation_scope
    ))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(span_noop$new())                                       # safe
  })                                                                 # safe
}
# safe end

start_span_safe <- start_span

#' Activate an OpenTelemetry span for an R scope
#'
#' @param span The OpenTelemetry span to activate.
#' @param activation_scope The scope to activate it for, defaults to the
#'   caller frame.
#'
#' @export

# safe start
local_active_span <- function(span, activation_scope = parent.frame()) {
  tryCatch({                                                         # safe
    invisible(span$activate(activation_scope))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
  })                                                                 # safe
}
# safe end

local_actice_span_safe <- local_active_span

#' Evaluate R code with an active OpenTelemetry span
#'
#' @param span The OpenTelemetry span to activate.
#' @param expr R expression to evaluate.
#' @return The return value of `expr`.
#' @export

# safe start
with_active_span <- function(span, expr) {
  local({
    tryCatch({                                                       # safe
      invisible(span$activate())
    }, error = function(err) {                                       # safe
      errmsg("OpenTelemetry error: ", conditionMessage(err))         # safe
    })                                                               # safe
    expr
  })
}
# safe end

with_active_span_safe <- with_active_span

#' Log an OpenTelemetry log message, using the default logger
#'
#' @param msg Log message, may contain R expressions to evaluate within
#'   braces.
#' @param severity Log severity, a string, one of
#'   `r md_log_severity_levels`.
#' @param ... Additional arguments are passed to the `$log()` method of
#'   the default logger.
#' @param .envir Environment to evaluate the interpolated  expressions of
#'   the log message in.
#'   automatically deactivated after this scope.
#'
#' @return The logger, invisibly.
#'
#' @export
#' @family OpenTelemetry logging

# safe start
log <- function(msg, ..., severity = "info", .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, severity, ..., .envir = .envir)
    invisible(lgr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

#' @details `log_trace()` is the same as `log()` with `severity_level`
#'   "trace".
#' @rdname log
#' @export

# safe start
log_trace <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "trace", ..., .envir = .envir)
    invisible(lgr)
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
log_debug <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "debug", ..., .envir = .envir)
    invisible(lgr)
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
log_info <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "info", ..., .envir = .envir)
    invisible(lgr)
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
log_warn <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "warn", ..., .envir = .envir)
    invisible(lgr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_warn_safe <- log_warn

#' @details `log_error()` is the same as `log()` with `severity_level`
#'   "error".
#' @rdname log
#' @export

# safe start
log_error <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "error", ..., .envir = .envir)
    invisible(lgr)
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
log_fatal <- function(msg, ..., .envir = parent.frame()) {
  tryCatch({                                                         # safe
    lgr <- get_logger()
    lgr$log(msg, "fatal", ..., .envir = .envir)
    invisible(lgr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_debug_safe <- log_debug

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

#' Increase an OpenTelemetry counter using the default meter
#'
#' @param name Name of the counter.
#' @param value Value to add to the counter, defaults to 1.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#'
#' @return The counter object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @export
#' @family OpenTelemetry metrics

# safe start
counter_add <- function(name, value = 1L, attributes = NULL, context = NULL) {
  tryCatch({                                                         # safe
    mtr <- get_meter()
    ctr <- mtr$create_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    counter_noop$new()                                               # safe
  })                                                                 # safe
}
# safe end

counter_add_safe <- counter_add

#' Increase or decrease an OpenTelemetry up-down counter using the default
#' meter
#'
#' @param name Name of the up-down counter.
#' @param value Value to add to or subtract from the counter, defaults
#'   to 1.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#'
#' @return The up-down counter object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @export
#' @family OpenTelemetry metrics

# safe start
up_down_counter_add <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL
) {
  tryCatch({                                                         # safe
    mtr <- get_meter()
    ctr <- mtr$create_up_down_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    up_down_counter_noop$new()                                       # safe
  })                                                                 # safe
}
# safe end

up_down_counter_add_safe <- up_down_counter_add

#' Record a value of an OpenTelemetry histogram using the default
#' meter
#'
#' @param name Name of the histogram.
#' @param value Value to record.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#'
#' @return The histogram object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @export
#' @family OpenTelemetry metrics

# safe start
histogram_record <- function(name, value, attributes = NULL, context = NULL) {
  tryCatch({                                                         # safe
    mtr <- get_meter()
    ctr <- mtr$create_histogram(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    histogram_noop$new()                                             # safe
  })                                                                 # safe
}
# safe end

histogram_record_safe <- histogram_record

#' Record a value of an OpenTelemetry gauge using the default
#' meter
#'
#' @param name Name of the gauge
#' @param value Value to record.
#' @param attributes Additional attributes to add.
#' @param context Span context. If missing the active context is used,
#'   if any.
#'
#' @return The gauge object, invisibly.
#'
#' @family OpenTelemetry metrics instruments
#' @export
#' @family OpenTelemetry metrics

# safe start
gauge_record <- function(name, value, attributes = NULL, context = NULL) {
  tryCatch({                                                         # safe
    mtr <- get_meter()
    ctr <- mtr$create_gauge(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    gauge_noop$new()                                                 # safe
  })                                                                 # safe
}
# safe end

gauge_record_safe <- gauge_record

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
