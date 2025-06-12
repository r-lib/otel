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
#' @param name Name of the new tracer. This is typically the name of the
#'   package or project. Defaults to the name of the calling package,
#'   or the name of the current working directory if not called from a
#'   package.
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
#' @param name Name of the new logger. This is typically the name of the
#'   package or project. Defaults to the name of the calling package,
#'   or the name of the current working directory if not called from a
#'   package.
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
#' @param name Name of the new meter. This is typically the name of the
#'   package or project. Defaults to the name of the calling package,
#'   or the name of the current working directory if not called from a
#'   package.
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
#' @param session Optionally, an OpenTelemetry session to activate before
#'   starting the span. It can also be a Shiny session (`ShinySession`
#'   object), that was previously used as an argument to
#'   [start_shiny_session()].
#' @param ...,scope Additional arguments are passed to the default tracer's
#'   `start_span()` method.
#' @return The new Opentelemetry span object, invisibly.
#'
#' @export
#' @family OpenTelemetry tracing

# safe start
start_span <- function(name = NULL, session = NULL, ...,
                       scope = parent.frame()) {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    if (!is.null(session)) {
      if (inherits(session, "ShinySession")) {
        session <- session$userData$otel_session
      }
      session$activate_session()
    }
    invisible(trc$start_span(name = name, ..., scope = scope))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(span_noop$new())                                       # safe
  })                                                                 # safe
}
# safe end

start_span_safe <- start_span

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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
    logger_noop$new()                                                # safe
  })                                                                 # safe
}
# safe end

log_safe <- log

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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
    gauge_noop$new()                                             # safe
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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
    span_context_noop$new(NA_character_)                             # safe
  })                                                                 # safe
}
# safe end

get_active_span_context_safe <- get_active_span_context

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
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
    span_context_noop$new(NA_character_)                             # safe
  })                                                                 # safe
}
# safe end

extract_http_context_safe <- extract_http_context
