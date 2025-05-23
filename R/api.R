# -------------------------------------------------------------------------
# Simplified API

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

# safe start
get_tracer <- function(name = NULL) {
  tryCatch({                                                         # safe
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
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

# safe start
get_logger <- function(name = NULL) {
  tryCatch({                                                         # safe
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
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

# safe start
get_meter <- function(name = NULL) {
  tryCatch({                                                         # safe
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
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

# safe start
start_span <- function(name = NULL, session = NULL, ...,
                       scope = parent.frame()) {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    if (!is.null(session)) {
      if (inherits(session, "ShinySession")) {
        session <- session$userData$otel_session
      }
      trc$activate_session(session)
    }
    invisible(trc$start_span(name = name, ..., scope = scope))
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(span_noop$new())                                       # safe
  })                                                                 # safe
}
# safe end

start_span_safe <- start_span

#' Return the current span context
#'
#' This is sometimes useful when writing loggers or meters, to associate
#' logging and metrics reporting with traces.
#'
#' @return The current span context. If these is no current span context,
#' then an invalid span context is returned, i.e. `spc$is_valid()` will be
#' `FALSE` for the returned `spc`.
#'
#' @export

# safe start
get_current_span_context <- function() {
  tryCatch({                                                         # safe
    trc <- get_tracer()
    trc$get_current_span_context()
  }, error = function(err) {                                         # safe
    errmsg("Opentelemetry error: ", conditionMessage(err))           # safe
    span_context_noop$new(NA_character_)                             # safe
  })                                                                 # safe
}
# safe end

get_current_span_context_safe <- get_current_span_context

#' Extract a span context from HTTP headers received from a client
#'
#' The return value can be used as the `parent` option when starting
#' a span.
#'
#' @param headers A named list with one or two strings: `traceparent` is
#' mandatory, and `tracestate` is optional.
#'
#' @export

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
