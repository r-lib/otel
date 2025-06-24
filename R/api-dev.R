
is_tracing_dev <- function() {
    trc <- get_tracer()
    trc$is_enabled()
}

is_logging_dev <- function() {
    lgr <- get_logger()
    !inherits(lgr, "otel_logger_noop")
}

is_measuring_dev <- function() {
    mtr <- get_meter()
    !inherits(mtr, "otel_meter_noop")
}

get_tracer_dev <- function(name = NULL) {
    # does setup if necessary
    tp <- get_default_tracer_provider()
    trc <- tp$get_tracer(name)
    invisible(trc)
}

get_logger_dev <- function(name = NULL) {
    # does setup if necessary
    tp <- get_default_logger_provider()
    trc <- tp$get_logger(name)
    invisible(trc)
}

get_meter_dev <- function(name = NULL) {
    # does setup if necessary
    tp <- get_default_meter_provider()
    trc <- tp$get_meter(name)
    invisible(trc)
}

start_span_dev <- function(
  name = NULL,
  tracer_name = NULL,
  ...,
  scope = parent.frame(),
  activation_scope = parent.frame()
) {
    trc <- get_tracer(tracer_name)
    invisible(trc$start_span(
      name = name, ...,
      scope = scope,
      activation_scope = activation_scope
    ))
}

local_active_span_dev <- function(span, activation_scope = parent.frame()) {
    invisible(span$activate(activation_scope))
}

with_active_span_dev <- function(span, expr) {
  local({
      invisible(span$activate())
    expr
  })
}

log_dev <- function(msg, ..., severity = "info", .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, severity, ..., .envir = .envir)
    invisible(lgr)
}

log_trace_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "trace", ..., .envir = .envir)
    invisible(lgr)
}

log_debug_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "debug", ..., .envir = .envir)
    invisible(lgr)
}

log_info_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "info", ..., .envir = .envir)
    invisible(lgr)
}

log_warn_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "warn", ..., .envir = .envir)
    invisible(lgr)
}

log_error_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "error", ..., .envir = .envir)
    invisible(lgr)
}

log_fatal_dev <- function(msg, ..., .envir = parent.frame()) {
    lgr <- get_logger()
    lgr$log(msg, "fatal", ..., .envir = .envir)
    invisible(lgr)
}

counter_add_dev <- function(name, value = 1L, attributes = NULL, context = NULL) {
    mtr <- get_meter()
    ctr <- mtr$create_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
}

up_down_counter_add_dev <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL
) {
    mtr <- get_meter()
    ctr <- mtr$create_up_down_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
}

histogram_record_dev <- function(name, value, attributes = NULL, context = NULL) {
    mtr <- get_meter()
    ctr <- mtr$create_histogram(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
}

gauge_record_dev <- function(name, value, attributes = NULL, context = NULL) {
    mtr <- get_meter()
    ctr <- mtr$create_gauge(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
}

get_active_span_context_dev <- function() {
    trc <- get_tracer()
    trc$get_active_span_context()
}

pack_http_context_dev <- function() {
    trc <- get_tracer()
    trc$get_active_span_context()$to_http_headers()
}

extract_http_context_dev <- function(headers) {
    trc <- get_tracer()
    trc$extract_http_context(headers)
}

get_default_tracer_provider_dev <- function() {
    if (is.null(the$tracer_provider)) {
      setup_default_tracer_provider()
    }
    the$tracer_provider
}

get_default_logger_provider_dev <- function() {
    if (is.null(the$logger_provider)) {
      setup_default_logger_provider()
    }
    the$logger_provider
}

get_default_meter_provider_dev <- function() {
    if (is.null(the$meter_provider)) {
      setup_default_meter_provider()
    }
    the$meter_provider
}

start_shiny_app_dev <- function(service_name = NULL) {
    service_name <- service_name %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    service_name <- as_string(service_name, null = FALSE)
    Sys.setenv(OTEL_SERVICE_NAME = service_name)
    the$tracer_app <- get_tracer(service_name)
    invisible(the$tracer_app)
}

start_shiny_session_dev <- function(
    session, attributes = NULL, options = NULL, ...) {
    name <- get_env("OTEL_SERVICE_NAME")
    trc <- get_tracer(name)
    # inactive tracer, do nothing, but return a (session) span
    if (!trc$is_enabled()) {
      return(invisible(trc$start_span("session", options, ..., scope = NULL)))
    }

    attributes[["PATH_INFO"]] <- attributes[["PATH_INFO"]] %||%
      session[["request"]][["PATH_INFO"]] %||% ""
    attributes[["HTTP_HOST"]] <- attributes[["HTTP_HOST"]] %||%
      session[["request"]][["HTTP_HOST"]] %||% ""
    attributes[["HTTP_ORIGIN"]] <- attributes[["HTTP_ORIGIN"]] %||%
      session[["request"]][["HTTP_ORIGIN"]] %||% ""
    attributes[["QUERY_STRING"]] <- attributes[["QUERY_STRING"]] %||%
      session[["request"]][["QUERY_STRING"]] %||% ""
    attributes[["SERVER_PORT"]] <- attributes[["SERVER_PORT"]] %||%
      session[["request"]][["SERVER_PORT"]] %||% -1L
    try(attributes[["SERVER_PORT"]] <-
      as.integer(attributes[["SERVER_PORT"]]))

    options[["parent"]] <- options[["parent"]] %||% NA

    assign(
      "otel_span",
      trc$start_span(
        "session",
        attributes = attributes,
        options = options,
        scope = NULL,
        ...
      ),
      envir = session$userData
    )
    session$onSessionEnded(function(...) {
      session$userData$otel_span$end()
    })

    invisible(session$userData$otel_span)
}
