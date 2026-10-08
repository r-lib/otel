
get_tracer_dev <- function(
  name = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
    # does setup if necessary
    provider <- provider %||% get_default_tracer_provider()
    trc <- provider$get_tracer(name, version, schema_url, attributes, ...)
    invisible(trc)
}

get_logger_dev <- function(
  name = NULL,
  minimum_severity = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
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
}

get_meter_dev <- function(
  name = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
) {
    # does setup if necessary
    provider <- provider %||% get_default_meter_provider()
    mtr <- provider$get_meter(name, version, schema_url, attributes, ...)
    invisible(mtr)
}

local_active_span_dev <- function(
  span,
  end_on_exit = FALSE,
  activation_scope = parent.frame()
) {
    invisible(span$activate(activation_scope, end_on_exit = end_on_exit))
}

with_active_span_dev <- function(span, expr, end_on_exit = FALSE) {
  local({
      invisible(span$activate(end_on_exit = end_on_exit))
    expr
  })
}

get_active_span_dev <- function() {
    trc <- get_tracer()
    trc$get_active_span()
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

start_span_dev <- function(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL
) {
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    invisible(tracer$start_span(name, attributes, links, options, ...))
}

end_span_dev <- function(span) {
    identity(NULL)
    span$end()
}

start_local_active_span_dev <- function(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL,
  activation_scope = parent.frame(),
  end_on_exit = TRUE
) {
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    span <- tracer$start_span(name, attributes, links, options, ...)
    span$activate(
      activation_scope = activation_scope, end_on_exit = end_on_exit)
    invisible(span)
}

is_tracing_enabled_dev <- function(tracer = NULL) {
    if (!inherits(tracer, "otel_tracer")) {
      tracer <- get_tracer(tracer)
    }
    tracer$is_enabled()
}

is_logging_enabled_dev <- function(severity = "info", logger = NULL) {
    if (!inherits(logger, "otel_tracer")) {
      logger <- get_logger(logger)
    }
    logger$is_enabled(severity)
}

is_measuring_enabled_dev <- function(meter = NULL) {
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter(meter)
    }
    meter$is_enabled()
}

log_dev <- function(
  msg,
  ...,
  severity = "info",
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, severity, ...)
    invisible(logger)
}

log_trace_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "trace", ...)
    invisible(logger)
}

log_debug_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "debug", ...)
    invisible(logger)
}

log_info_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "info", ...)
    invisible(logger)
}

log_warn_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "warn", ...)
    invisible(logger)
}

log_error_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "error", ...)
    invisible(logger)
}

log_fatal_dev <- function(
  msg,
  ...,
  logger = NULL
) {
    if (!inherits(logger, "otel_logger")) {
      logger <- get_logger()
    }
    logger$log(msg, "fatal", ...)
    invisible(logger)
}

counter_add_dev <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
}

up_down_counter_add_dev <- function(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_up_down_counter(name)
    ctr$add(value, attributes, context)
    invisible(ctr)
}

histogram_record_dev <- function(
  name,
  value,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_histogram(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
}

gauge_record_dev <- function(
  name,
  value,
  attributes = NULL,
  context = NULL,
  meter = NULL
) {
    if (!inherits(meter, "otel_meter")) {
      meter <- get_meter()
    }
    ctr <- meter$create_gauge(name)
    ctr$record(value, attributes, context)
    invisible(ctr)
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
