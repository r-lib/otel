generic_print <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.otel_tracer_provider <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("get_tracer" %in% names(x)) {
      "  get_tracer(name, version, schema_url, attributes)"
    },
    if ("flush" %in% names(x)) {
      "  flush()"
    },
    if ("get_spans" %in% names(x)) {
      "  get_spans()"
    },
    NULL
  )
}

#' @export

print.otel_tracer_provider <- generic_print

#' @export

format.otel_tracer <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("start_span" %in% names(x)) {
      "  start_span(name, attributes, links, options)"
    },
    if ("is_enabled" %in% names(x)) {
      "  is_enabled()"
    },
    if ("flush" %in% names(x)) {
      "  flush()"
    },
    NULL
  )
}

#' @export

print.otel_tracer <- generic_print

#' @export

format.otel_span <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    paste0("name: ", x$name),
    "methods:",
    if ("add_event" %in% names(x)) {
      "  add_event(name, attributes, timestamp)"
    },
    if ("end" %in% names(x)) {
      "  end(options, status_code)"
    },
    if ("get_context" %in% names(x)) {
      "  get_context()"
    },
    if ("is_recording" %in% names(x)) {
      "  is_recording()"
    },
    if ("record_exception" %in% names(x)) {
      "  record_exception(error_condition, attributes, ...)"
    },
    if ("set_attribute" %in% names(x)) {
      "  set_attribute(name, value)"
    },
    if ("set_status" %in% names(x)) {
      "  set_status(status_code, description)"
    },
    if ("update_name" %in% names(x)) {
      "  update_name(name)"
    },
    NULL
  )
}

#' @export

print.otel_span <- generic_print

#' @export

format.otel_span_context <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("get_span_id" %in% names(x)) {
      "  get_span_id()"
    },
    if ("get_trace_flags" %in% names(x)) {
      "  get_trace_flags()"
    },
    if ("get_trace_id" %in% names(x)) {
      "  get_trace_id()"
    },
    if ("is_remote" %in% names(x)) {
      "  is_remote()"
    },
    if ("is_sampled" %in% names(x)) {
      "  is_sampled()"
    },
    if ("is_valid" %in% names(x)) {
      "  is_valid()"
    },
    if ("to_http_headers" %in% names(x)) {
      "  to_http_headers()"
    },
    NULL
  )
}

#' @export

print.otel_span_context <- generic_print

#' @export

format.otel_logger_provider <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("get_logger" %in% names(x)) {
      "  get_logger(name, minimum_severity, version, schema_url, attributes)"
    },
    if ("flush" %in% names(x)) {
      "  flush()"
    },
    NULL
  )
}

#' @export

print.otel_logger_provider <- generic_print

#' @export

format.otel_logger <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("is_enabled" %in% names(x)) {
      "  is_enabled()"
    },
    if ("get_minimum_severity" %in% names(x)) {
      "  get_minimum_severity()"
    },
    if ("set_minimum_severity" %in% names(x)) {
      "  set_minimum_severity(minimum_severity)"
    },
    if ("log" %in% names(x)) {
      "  log(msg, severity, span_context, attributes, ..., .envir)"
    },
    if ("trace" %in% names(x)) {
      "  trace(msg, span_context, attributes, ..., .envir)"
    },
    if ("debug" %in% names(x)) {
      "  debug(msg, span_context, attributes, ..., .envir)"
    },
    if ("info" %in% names(x)) {
      "  info(msg, span_context, attributes, ..., .envir)"
    },
    if ("warn" %in% names(x)) {
      "  warn(msg, span_context, attributes, ..., .envir)"
    },
    if ("error" %in% names(x)) {
      "  error(msg, span_context, attributes, ..., .envir)"
    },
    if ("fatal" %in% names(x)) {
      "  fatal(msg, span_context, attributes, ..., .envir)"
    },
    NULL
  )
}

#' @export

print.otel_logger <- generic_print

#' @export

format.otel_meter_provider <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("get_meter" %in% names(x)) {
      "  get_meter(name, version, schema_url, attributes)"
    },
    if ("flush" %in% names(x)) {
      "  flush(timeout)"
    },
    if ("shutdown" %in% names(x)) {
      "  shutdown(timeout)"
    },
    if ("get_metrics" %in% names(x)) {
      "  get_metrics()"
    },
    NULL
  )
}

#' @export

print.otel_meter_provider <- generic_print

#' @export

format.otel_meter <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("create_counter" %in% names(x)) {
      "  create_counter(name, description, unit)"
    },
    if ("create_up_down_counter" %in% names(x)) {
      "  create_up_down_counter(name, description, unit)"
    },
    if ("create_histogram" %in% names(x)) {
      "  create_histogram(name, description, unit)"
    },
    if ("create_gauge" %in% names(x)) {
      "  create_gauge(name, description, unit)"
    },
    NULL
  )
}

#' @export

print.otel_meter <- generic_print

#' @export

format.otel_counter <- function(x, ...) {
  c(
    paste0("<", paste(class(x), collapse = "/"), ">"),
    "methods:",
    if ("add" %in% names(x)) {
      "  add(value, attributes, span_context)"
    },
    if ("record" %in% names(x)) {
      "  record(value, attributes, span_context)"
    },
    NULL
  )
}

#' @export

print.otel_counter <- generic_print

#' @export

format.otel_up_down_counter <- format.otel_counter

#' @export

print.otel_up_down_counter <- generic_print

#' @export

format.otel_histogram <- format.otel_counter

#' @export

print.otel_histogram <- generic_print

#' @export

format.otel_gauge <- format.otel_counter

#' @export

print.otel_gauge <- generic_print
