#' OpenTelemetry Logger Provider Object
#'
#' @description
#' [otel_logger_provider] -> [otel_logger]
#'
#' @details
#' The logger provider defines how logs are exported when collecting
#' telemetry data. It is unlikely that you need to use logger provider
#' objects directly.
#'
#' Usually there is a single logger provider for an R app or script.
#'
#' Typically the logger provider is created automatically, at the first
#' [log()] call. otel decides which logger provider class to use based on
#' [Environment Variables].
#'
#' # Implementations
#'
#' Note that this list is updated manually and may be incomplete.
#'
#' - [logger_provider_noop]: No-op logger provider, used when no logs are
#'   emitted.
#' - [otelsdk::logger_provider_file]: Save logs to a JSONL file.
#' - [otelsdk::logger_provider_http]: Send logs to a collector over
#'   HTTP/OTLP.
#' - [otelsdk::logger_provider_stdstream]: Write logs to standard output
#'   or error or to a file. Primarily for debugging.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `logger_provider$get_logger()`
#'
#' Get or create a new logger object.
#'
#' ### Usage
#'
#' ```r
#' logger_provider$get_logger(
#'  name = NULL,
#'  version = NULL,
#'  schema_url = NULL,
#'  attributes = NULL
#' )
#' ```
#'
#' ### Arguments
#'
#' - `name` Logger name. It makes sense to reuse the tracer name as the
#'   logger name. See [get_logger()] and [default_tracer_name()].
#' - `version`: Optional. Specifies the version of the instrumentation
#'   scope if the scope has a version (e.g. R package version).
#'   Example value: `"1.0.0"`.
#' - `schema_url`: Optional. Specifies the Schema URL that should be
#'   recorded in the emitted telemetry.
#' - `attributes`: Optional. Specifies the instrumentation scope
#'   attributes to associate with emitted telemetry. See [as_attributes()]
#'   for allowed values. You can also use [as_attributes()] to convert R
#'   objects to OpenTelemetry attributes.
#'
#' ### Value
#'
#' An OpenTelemetry logger ([otel_logger]) object.
#'
#' ### See also
#'
#' [get_default_logger_provider()], [get_logger()].
#'
# -------------------------------------------------------------------------
#' ## `logger_provider$flush()`
#'
#' Force any buffered logs to flush. Logger providers might not implement
#' this method.
#'
#' ### Usage
#'
#' ```r
#' logger_provider$flush()
#' ```
#'
#' ### Value
#'
#' Nothing.
#'
#' @name otel_logger_provider
#' @family low level logs API
#' @return Not applicable.
#' @examples
#' lp <- otel::get_default_logger_provider()
#' lgr <- lp$get_logger()
#' lgr$is_enabled()
NULL

#' No-op logger provider
#'
#' This is the logger provider ([otel_logger_provider]) otel uses when
#' logging is disabled.
#'
#' All methods are no-ops or return objects that are also no-ops.
#'
#' @family low level logs API
#' @usage NULL
#' @format NULL
#' @keywords internal
#' @export
#' @return Not applicable.
#' @examples
#' logger_provider_noop$new()

logger_provider_noop <- list(
  new = function() {
    structure(
      list(
        get_logger = function(
          name = NULL,
          minimum_severity = NULL,
          version = NULL,
          schema_url = NULL,
          attributes = NULL
        ) {
          logger_noop$new(
            name,
            minimum_severity = minimum_severity,
            version = version,
            schema_url = schema_url,
            attributes = attributes
          )
        },
        flush = function() {
          # noop
        }
      ),
      class = c(
        "otel_logger_provider_noop",
        "otel_logger_provider"
      )
    )
  }
)

#' OpenTelemetry Logger Object
#' @name otel_logger
#' @family low level logs API
#' @description
#' [otel_logger_provider] -> [otel_logger]
#'
#' @details
#' Usually you do not need to deal with otel_logger objects directly.
#' [log()] automatically sets up the logger for emitting the logs.
#'
#' A logger object is created by calling the `get_logger()` method of an
#' [otel_logger_provider].
#'
#' You can use the `log()` method of the logger object to emit logs.
#'
#' Typically there is a separate logger object for each instrumented R
#' package.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `logger$is_enabled()`
#'
#' Whether the logger is active and emitting logs at a certain severity
#' level.
#'
#' This is equivalent to the [is_logging_enabled()] function.
#'
#' ### Usage
#'
#' ```r
#' logger$is_enabled(severity = "info", event_id = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `severity`: Check if logs are emitted at this severity level.
#' - `event_id`: Not implemented yet.
#'
#' ### Value
#'
#' Logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `logger$get_minimum_severity()`
#'
#' Get the current minimum severity at which the logger is emitting logs.
#'
#' ### Usage
#'
#' ```r
#' logger_get_minimum_severity()
#' ```
#'
#' ### Value
#'
#' Named integer scalar.
#'
# -------------------------------------------------------------------------
#' ## `logger$set_minimum_severiry()`
#'
#' Set the minimum severity for emitting logs.
#'
#' ### Usage
#'
#' ```r
#' logger$set_minimum_severity(minimum_severity)
#' ```
#'
#' ### Arguments
#'
#' - `minimum_severity`: Log severity, a string, one of
#'   `r md_log_severity_levels`.
#'
#' ### Value
#'
#' Nothing.
#'
# -------------------------------------------------------------------------
#' ## `logger$log()`
#'
#' Log an OpenTelemetry log message.
#'
#' ### Usage
#'
#' ```r
#' logger$log(
#'   msg = "",
#'   severity = "info",
#'   span_context = NULL,
#'   span_id = NULL,
#'   trace_id = NULL,
#'   trace_flags = NULL,
#'   timestamp = SYs.time(),
#'   observed_timestamp = NULL,
#'   attributes = NULL,
#'   .envir = parent.frame()
#' )
#' ```
#'
#' ### Arguments
#'
#' - `msg`: Log message, may contain R expressions to evaluate within
#'   braces.
#' - `severity`: Log severity, a string, one of
#'   `r md_log_severity_levels`.
#' - `span_context`: An [otel_span_context] object to associate the log
#'   message with a span.
#' - `span_id`: Alternatively to `span_context`, you can also specify
#'   `span_id`, `trace_id` and `trace_flags` to associate a log message
#'    with a span.
#' - `trace_id`: Alternatively to `span_context`, you can also specify
#'   `span_id`, `trace_id` and `trace_flags` to associate a log message
#'    with a span.
#' - `trace_flags`: Alternatively to `span_context`, you can also specify
#'   `span_id`, `trace_id` and `trace_flags` to associate a log message
#'    with a span.
#' - `timestamp`: Time stamp, defaults to the current time. This is the
#'   time the logged event occurred.
#' - `observed_timestamp`: Observed time stamp, this is the time the
#'   event was observed.
#' - `attributes`: Optional attributes, see [as_attributes()] for the
#'   possible values.
#' - `.envir`: Environment to evaluate the interpolated expressions of
#'   the log message in.
#' `
#' ### Value
#'
#' The logger object, invisibly.
#'
#' ## `logger$trace()`
#'
#' The same as `logger$log()`, with `severity = "trace"`.
#'
#' ## `logger$debug()`
#'
#' The same as `logger$log()`, with `severity = "debug"`.
#'
#' ## `logger$info()`
#'
#' The same as `logger$log()`, with `severity = "info"`.
#'
#' ## `logger$warn()`
#'
#' The same as `logger$log()`, with `severity = "warn"`.
#'
#' ## `logger$error()`
#'
#' The same as `logger$log()`, with `severity = "error"`.
#'
#' ## `logger$fatal()`
#'
#' The same as `logger$log()`, with `severity = "fatal"`.
#'
#' @return Not applicable.
#' @examples
#' lp <- get_default_logger_provider()
#' lgr <- lp$get_logger()
#' platform <- utils::sessionInfo()$platform
#' lgr$log("This is a log message from {platform}.", severity = "trace")
NULL

logger_noop <- list(
  new = function(
    name = NULL,
    minimum_severity = NULL,
    version = NULL,
    schema_url = NULL,
    attributes = NULL
  ) {
    self <- structure(
      list(
        trace = function(...) {
          invisible(self)
        },
        debug = function(...) {
          invisible(self)
        },
        info = function(...) {
          invisible(self)
        },
        warn = function(...) {
          invisible(self)
        },
        error = function(...) {
          invisible(self)
        },
        fatal = function(...) {
          invisible(self)
        },
        is_enabled = function(severity = "info", event_id = NULL) {
          FALSE
        },
        get_minimum_severity = function() {
          c("maximumseverity" = 255L)
        },
        set_minimum_severity = function(minimum_severity) {
          invisible(self)
        },
        log = function(
          msg = "",
          severity = "info",
          span_context = NULL,
          ...,
          .envir = parent.frame()
        ) {
          invisible(self)
        }
      ),
      class = c("otel_logger_noop", "otel_logger")
    )
    self
  }
)

log_record_noop <- list(
  new = function() {
    self <- structure(
      list(
        set_timestamp = function(timestamp) {
          invisible(self)
        },
        set_observed_timestamp = function(timestamp) {
          invisible(self)
        },
        set_severity = function(severity) {
          invisible(self)
        },
        set_body = function(message) {
          invisible(self)
        },
        set_attribute = function(key, value) {
          invisible(self)
        },
        set_event_id = function(id, name) {
          invisible(self)
        },
        set_trace_id = function(trace_id) {
          invisible(self)
        },
        set_span_id = function(span_id) {
          invisible(self)
        },
        set_trace_flags = function(trace_flags) {
          invisible(self)
        }
      ),
      class = c("otel_log_record_noop", "otel_log_record")
    )
    self
  }
)
