logger_provider_noop <- list(
  new = function() {
    structure(
      list(
        get_logger = function(name = NULL, ...) {
          logger_noop$new(name, ...)
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

logger_noop <- list(
  new = function(name = NULL, minimum_severity = "warn", ...) {
    self <- structure(
      list(
        get_name = function() "default-logger",
        create_log_record = function() {
          log_record_noop$new()
        },
        emit_log_record = function(log_record, ...) {
          invisible(self)
        },
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
        is_enabled = function(severity = "warn", event_id = NULL) {
          FALSE
        },
        get_minimum_severity = function() {
          c("maximumseverity" = 255L)
        },
        set_minimum_severity = function(minimum_severity) {
          invisible(self)
        },
        log = function(...) {
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
