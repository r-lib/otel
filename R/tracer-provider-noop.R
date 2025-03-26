tracer_provider_noop <- list(
  new = function() {
    structure(
      list(
        get_tracer = function(name, ...) {
          tracer_noop$new(name, ...)
        },
        flush = function() {
          # noop
        }
      ),
      class = c(
        "otel_tracer_provider_noop",
        "otel_tracer_provider"
      )
    )
  }
)

tracer_noop <- list(
  new = function(name, ...) {
    if (!is_string(name)) {
      stop("Opentelemetry tracer name must be a string.")
    }
    structure(
      list(
        start_span = function(name, ...) {
          span_noop$new(name, ...)
        },
        is_enabled = function(...) FALSE,
        start_session = function() { },
        activate_session = function(session) { },
        deactivate_session = function(Session) { },
        finish_session = function(session) { },
        finish_all_sessions = function() { },
        flush = function() { }
      ),
      class = c("otel_tracer_noop", "otel_tracer")
    )
  }
)

span_noop <- list(
  new = function(name, ...) {
    if (!is_string(name)) {
      stop("Opentelemetry span name must be a string.")
    }
    self <- structure(
      list(
        get_context = function() {
          # TODO?
        },

        is_recording = function() {
          FALSE
        },

        set_attribute = function(name, value = NULL) {
          invisible(self)
        },

        add_event = function(name, attributes = NULL, timestamp = NULL) {
          invisible(self)
        },

        add_link = function(link) {
          invisible(self)
        },

        set_status = function(
          status_code = c("unset", "ok", "error"),
          description = NULL) {
            invisible(self)
          },

        update_name = function(name) {
          invisible(self)
        },

        end = function(options = NULL) {
          invisible(self)
        },

        record_exception = function(attributes = NULL) {
          invisible(self)
        }
      ),
      class = c("otel_span_noop", "otel_span")
    )
  }
)
