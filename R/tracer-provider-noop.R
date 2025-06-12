tracer_provider_noop <- list(
  new = function() {
    structure(
      list(
        get_tracer = function(name = NULL, ...) {
          tracer_noop$new(name, ...)
        },
        flush = function() {
          # noop
        },
        get_spans = function() {
          list()
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
  new = function(name = NULL, ...) {
    structure(
      list(
        start_span = function(name = NULL, ...) {
          span_noop$new(name, ...)
        },
        get_active_span_context = function() span_context_noop$new(),
        is_enabled = function() FALSE,
        start_session = function(name = NULL, ...) {
          session_noop$new(name, ...)
        },
        flush = function() {},
        extract_http_context = function(headers) {
          span_context_noop$new()
        }
      ),
      class = c("otel_tracer_noop", "otel_tracer")
    )
  }
)

span_noop <- list(
  new = function(name = "", ...) {
    self <- structure(
      list(
        get_context = function() {
          span_context_noop$new()
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
          description = NULL
        ) {
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
    self
  }
)

session_noop <- list(
  new = function(name = "", ...) {
    s <- span_noop$new(name = name, ...)
    s$activate_session <- function() {}
    s$deactivate_session <- function() {}
    class(s) <- c("otel_session", class(s))
    s
  }
)

span_context_noop <- list(
  new = function(...) {
    self <- structure(
      list(
        is_valid = function() {
          FALSE
        },
        get_trace_flags = function() {
          list()
        },
        get_trace_id = function() {
          invalid_trace_id
        },
        get_span_id = function() {
          invalid_span_id
        },
        is_remote = function() {
          FALSE
        },
        is_sampled = function() {
          FALSE
        },
        to_http_headers = function() {
          structure(character(), names = character())
        }
      ),
      class = c("otel_span_context_noop", "otel_span_context")
    )
    self
  }
)
