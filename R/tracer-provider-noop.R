#' OpenTelemetry tracer provider objects
#'
#' The tracer provider defines how traces are exported when collecting
#' telemetry data. It is unlikely that you'd need to use tracer provider
#' objects directly.
#'
#' ## Implementations
#'
#' Note that this list is updated manually and may be incomplete.
#'
#' - [tracer_provider_noop]: No-op tracer provider, used when no traces are
#'   emitted.
#' - [otelsdk::tracer_provider_file]: Save traces to a JSONL file.
#' - [otelsdk::tracer_provider_http]: Send traces to a collector over
#'   HTTP/OTLP.
#' - [otelsdk::tracer_provider_memory]: Collect emitted traces in memory.
#'   For testing.
#' - [otelsdk::tracer_provider_stdstream]: Write traces to standard output
#'   or error or to a file. Primarily for debugging.
#'
#' ## Methods
#'
#' ### Method: `get_tracer()`
#'
#' #### Description
#'
#' Get or create a new tracer object.
#'
#' #### Usage
#'
#' ```
#' $get_tracer(
#'   name = NULL,
#'   version = NULL,
#'   schema_url = NULL,
#'   attributes = NULL
#' )
#' ```
#'
#' #### Arguments:
#'
#' - `name`: Tracer name, see [get_tracer()].
#' - `version`: Optional. Specifies the version of the instrumentation
#'   scope if the scope has a version (e.g. R package version).
#'   Example value: `"1.0.0"`.
#' - `schema_url`: Optional. Specifies the Schema URL that should be
#'   recorded in the emitted telemetry.
#' - `attributes`: Optional. Specifies the instrumentation scope
#'   attributes to associate with emitted telemetry.
#'
#' #### See also
#'
#' [get_tracer()].
#'
#' ### Method: `flush()`
#'
#' #### Description
#'
#' Force any buffered spans to flush. Tracer providers might not implement
#' this method.
#'
#' #### Usage
#'
#' ```
#' $flush()
#' ```
#'
#' @name otel_tracer_provider
NULL

#' No-op tracer provider
#' @keywords internal
#' @export

tracer_provider_noop <- list(
  new = function() {
    structure(
      list(
        get_tracer = function(
          name = NULL,
          version = NULL,
          schema_url = NULL,
          attributes = NULL
        ) {
          tracer_noop$new(name, version, schema_url, attributes)
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

        end = function(options = NULL, status_code = NULL) {
          invisible(self)
        },

        record_exception = function(attributes = NULL) {
          invisible(self)
        },

        activate = function(activation_scope, end_on_exit = FALSE) {
          invisible(self)
        }
      ),
      class = c("otel_span_noop", "otel_span")
    )
    self
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
