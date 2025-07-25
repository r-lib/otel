#' OpenTelemetry Tracer Provider Object
#'
#' @description
#' [otel_tracer_provider] -> [otel_tracer] -> [otel_span] -> [otel_span_context]
#'
#' @details
#' The tracer provider defines how traces are exported when collecting
#' telemetry data. It is unlikely that you'd need to use tracer provider
#' objects directly.
#'
#' Usually there is a single tracer provider for an R app or script.
#'
#' Typically the tracer provider is created automatically, at the first
#' [start_local_active_span()] or [start_span()] call. otel decides which
#' tracer provider class to use based on [Environment Variables].
#'
#' # Implementations
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
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `tracer_provider$get_tracer()`
#'
#' Get or create a new tracer object.
#'
#' ### Usage
#'
#' ```r
#' tracer_provider$get_tracer(
#'   name = NULL,
#'   version = NULL,
#'   schema_url = NULL,
#'   attributes = NULL
#' )
#' ```
#'
#' ### Arguments
#'
#' - `name`: Tracer name, see [get_tracer()].
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
#' Returns an OpenTelemetry tracer ([otel_tracer]) object.
#'
#' ### See also
#'
#' [get_default_tracer_provider()], [get_tracer()].
#'
# -------------------------------------------------------------------------
#' ## `tracer_provider$flush()`
#'
#' Force any buffered spans to flush. Tracer providers might not implement
#' this method.
#'
#' ### Usage
#'
#' ```
#' tracer_provider$flush()
#' ```
#'
#' ### Value
#'
#' Nothing.
#'
#' @name otel_tracer_provider
#' @return Not applicable.
#' @family low level trace API
#' @examples
#' tp <- otel::get_default_tracer_provider()
#' trc <- tp$get_tracer()
#' trc$is_enabled()
NULL

#' No-op tracer provider
#'
#' This is the tracer provider ([otel_tracer_provider]) otel uses when
#' tracing is disabled.
#'
#' All methods are no-ops or return objects that are also no-ops.
#'
#' @family low level trace API
#' @usage NULL
#' @format NULL
#' @export
#' @return Not applicable.
#' @examples
#' tracer_provider_noop$new()

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

#' OpenTelemetry Tracer Object
#' @name otel_tracer
#' @family low level trace API
#' @description
#' [otel_tracer_provider] -> [otel_tracer] -> [otel_span] -> [otel_span_context]
#'
#' @details
#' Usually you do not need to deal with otel_tracer objects directly.
#' [start_local_active_span()] (and [start_span()]) automatically
#' sets up the tracer and uses it to create spans.
#'
#' A tracer object is created by calling the `get_tracer()` method of an
#' [otel_tracer_provider].
#'
#' You can use the `start_span()` method of the  tracer object to create a
#' span.
#'
#' Typically there is a separate tracer object for each instrumented R
#' package.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `tracer$start_span()`
#'
#' Creates and starts a new span.
#'
#' It does not activate the new span.
#'
#' It is equivalent to the [start_span()] function.
#'
#' ### Usage
#'
#' ```r
#' tracer_start_span(
#'   name = NULL,
#'   attributes = NULL,
#'   links = NULL,
#'   options = NULL
#' )
#' ```
#'
#' ### Arguments
#'
#' - `name`: `r doc_arg()[["span-name"]]`
#' - `attributes`: `r doc_arg()[["attributes"]]`
#' - `links`: `r doc_arg()[["links"]]`
#' - `options`: `r paste0("  ", doc_arg()[["span-options"]])`
#'
#' ### Value
#'
#' A new [otel_span] object.
#'
# -------------------------------------------------------------------------
#' ## `tracer$is_enabled()`
#'
#' Whether the tracer is active and recording traces.
#'
#' This is equivalent to the [is_tracing_enabled()] function.
#'
#' ### Usage
#'
#' ```r
#' tracer$is_enabled()
#' ```
#'
#' ### Value
#'
#' Logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `tracer$flush()`
#'
#' Flush the tracer provider: force any buffered spans to flush. Tracer
#' providers might not implement this method.
#'
#' ### Usage
#'
#' ```r
#' tracer$flush()
#' ```
#'
#' ### Value
#'
#' Nothing.
#'
#' @return Not applicable.
#' @examples
#' tp <- get_default_tracer_provider()
#' trc <- tp$get_tracer()
#' trc$is_enabled()
NULL

tracer_noop <- list(
  new = function(name = NULL, attributes = NULL, links = NULL, options = NULL) {
    structure(
      list(
        start_span = function(
          name = NULL,
          attributes = NULL,
          links = NULL,
          options = NULL
        ) {
          span_noop$new(
            name = name,
            attributes = attributes,
            links = links,
            options = options
          )
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

#' OpenTelemetry Span Object
#'
#' @name otel_span
#' @family low level trace API
#' @description
#' [otel_tracer_provider] -> [otel_tracer] -> [otel_span] -> [otel_span_context]
#'
#' @details
#' An otel_span object represents an OpenTelemetry span.
#'
#' Use [start_local_active_span()] or [start_span()] to create and start
#' a span.
#'
#' Call [end_span()] to end a span explicitly. (See
#' [start_local_active_span()] and [local_active_span()] to end a span
#' automatically.)
#'
#' # Lifetime
#'
#' The span starts when it is created in the [start_local_active_span()]
#' or [start_span()] call.
#'
#' The span ends when [end_span()] is called on it, explicitly or
#' automatically via [start_local_active_span()] or [local_active_span()].
#'
#' # Activation
#'
#' After a span is created it may be active or inactively, independently
#' of its lifetime. A live span (i.e. a span that hasn't ended yet) may
#' be inactive. While this is less common, a span that has ended may still
#' be active.
#'
#' When otel creates a new span, it sets the parent span of the new span
#' to the active span by default.
#'
#' ## Automatic spans
#'
#' [start_local_active_span()] creates a new span, starts it and activates
#' it for the caller frame. It also automatically ends the span when the
#' caller frame exits.
#'
#' ## Manual spans
#'
#' [start_span()] creates a new span and starts it, but it does not
#' activate it. You must activate the span manually using
#' [local_active_span()] or [with_active_span()]. You must also end the
#' span manually with an [end_span()] call. (Or the `end_on_exit` argument
#' of [local_active_span()] or [with_active_span()].)
#'
#' # Parent spans
#'
#' OpenTelemetry spans form a hierarchy: a span can refer to a parent span.
#' A span without a parent span is called a root span. A trace is a set of
#' connected spans.
#'
#' When otel creates a new span, it sets the parent span of the new span
#' to the active span by default.
#'
#' Alternatively, you can set the parent span of the new span manually.
#' You can also make the new span be a root span, by setting `parent = NA`
#' in `options` to the [start_local_active_span()] or [start_span()] call.
#'
#' # Methods

# we cannot document these inline, because then roxygen2 adds the 'span_noop'
# alias to the manual page.

# -------------------------------------------------------------------------
#' ## `span$add_event()`
#'
#' Add a single event to the span.
#'
#' ### Usage
#'
#' ```r
#' span$add_event(name, attributes = NULL, timestamp = NULL)
#' ```
#'
#' ### Arguments
#'
#' * `name`: Event name.
#' * `attributes`: Attributes to add to the event. See [as_attributes()]
#'   for supported R types. You may also use [as_attributes()] to convert
#'   an R object to an OpenTelemetry attribute value.
#' * `timestamp`: A [base::POSIXct] object. If missing, the current time is
#'   used.
#'
#' ### Value
#'
#' The span object itself, invisibly.
#'
# -------------------------------------------------------------------------
#' ## `span$end()`
#'
#' End the span. Calling this method is equivalent to calling the
#' [end_span()] function on the span.
#'
#' Spans created with [start_local_active_span()] end automatically by
#' default. You must end every other span manually, by calling `end_span`,
#' or using the `end_on_exit` argument of [local_active_span()] or
#' [with_active_span()].
#'
#' Calling the `span$end()` method (or `end_span()`) on a span multiple
#' times is not an error, the first call ends the span, subsequent calls do
#' nothing.
#'
#' ### Usage
#'
#' ```r
#' span$end(options = NULL, status_code = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `options`: Named list of options. Possible entry:
#'   * `end_steady_time`: A [base::POSIXct] object that will be used as
#'     a steady timer.
#' - `status_code`: Span status code to set before ending the span, see
#'   the `span$set_status()` method for possible values.
#'
#' ### Value
#'
#' The span object itself, invisibly.
#'
# -------------------------------------------------------------------------
#' ## `span$get_context()`
#'
#' Get a span's span context. The span context is an [otel_span_context]
#' object that can be serialized, copied to other processes, and it can be
#' used to create new child spans.
#'
#' ### Usage
#'
#' ```r
#' span$get_context()
#' ```
#'
#' ### Value
#'
#' An [otel_span_context] object.
#'
# -------------------------------------------------------------------------
#' ## `span$is_recording()`
#'
#' Checks whether a span is recorded. If tracing is off, or the span ended
#' already, or the sampler decided not to record the trace the span belongs
#' to.
#'
#' ### Usage
#'
#' ```r
#' span$is_recording()
#' ```
#'
#' ### Value
#'
#' A logical scalar, `TRUE` if the span is recorded.
#'
# -------------------------------------------------------------------------
#' ## `span$record_exception()`
#'
#' Record an exception (error, usually) event for a span.
#'
#' If the span was created with [start_local_active_span()], or it was
#' ended automatically with [local_active_span()] or [with_active_span()],
#' then otel records exceptions automatically, and you don't need to call
#' this function manually.
#'
#' You can still use it to record exceptions that are not R errors.
#'
#' ### Usage
#'
#' ```r
#' span$record_exception(error_condition, attributes, ...)
#' ```
#'
#' ### Arguments
#'
#' - `error_condition`: An R error object to record.
#' - `attributes`: Additional attributes to add to the exception event.
#' - `...`: Passed to the `span$add_event()` method.
#'
#' ### Value
#'
#' The span object itself, invisibly.
#'
# -------------------------------------------------------------------------
#' ## `span$set_attribute()`
#'
#' Set a single attribute. It is better to set attributes at span creation,
#' instead of calling this method later, since samplers can only make
#' decisions based on attributes present at span creation.
#'
#' ### Usage
#'
#' ```r
#' span$set_attribute(name, value)
#' ```
#'
#' ### Arguments
#'
#' * `name`: Attribute name.
#' * `value`: Attribute value. See [as_attributes()] for supported R types.
#'   You may also use [as_attributes()] to convert an R object to an
#'   OpenTelemetry attribute value.
#'
#' ### Value
#'
#' The span object itself, invisibly.
#'
# -------------------------------------------------------------------------
#' ## `span$set_status()`
#'
#' Set the status of the span.
#'
#' If the span was created with [start_local_active_span()], or it was
#' ended automatically with [local_active_span()] or [with_active_span()],
#' then otel sets the status of the span automatically to `ok` or `error`,
#' depending on whether an error happened in the frame the span was
#' activated for.
#'
#' Otherwise the default span status is `unset`, and you need to set it
#' manually.
#'
#' ### Usage
#'
#' ```r
#' span$set_status(status_code, description = NULL)
#' ```
#'
#' ### Arguments
#'
#' * `status_code`: Possible values:
#'   `r paste(span_status_codes, collapse = ", ")`.
#' * `description`: Optional description, a string.
#'
#' ### Value
#'
#' The span itself, invisibly.
#'
# -------------------------------------------------------------------------
#' ## `span$update_name()`
#'
#' Update the span's name. Overrides the name give in
#' [start_local_active_span()] or [start_span()].
#'
#' It is undefined whether a sampler will use the original or the new name.
#'
#' ### Usage
#'
#' ```r
#' span$update_name(name)
#' ```
#'
#' ### Arguments
#'
#' - `name`: String, the new span name.
#'
#' ### Value
#'
#' The span object itself, invisibly.
#'
#' @return Not applicable.
#' @examples
#' fn <- function() {
#'   trc <- otel::get_tracer("myapp")
#'   spn <- trc$start_span("fn")
#'   # ...
#'   spn$set_attribute("key", "value")
#'   # ...
#'   on.exit(spn$end(status_code = "error"), add = TRUE)
#'   # ...
#'   spn$end(status_code = "ok")
#' }
#' fn()
NULL

span_noop <- list(
  new = function(name = "", ...) {
    self <- structure(
      list(
        get_context = function() {
          span_context_noop$new()
        },

        # We don't need this currently, because we never return an invalid
        # span, `get_active_span_context()` returns an invalid span context,
        # but we don't have a `get_active_span()` function.
        is_valid = function() {
          FALSE
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

        # TODO: this is not implemented in otelsdk
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
    self$name <- name %||% "<NA>"
    self
  }
)

#' An OpenTelemetry Span Context object
#'
#' @description
#' [otel_tracer_provider] -> [otel_tracer] -> [otel_span] -> [otel_span_context]
#'
#' @details
#' This is a representation of a span that can be serialized, copied to
#' other processes, and it can be used to create new child spans.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `span_context$get_span_id()`
#'
#' Get the id of the span.
#'
#' ### Usage
#'
#' ```r
#' span_context$get_span_id()
#' ```
#'
#' ### Value
#'
#' String scalar, a span id. For invalid spans it is [invalid_span_id].
#'
# -------------------------------------------------------------------------
#' ## `span_context$get_trace_flags()`
#'
#' Get the trace flags of a span.
#'
#' See the [specification](https://w3c.github.io/trace-context/#trace-flags)
#' for more details on trace flags.
#'
#' ### Usage
#'
#' ```r
#' span_context$get_trace_flags()
#' ```
#'
#' ### Value
#'
#' A list with entries:
#' * `is_sampled`: logical flag, whether the trace of the span is sampled.
#'   If `FALSE` then the caller is not recording the trace. See details in
#'   the [specification](https://w3c.github.io/trace-context/#sampled-flag).
#' * `is_random`: logical flag, it specifies how trace ids are generated.
#'   See details in the [specification](
#'   https://w3c.github.io/trace-context/#random-trace-id-flag).
#'
# -------------------------------------------------------------------------
#' ## `span_context$get_trace_id()`
#'
#' Get the id of the trace the span belongs to.
#'
#' ### Usage
#'
#' ```r
#' span_context$get_trace_id()
#' ```
#'
#' ### Value
#'
#' A string scalar, a trace id. For invalid spans it is [invalid_trace_id].
#'
# -------------------------------------------------------------------------
#' ## `span_context$is_remote()`
#'
#' Whether the span was propagated from a remote parent.
#'
#' ### Usage
#'
#' ```r
#' span_context$is_remote()
#' ```
#'
#' ### Value
#'
#' A logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `span_context$is_sampled()`
#'
#' Whether the span is sampled. This is the same as the `is_sampled`
#' trace flags, see `get_trace_flags()` above.
#'
#' ### Usage
#'
#' ```r
#' span_context$is_sampled()
#' ```
#'
#' ### Value
#'
#' Logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `span_context$is_valid()`
#'
#' Whether the span is valid. Sometimes otel functions return an
#' invalid span or a span context referring to an invalid span. E.g.
#' [get_active_span_context()] does that if there is no active span.
#'
#' `is_valid()` checks if the span is valid.
#'
#' An span id of an invalid span is [invalid_span_id].
#'
#' ### Usage
#'
#' ```r
#' span_context$is_valid()
#' ```
#'
#' ### Value
#'
#' A logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `span_context$to_http_headers()`
#'
#' Serialize the span context into one or more HTTP headers that can
#' be transmitted to other processes or servers, to create a distributed
#' trace.
#'
#' The other process can deserialize these headers into a span context that
#' can be used to create new remote spans.
#'
#' ### Usage
#'
#' ```r
#' span_context$to_http_headers()
#' ```
#'
#' ### Value
#'
#' A named character vector, the HTTP header representation of the span
#' context. Usually includes a `traceparent` header. May include other
#' headers.
#'
#' @name otel_span_context
#' @family low level trace API
#' @return Not applicable.
#' @examples
#' spc <- get_active_span_context()
#' spc$get_trace_flags()
#' spc$get_trace_id()
#' spc$get_span_id()
#' spc$is_remote()
#' spc$is_sampled()
#' spc$is_valid()
#' spc$to_http_headers()
NULL

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
