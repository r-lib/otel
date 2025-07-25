#' OpenTelemetry tracing constants
#'
#' Various constants related OpenTelemetry tracing.
#'
#' @details
#' ## `invalid_trace_id`
#'
#' `invalid_trace_id` is a string scalar, an invalid trace id. If there is
#' no active span, then [get_active_span_context()] returns a span context
#' that has an invalid trace id.
#'
#' @format NULL
#' @name tracing-constants
#' @export
#' @family OpenTelemetry trace API
#' @return Not applicable.
#' @examples
#' invalid_trace_id

invalid_trace_id <- strrep("0", 32)

#' @details
#' ## `invalid_span_id`
#'
#' `invalid_span_id` is a string scalar, an invalid span id. If there is
#' no active span, then [get_active_span_context()] returns a span context
#' that has an invalid span id.
#'
#' @rdname tracing-constants
#' @format NULL
#' @export
#' @examples
#' invalid_span_id

invalid_span_id <- strrep("0", 16)

#' @details
#' ## `span_kinds`
#'
#' `span_kinds` is a character vector listing all possible span kinds.
#' See the [OpenTelemetry specification](
#'  https://opentelemetry.io/docs/specs/otel/trace/api/#spankind) for
#' when to use which.
#'
#' @rdname tracing-constants
#' @format NULL
#' @export
#' @examples
#' span_kinds

span_kinds <- c(
  default = "internal",
  "server",
  "client",
  "producer",
  "consumer"
)

#' @details
#' ## `span_status_codes`
#'
#' `span_status_codes` is a character vector listing all possible span
#' status codes. You can set the status code of a a span with the
#' `set_status()` method of [otel_span] objects. If not set explicitly,
#' and the span is ended automatically (by [start_local_active_span()],
#' [local_active_span()] or [with_active_span()]), then otel sets the
#' status automatically to "ok" or "error", depending on whether the span
#' ended during handling an error.
#'
#' @rdname tracing-constants
#' @format NULL
#' @export
#' @examples
#' span_status_codes

span_status_codes <- c(default = "unset", "ok", "error")
