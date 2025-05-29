#' OpenTelemetry tracing constants
#'
#' Various constants related OpenTelemetry tracing.
#'
#' `invalid_trace_id` is a string scalar, an invalid trace id.
#'
#' @name tracing
#' @export

invalid_trace_id <- strrep("0", 32)

#' @details
#' `invalid_span_id` is a string scalar, an invalid span id.
#' @rdname tracing
#' @export

invalid_span_id <- strrep("0", 16)

#' @details
#' `span_kinds` is a character vector listing all possible span kinds.
#' @rdname tracing
#' @export

span_kinds <- c(
  default = "internal",
  "server",
  "client",
  "producer",
  "consumer"
)

#' @details
#' `span_status_codes` is a character vector listing all possible span
#' status codes.
#' @rdname tracing
#' @export

span_status_codes <- c(default = "unset", "ok", "error")
