# -------------------------------------------------------------------------
# Simplified API

#' Get a tracer from the default tracer provider
#'
#' Calls [get_default_tracer_provider()] to get the default tracer
#' provider. Then calls its `$get_tracer()` method to create a new tracer.
#'
#' @param name Name of the new tracer. This is typically the name of the
#'   package or project. Defaults to the name of the calling package,
#'   or the name of the current working directory if not called from a
#'   package.
#' @return An OpenTelemetry tracer, an `otel_tracer` object.
#' @export

get_default_tracer <- function(name = NULL) {
  name <- name %||% utils::packageName() %||% basename(getwd())
  # does setup if necessary
  tp <- get_default_tracer_provider()
  trc <- tp$get_tracer(name)
  invisible(trc)
}

#' Start a new OpenTelemetry span, using the default tracer
#'
#' @param name Name of the span.
#' @param session Optionally, an OpenTelemetry session to activate before
#'   starting the span. It can also be a Shiny session (`ShinySession`
#'   object), that was previously used as an argument to
#'   [start_shiny_session()].
#' @param ...,scope Additional arguments are passed to the default tracer's
#'   `start_span()` method.
#' @return The new Opentelemetry span object, invisibly.
#'
#' @export

start_span <- function(name = NULL, session = NULL, ...,
                       scope = parent.frame()) {
  # if no tracer, return something useful
  if (is.null(the$tracer)) {
    return(span_noop$new(name, ..., scope = scope))
  }
  if (!is.null(session)) {
    if (inherits(session, "ShinySession")) {
      session <- session$userData$otel_session
    }
    the$tracer$activate_session(session)
  }
  invisible(the$tracer$start_span(name = NULL, ..., scope = scope))
}
