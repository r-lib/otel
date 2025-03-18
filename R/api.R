# -------------------------------------------------------------------------
# Simplified API

#' Get a tracer from the default tracer provider
#'
#' Calls [get_default_tracer_provider()] to get the default tracer
#' provider. Then calls its `$get_tracer()` method to create a new tracer.
#'
#' @param name Name of the new tracer. This is typically the R package
#'   name.
#' @return An OpenTelemetry tracer, an `opentelemetry_tracer` object.
#' @export

setup_default_tracer <- function(name) {
  # does setup if necessary
  tp <- get_default_tracer_provider()
  trc <- tp$get_tracer(name)
  invisible(trc)
}

#' Start a new OpenTelemetry span, using the default tracer
#'
#' The default tracer is stored as `.tracer`, in the global environment.
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

start_span <- function(name, session = NULL, ..., scope = parent.frame()) {
  if (!is.null(session)) {
    if (inherits(session, "ShinySession")) {
      session <- session$userData$otel_session
    }
    .GlobalEnv$.tracer$activate_session(session)
  }
  invisible(.GlobalEnv$.tracer$start_span(name, ..., scope = scope))
}
