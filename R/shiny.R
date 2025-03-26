#' Start tracing a Shiny app
#'
#' Call this function from `global.R`.
#' @param service_name The name of the app. Defaults to the name of the
#'   current working directory.
#' @param ... Additional arguments are passed to `$start_span` for the
#'   `app` span.
#' @return The OpenTelemetry tracer (`otel_tracer`), invisibly.
#'
#' @export

start_shiny_app <- function(service_name = NULL, ...) {
  # TODO: do not error in PROD mode
  service_name <- service_name %||% basename(getwd())
  service_name <- as_string(service_name, null = FALSE)
  Sys.setenv(OTEL_SERVICE_NAME = service_name)
  .GlobalEnv$.tracer <- setup_default_tracer(service_name)
  .GlobalEnv$.span_app <- .GlobalEnv$.tracer$start_span("app", scope = NULL, ...)
  if (.GlobalEnv$.tracer$is_enabled()) {
    shiny::onStop(function() {
      .GlobalEnv$.tracer$finish_all_sessions()
      .GlobalEnv$.span_app$end()
    })
  }
  invisible(.GlobalEnv$.tracer)
}

#' Start tracing a Shiny session
#'
#' Call this function from the Shiny server function, at the beginning.
#' @param session Shiny session object.
#' @param attributes,options,... Additional arguments are passed to
#'   `$start_span()` for the `session` span.
#' @return The OpenTelemetry span corresponding to the Shiny session,
#'   invisibly.
#'
#' @export

start_shiny_session <- function(
    session, attributes = NULL, options = NULL, ...) {
  # if there is no tracer, return a noop span, so the caller can use it
  # TODO: error/warning in DEV mode
  if (is.null(.GlobalEnv$.tracer)) {
    return(invisible(span_noop$new("session", options, ...)))
  }
  # inactive tracer, probably noop
  if (!.GlobalEnv$.tracer$is_enabled()) {
    return(invisible(.GlobalEnv$.tracer$start_span("session", options, ...)))
  }

  attributes[["PATH_INFO"]] <- attributes[["PATH_INFO"]] %||%
    session[["request"]][["PATH_INFO"]] %||% ""
  attributes[["HTTP_HOST"]] <- attributes[["HTTP_HOST"]] %||%
    session[["request"]][["HTTP_HOST"]] %||% ""
  attributes[["HTTP_ORIGIN"]] <- attributes[["HTTP_ORIGIN"]] %||%
    session[["request"]][["HTTP_ORIGIN"]] %||% ""
  attributes[["QUERY_STRING"]] <- attributes[["QUERY_STRING"]] %||%
    session[["request"]][["QUERY_STRING"]] %||% ""
  attributes[["SERVER_PORT"]] <- attributes[["$SERVER_PORT"]] %||%
    session[["request"]][["SERVER_PORT"]] %||% -1L
  try(attributes[["SERVER_PORT"]] <-
    as.integer(attributes[["SERVER_PORT"]]))

  options[["parent"]] <- options[["parent"]] %||% .GlobalEnv$.span_app

  assign(
    "otel_session",
    .GlobalEnv$.tracer$start_session(),
    envir = session$userData
  )
  assign(
    "session_span",
    .GlobalEnv$.tracer$start_span(
      "session",
      attributes = attributes,
      options = options,
      scope = NULL,
      ...
    ),
    envir = session$userData
  )
  session$onSessionEnded(function(...) {
    session$userData$session_span$end()
    .GlobalEnv$.tracer$finish_session(session$userData$otel_session)
  })

  invisible(session$userData$session_span)
}
