#' Start tracing a Shiny app
#'
#' Call this function from `global.R`.
#' @param service_name The name of the app.
#' @return The OpenTelemetry tracer (`opentelemetry_tracer`), invisibly.
#'
#' @export

start_shiny_app <- function(service_name) {
  service_name <- as_string(service_name, null = FALSE)
  Sys.setenv(OTEL_SERVICE_NAME = service_name)
  .GlobalEnv$.tracer <- setup_default_tracer(service_name)
  .GlobalEnv$.span_app <- .GlobalEnv$.tracer$start_span("app", scope = NULL)
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
#' @return The OpenTelemetry span corresponding to the Shiny session,
#'   invisibly.
#'
#' @export

start_shiny_session <- function(session) {
  if (!.GlobalEnv$.tracer$is_enabled()) {
    return(invisible(.GlobalEnv$.tracer$start_span("session")))
  }

  assign(
    "otel_session",
    .GlobalEnv$.tracer$start_session(),
    envir = session$userData
  )
  assign(
    "session_span",
    .GlobalEnv$.tracer$start_span(
      "session",
      options = list(parent = .GlobalEnv$.span_app),
      scope = NULL
    ),
    envir = session$userData
  )
  session$onSessionEnded(function(...) {
    session$userData$session_span$end()
    .GlobalEnv$.tracer$finish_session(session$userData$otel_session)
  })

  invisible(session$userData$session_span)
}
