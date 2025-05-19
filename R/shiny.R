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

# safe start
start_shiny_app <- function(service_name = NULL, ...) {
  tryCatch({                                                         # safe
    service_name <- service_name %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    service_name <- as_string(service_name, null = FALSE)
    Sys.setenv(OTEL_SERVICE_NAME = service_name)
    the$tracer_app <- get_tracer(service_name)
    the$span_app <- the$tracer_app$start_span("app", scope = NULL, ...)
    if (the$tracer_app$is_enabled()) {
      shiny::onStop(function() {
        the$tracer_app$finish_all_sessions()
        the$span_app$end()
      })
    }
    invisible(the$tracer_app)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(tracer_noop$new())                                     # safe
  })                                                                 # safe
}
# safe end

start_shiny_app_safe <- start_shiny_app

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

# safe start
start_shiny_session <- function(
    session, attributes = NULL, options = NULL, ...) {
  tryCatch({                                                         # safe
    name <- get_env("OTEL_SERVICE_NAME")
    trc <- get_tracer(name)
    # inactive tracer, do nothing, but return a span
    if (!trc$is_enabled()) {
      return(invisible(trc$start_span("session", options, ...)))
    }

    attributes[["PATH_INFO"]] <- attributes[["PATH_INFO"]] %||%
      session[["request"]][["PATH_INFO"]] %||% ""
    attributes[["HTTP_HOST"]] <- attributes[["HTTP_HOST"]] %||%
      session[["request"]][["HTTP_HOST"]] %||% ""
    attributes[["HTTP_ORIGIN"]] <- attributes[["HTTP_ORIGIN"]] %||%
      session[["request"]][["HTTP_ORIGIN"]] %||% ""
    attributes[["QUERY_STRING"]] <- attributes[["QUERY_STRING"]] %||%
      session[["request"]][["QUERY_STRING"]] %||% ""
    attributes[["SERVER_PORT"]] <- attributes[["SERVER_PORT"]] %||%
      session[["request"]][["SERVER_PORT"]] %||% -1L
    try(attributes[["SERVER_PORT"]] <-
      as.integer(attributes[["SERVER_PORT"]]))

    options[["parent"]] <- options[["parent"]] %||% the$span_app

    assign(
      "otel_session",
      trc$start_session(),
      envir = session$userData
    )
    assign(
      "session_span",
      trc$start_span(
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
      trc$finish_session(session$userData$otel_session)
    })

    invisible(session$userData$session_span)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(span_noop$new())                                       # safe
  })                                                                 # safe
}
# safe end

start_shiny_session_safe <- start_shiny_session
