#' Start tracing a Shiny app
#'
#' Call this function from `global.R`.
#' @param service_name The name of the app. Defaults to the name of the
#'   current working directory.
#' @return The OpenTelemetry tracer (`otel_tracer`), invisibly.
#'
#' @export

# safe start
start_shiny_app <- function(service_name = NULL) {
  tryCatch({                                                         # safe
    service_name <- service_name %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    service_name <- as_string(service_name, null = FALSE)
    Sys.setenv(OTEL_SERVICE_NAME = service_name)
    the$tracer_app <- get_tracer(service_name)
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
    # inactive tracer, do nothing, but return a (session) span
    if (!trc$is_enabled()) {
      return(invisible(trc$start_session("session", options, ...)))
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

    assign(
      "otel_session",
      trc$start_session(
        "session",
        attributes = attributes,
        options = options,
        scope = NULL,
        ...
      ),
      envir = session$userData
    )
    session$onSessionEnded(function(...) {
      session$userData$otel_session$end()
    })

    invisible(session$userData$otel_session)
  }, error = function(err) {                                         # safe
    errmsg("OpenTelemetry error: ", conditionMessage(err))           # safe
    invisible(session_noop$new())                                    # safe
  })                                                                 # safe
}
# safe end

start_shiny_session_safe <- start_shiny_session
