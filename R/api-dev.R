
get_tracer_dev <- function(name = NULL) {
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    # does setup if necessary
    tp <- get_default_tracer_provider()
    trc <- tp$get_tracer(name)
    invisible(trc)
}

get_logger_dev <- function(name = NULL) {
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    # does setup if necessary
    tp <- get_default_logger_provider()
    trc <- tp$get_logger(name)
    invisible(trc)
}

get_meter_dev <- function(name = NULL) {
    name <- name %||%
      utils::packageName() %||%
      get_env("OTEL_SERVICE_NAME") %||%
      basename(getwd())
    # does setup if necessary
    tp <- get_default_meter_provider()
    trc <- tp$get_meter(name)
    invisible(trc)
}

start_span_dev <- function(name = NULL, session = NULL, ...,
                       scope = parent.frame()) {
    trc <- get_tracer()
    if (!is.null(session)) {
      if (inherits(session, "ShinySession")) {
        session <- session$userData$otel_session
      }
      trc$activate_session(session)
    }
    invisible(trc$start_span(name = name, ..., scope = scope))
}

get_default_tracer_provider_dev <- function() {
    if (is.null(the$tracer_provider)) {
      setup_default_tracer_provider()
    }
    the$tracer_provider
}

get_default_logger_provider_dev <- function() {
    if (is.null(the$logger_provider)) {
      setup_default_logger_provider()
    }
    the$logger_provider
}

get_default_meter_provider_dev <- function() {
    if (is.null(the$meter_provider)) {
      setup_default_meter_provider()
    }
    the$meter_provider
}

start_shiny_app_dev <- function(service_name = NULL, ...) {
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
}

start_shiny_session_dev <- function(
    session, attributes = NULL, options = NULL, ...) {
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
    attributes[["SERVER_PORT"]] <- attributes[["$SERVER_PORT"]] %||%
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
}
