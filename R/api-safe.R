get_default_tracer_provider_safe <-
function () 
{
    tryCatch({
        {
            if (is.null(the$tracer_provider)) {
                setup_default_tracer_provider()
            }
            the$tracer_provider
        }
    }, error = function(err) {
        tracer_provider_noop$new()
    })
}
get_default_tracer_safe <-
function (name = NULL) 
{
    tryCatch({
        {
            name <- name %||% utils::packageName() %||% basename(getwd())
            tp <- get_default_tracer_provider()
            trc <- tp$get_tracer(name)
            invisible(trc)
        }
    }, error = function(err) {
        tracer_noop$new()
    })
}
start_shiny_app_safe <-
function (service_name = NULL, ...) 
{
    tryCatch({
        {
            service_name <- service_name %||% basename(getwd())
            service_name <- as_string(service_name, null = FALSE)
            Sys.setenv(OTEL_SERVICE_NAME = service_name)
            the$tracer <- get_default_tracer(service_name)
            the$span_app <- the$tracer$start_span("app", scope = NULL, 
                ...)
            if (the$tracer$is_enabled()) {
                shiny::onStop(function() {
                  the$tracer$finish_all_sessions()
                  the$span_app$end()
                })
            }
            invisible(the$tracer)
        }
    }, error = function(err) {
        invisible(tracer_noop$new())
    })
}
start_shiny_session_safe <-
function (session, attributes = NULL, options = NULL, ...) 
{
    tryCatch({
        {
            if (is.null(the$tracer)) {
                return(invisible(span_noop$new("session", options, 
                  ...)))
            }
            if (!the$tracer$is_enabled()) {
                return(invisible(the$tracer$start_span("session", 
                  options, ...)))
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
            try(attributes[["SERVER_PORT"]] <- as.integer(attributes[["SERVER_PORT"]]))
            options[["parent"]] <- options[["parent"]] %||% the$span_app
            assign("otel_session", the$tracer$start_session(), 
                envir = session$userData)
            assign("session_span", the$tracer$start_span("session", 
                attributes = attributes, options = options, scope = NULL, 
                ...), envir = session$userData)
            session$onSessionEnded(function(...) {
                session$userData$session_span$end()
                the$tracer$finish_session(session$userData$otel_session)
            })
            invisible(session$userData$session_span)
        }
    }, error = function(err) {
        invisible(span_noop$new())
    })
}
start_span_safe <-
function (name = NULL, session = NULL, ..., scope = parent.frame()) 
{
    tryCatch({
        {
            if (is.null(the$tracer)) {
                return(span_noop$new(name, ..., scope = scope))
            }
            if (!is.null(session)) {
                if (inherits(session, "ShinySession")) {
                  session <- session$userData$otel_session
                }
                the$tracer$activate_session(session)
            }
            invisible(the$tracer$start_span(name = NULL, ..., 
                scope = scope))
        }
    }, error = function(err) {
        invisible(span_noop$new())
    })
}
