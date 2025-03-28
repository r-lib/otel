Sys.setenv(OTEL_ENV = "dev")
pkgload::load_all()
output_file <- Sys.getenv("OTEL_SAFE_API_OUTPUT_FILE", "R/api-safe.R")

dump <- function(...) suppressWarnings(base::dump(...))

# -------------------------------------------------------------------------

get_default_tracer_provider_safe <- get_default_tracer_provider
body(get_default_tracer_provider_safe) <- substitute({
  tryCatch({
    body
  }, error = function(err) {
    # TODO: message
    tracer_provider_noop$new()
  })
}, list(body = body(get_default_tracer_provider)))

dump("get_default_tracer_provider_safe", file = output_file)

# -------------------------------------------------------------------------

get_default_tracer_safe <- get_default_tracer
body(get_default_tracer_safe) <- substitute({
  tryCatch({
    body
  }, error = function(err) {
    # TODO: message
    tracer_noop$new()
  })
}, list(body = body(get_default_tracer)))

dump("get_default_tracer_safe", file = output_file, append = TRUE)

# -------------------------------------------------------------------------

start_shiny_app_safe <- start_shiny_app
body(start_shiny_app_safe) <- substitute({
  tryCatch({
    body
  }, error = function(err) {
    # TODO: message
    invisible(tracer_noop$new())
  })
}, list(body = body(start_shiny_app)))

dump("start_shiny_app_safe", file = output_file, append = TRUE)

# -------------------------------------------------------------------------

start_shiny_session_safe <- start_shiny_session
body(start_shiny_session_safe) <- substitute({
  tryCatch({
    body
  }, error = function(err) {
    # TODO: message
    invisible(span_noop$new())
  })
}, list(body = body(start_shiny_session)))

dump("start_shiny_session_safe", file = output_file, append = TRUE)

# -------------------------------------------------------------------------

start_span_safe <- start_span
body(start_span_safe) <- substitute({
  tryCatch({
    body
  }, error = function(err) {
    # TODO: message
    invisible(span_noop$new())
  })
}, list(body = body(start_span)))

dump("start_span_safe", file = output_file, append = TRUE)

# -------------------------------------------------------------------------
