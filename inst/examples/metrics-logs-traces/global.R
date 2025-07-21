otel_tracer <- otel::get_tracer("kmeans-shiny-app")
otel_logger <- otel::get_logger("kmeans-shiny-app")
otel_meter <- otel::get_meter("kmeans-shiny-app")
otel_cnt_session <- otel_meter$create_counter("sessions")
otel_cnt_data_changes <- otel_meter$create_counter("data-changes")
otel_cnt_kmeans_runs <- otel_meter$create_counter("kmeans-runs")
otel_cnt_plots <- otel_meter$create_counter("plots")

otel_start_shiny_session <- function(session) {
  assign(
    "otel_span",
    otel_tracer$start_span("session", attr = otel_session_attr(session)),
    envir = session$userData
  )
}

otel_session_attr <- function(session) {
  attr <- list(
    PATH_INFO = session[["request"]][["PATH_INFO"]] %||% "",
    HTTP_HOST = session[["request"]][["HTTP_HOST"]] %||% "",
    HTTP_ORIGIN = session[["request"]][["HTTP_ORIGIN"]] %||% "",
    QUERY_STRING = session[["request"]][["QUERY_STRING"]] %||% "",
    SERVER_PORT = session[["request"]][["SERVER_PORT"]] %||% ""
  )
  try(attr[["SERVER_PORT"]] <- as.integer(attr[["SERVER_PORT"]]))

  session$onSessionEnded(function(...) {
    session$userData$otel_span$end()
  })
}
