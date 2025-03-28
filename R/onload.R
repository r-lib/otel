# nocov start
the <- new.env(parent = emptyenv())
the$mode <- "dev"
the$tracer_provider <- NULL
the$tracer <- NULL
the$span_app <- NULL

.onLoad <- function(libname, pkgname) {
  ev <- tolower(Sys.getenv("OTEL_ENV"))
  if (! ev %in% c("dev", "devel", "development")) {
    the$mode <- "prod"
    envir <- asNamespace(.packageName)
    assign(
      "get_default_tracer_provider",
      get_default_tracer_provider_safe,
      envir = envir
    )
    assign(
      "get_default_tracer",
      get_default_tracer_safe,
      envir = envir
    )
    assign(
      "start_shiny_app",
      start_shiny_app_safe,
      envir = envir
    )
    assign(
      "start_shiny_session",
      start_shiny_session_safe,
      envir = envir
    )
    assign(
      "start_span",
      start_span_safe,
      envir= envir
    )
  }
}
#nocov end