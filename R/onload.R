# nocov start
the <- new.env(parent = emptyenv())
the$mode <- "prod"
the$tracer_provider <- NULL
the$tracer_app <- NULL
the$span_app <- NULL

.onLoad <- function(libname, pkgname) {
  setup_dev_env()
  setup_r_trace()
}
#nocov end

setup_dev_env <- function() {
  ev <- tolower(Sys.getenv("OTEL_ENV"))
  if (ev %in% c("dev", "devel", "development")) {
    the$mode <- "dev"
    envir <- asNamespace(.packageName)
    assign(
      "get_default_tracer_provider",
      get_default_tracer_provider_dev,
      envir = envir
    )
    assign("get_tracer", get_tracer_dev, envir = envir)
    assign(
      "start_shiny_app",
      start_shiny_app_dev,
      envir = envir
    )
    assign(
      "start_shiny_session",
      start_shiny_session_dev,
      envir = envir
    )
    assign(
      "start_span",
      start_span_dev,
      envir= envir
    )
  }
}

setup_r_trace <- function() {
  ev <- Sys.getenv("OTEL_INSTRUMENT_R_PKGS", "")
  if (ev == "") return()

  if (!get_tracer()$is_enabled()) return()

  pkgs <- strsplit(ev, ",")
  for (pkg in pkgs) {
    if (pkg %in% loadedNamespaces()) {
      trace_namespace(pkg)
    } else {
      setHook(
        packageEvent(pkg, "onLoad"),
        function(...) otel::trace_namespace(pkg)
      )
    }
  }
}
