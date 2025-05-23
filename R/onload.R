otel_clean_cache <- function() {
  the$tracer_provider <- NULL
  the$logger_provider <- NULL
  the$meter_provider <- NULL
  the$tracer_app <- NULL
  the$span_app <- NULL
}

# nocov start
the <- new.env(parent = emptyenv())
the$mode <- "prod"

.onLoad <- function(libname, pkgname) {
  otel_clean_cache()
  setup_dev_env()
  setup_r_trace()
}
#nocov end

setup_dev_env <- function(envir = asNamespace(.packageName)) {
  ev <- tolower(Sys.getenv("OTEL_ENV"))
  if (ev %in% c("dev", "devel", "development")) {
    the$mode <- "dev"
    assign(
      "get_default_tracer_provider",
      get_default_tracer_provider_dev,
      envir = envir
    )
    assign("get_tracer", get_tracer_dev, envir = envir)
    assign(
      "get_default_logger_provider",
      get_default_logger_provider_dev,
      envir = envir
    )
    assign("get_logger", get_logger_dev, envir = envir)
    assign(
      "get_default_meter_provider",
      get_default_meter_provider_dev,
      envir = envir
    )
    assign("get_meter", get_meter_dev, envir = envir)
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
      envir = envir
    )
    assign(
      "get_current_span_context",
      get_current_span_context_dev,
      envir = envir
    )
    assign(
      "extract_http_context",
      extract_http_context_dev,
      envir = envir
    )
  }
}

setup_r_trace <- function() {
  ev <- trimws(Sys.getenv("OTEL_INSTRUMENT_R_PKGS", ""))
  if (ev == "") return()

  if (!get_tracer()$is_enabled()) return()

  pkgs <- strsplit(ev, ",", fixed = TRUE)[[1]]
  for (pkg in pkgs) {
    PKG <- gsub(".", "_", toupper(pkg), fixed = TRUE)
    inc <- get_env(paste0("OTEL_INSTRUMENT_R_PKGS_", PKG, "_INCLUDE"))
    exc <- get_env(paste0("OTEL_INSTRUMENT_R_PKGS_", PKG, "_EXCLUDE"))
    if (!is.null(inc)) inc <- trimws(strsplit(inc, ",")[[1]])
    if (!is.null(exc)) exc <- trimws(strsplit(exc, ",")[[1]])
    if (pkg %in% loadedNamespaces()) {
      trace_namespace(pkg, inc, exc)
    } else {
      setHook(
        packageEvent(pkg, "onLoad"),
        function(...) trace_namespace(pkg, inc, exc)
      )
    }
  }
}
