# used by otelsdk
otel_cache_vars <- c(
  "tracer_provider",
  "logger_provider",
  "meter_provider",
  "tracer_app",
  "instruments"
)

# used by otelsdk
otel_clean_cache <- function() {
  for (nm in otel_cache_vars) {
    the[[nm]] <- NULL
  }
}

# used by otelsdk
otel_save_cache <- function() {
  copy <- new.env(parent = emptyenv())
  for (nm in otel_cache_vars) {
    copy[[nm]] <- the[[nm]]
  }
  copy
}

# used by otelsdk
otel_restore_cache <- function(copy) {
  for (nm in names(copy)) {
    the[[nm]] <- copy[[nm]]
  }
}

# nocov start
the <- new.env(parent = emptyenv())
the$mode <- "prod"

.onLoad <- function(libname, pkgname) {
  otel_clean_cache()
  setup_dev_env()
  setup_r_trace()
}
# nocov end

setup_dev_env <- function(envir = asNamespace(.packageName)) {
  ev <- tolower(Sys.getenv("OTEL_ENV"))
  if (ev %in% c("dev", "devel", "development")) {
    the$mode <- "dev"
    assign(
      "start_local_active_span",
      start_local_active_span_dev,
      envir = envir
    )
    assign(
      "start_span",
      start_span_dev,
      envir = envir
    )
    assign(
      "end_span",
      end_span_dev,
      envir = envir
    )
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
    assign("local_active_span", local_active_span_dev, envir = envir)
    assign("with_active_span", with_active_span_dev, envir = envir)
    assign(
      "get_active_span_context",
      get_active_span_context_dev,
      envir = envir
    )
    assign(
      "get_active_span",
      get_active_span_dev,
      envir = envir
    )
    assign(
      "extract_http_context",
      extract_http_context_dev,
      envir = envir
    )
    assign(
      "pack_http_context",
      pack_http_context_dev,
      envir = envir
    )
  }
}

setup_r_trace <- function() {
  ev <- trimws(Sys.getenv("OTEL_R_INSTRUMENT_PKGS", ""))
  if (ev == "") {
    return()
  }

  pkgs <- strsplit(ev, ",", fixed = TRUE)[[1]]
  for (pkg in pkgs) {
    PKG <- gsub(".", "_", toupper(pkg), fixed = TRUE)
    inc <- get_env(paste0("OTEL_R_INSTRUMENT_PKGS_", PKG, "_INCLUDE"))
    exc <- get_env(paste0("OTEL_R_INSTRUMENT_PKGS_", PKG, "_EXCLUDE"))
    if (!is.null(inc)) {
      inc <- trimws(strsplit(inc, ",")[[1]])
    }
    if (!is.null(exc)) {
      exc <- trimws(strsplit(exc, ",")[[1]])
    }
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
