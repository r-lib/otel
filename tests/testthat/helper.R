test_pkg_root <- function() {
  pkg <- test_path("../../")
  if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
    pkg <- file.path(pkg, "00_pkg_src", .packageName)
  }
  pkg
}

local_otel_cache <- function(.local_envir = parent.frame()) {
  otel_clean_cache()
  withr::defer(otel_clean_cache(), envir = .local_envir)
}

local_otel_off <- function(.local_envir = parent.frame()) {
  local_otel_cache(.local_envir = .local_envir)
  withr::local_envvar(
    structure(
      rep("none", 3),
      names = c(
        default_traces_exporter_envvar_r,
        default_logs_exporter_envvar_r,
        default_metrics_exporter_envvar_r
      )
    ),
    .local_envir = .local_envir
  )
}

transform_env_address <- function(x) {
  x <- sub("<environment: 0x[0-9a-f]+>", "<environment: 0x<address>>", x)
  x
}
