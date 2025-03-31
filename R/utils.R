`%||%` <- function(l, r) if (is.null(l)) r else l

errmsg <- function(..., class = "otel_error_message") {
  cnd <- structure(
    list(message = paste0(..., collapse = "")),
    class = c(class, "message", "condition")
  )
  message(cnd)
}

msg <- function(message, .envir = parent.frame()) {
  if ("cli" %in% loadedNamespaces()) {
    cli::cli_alert_info(message, .envir = .envir)
  }
}

get_env <- function(n) {
  v <- Sys.getenv(n)
  if (v != "") v else NULL
}
