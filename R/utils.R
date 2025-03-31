`%||%` <- function(l, r) if (is.null(l)) r else l

msg <- function(..., class = "otel_error_message") {
  cnd <- structure(
    list(message = paste0(..., collapse = "")),
    class = c(class, "message", "condition")
  )
  message(cnd)
}

get_env <- function(n) {
  v <- Sys.getenv(n)
  if (v != "") v else NULL
}
