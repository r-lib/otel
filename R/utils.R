`%||%` <- function(l, r) if (is.null(l)) r else l

errmsg <- function(..., class = "otel_error_message") {
  cnd <- structure(
    list(message = paste0(c(..., "\n"), collapse = "")),
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

glob_filter <- function(x, include = NULL, exclude = NULL) {
  if (is.null(include)) {
    res <- x
  } else {
    res <- character()
    for (pat in include) {
      res <- c(res, grep(utils::glob2rx(pat), x, value = TRUE))
    }
    res <- unique(res)
  }
  for (pat in exclude) {
    res <- setdiff(
      res,
      grep(utils::glob2rx(pat), res, value = TRUE)
    )
  }
  res
}

get_env_count <- function(var, default) {
  strval <- Sys.getenv(var)
  if (tolower(strval) == "inf") {
    return(Inf)
  }
  intval <- suppressWarnings(as.integer(strval))
  if (!is.na(intval) && intval >= 0) {
    return(intval)
  }
  if (default == Inf) {
    return(Inf)
  }
  intval <- suppressWarnings(as.integer(default))
  if (!is.na(intval) && intval >= 0) {
    return(intval)
  }
  stop(
    "Invalid `default` in `get_env_count()`, must be a non-negative ",
    "integer scalar."
  )
}

map_lgl <- function(.x, .f, ...) {
  vapply(.x, .f, logical(1), ...)
}

map_chr <- function(.x, .f, ...) {
  vapply(.x, .f, character(1), ...)
}

mkdirp <- function(dir) {
  s <- map_lgl(dir, dir.create, recursive = TRUE, showWarnings = FALSE)
  invisible(s)
}
