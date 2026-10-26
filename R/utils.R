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

# Name of the function that is running in the `scope` frame, to use as
# the default span name. Returns `NULL` if there is no such frame, e.g.
# at the top level, or for anonymous functions. `qualified` has the
# package name as well, if the function is in a package namespace.
caller_function_name <- function(scope) {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    if (!identical(frames[[i]], scope)) {
      next
    }
    # code evaluated with eval(), e.g. via local() or source(), has an
    # eval() frame, but not a function of its own
    if (is.primitive(sys.function(i))) {
      return(NULL)
    }
    fun <- sys.call(i)[[1]]
    if (is.symbol(fun)) {
      name <- as.character(fun)
      top <- topenv(environment(sys.function(i)))
      qualified <- if (isNamespace(top)) {
        paste0(getNamespaceName(top), "::", name)
      } else {
        name
      }
      return(list(name = name, qualified = qualified))
    }
    if (is.call(fun) && as.character(fun[[1]]) %in% c("::", ":::", "$", "@")) {
      name <- paste(deparse(fun), collapse = "")
      return(list(name = name, qualified = name))
    }
    return(NULL)
  }
  NULL
}

# Default span name, from the name of the function running in `scope`.
# Also sets the `code.function.name` attribute, unless the caller set it.
# Callers only call this for real tracers, so it costs nothing if tracing
# is off.
default_span_name <- function(attributes, scope) {
  fn <- caller_function_name(scope)
  if (is.null(fn)) {
    return(list(name = NULL, attributes = attributes))
  }
  attributes[["code.function.name"]] <- attributes[["code.function.name"]] %||%
    fn$qualified
  list(name = fn$name, attributes = attributes)
}
