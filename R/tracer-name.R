#' Default tracer (and meter and logger) name for an R package
#'
#' @param name Custom tracer name. If `NULL` then otel will construct a
#'   a tracer (meter, logger) name according to the algorithm detailed
#'   below.
#'
#' TODO
#'
#' @aliases otel_tracer_name
#' @export

default_tracer_name <- function(name = NULL) {
  if (!is.null(name)) {
    ret <- list(name = name, package = NA_character_)
    ret[["on"]] <- is_scope_on(ret)
    return(ret)
  }

  for (n in 1:sys.nframe()) {
    top <- topenv(parent.frame(n), NULL)
    topname <- environmentName(top)
    if (topname == "" || topname == "otel" || topname == "otelsdk") {
      # keep going
    } else if (topname == "base" || topname == "R_GlobalEnv") {
      ret <- list(name = "org.r-project.R", package = "R")
      ret[["on"]] <- is_scope_on(ret)
      return(ret)
    } else {
      nm <- get0("otel_tracer_name", top, inherits = FALSE) %||%
        paste0("r.package.", topname)
      ret <- list(name = nm, package = topname)
      ret[["on"]] <- is_scope_on(ret)
      return(ret)
    }
  }
  ret <- list(name = "org.r-project.R", package = "R")
  ret[["on"]] <- is_scope_on(ret)
  ret
}

otel_emit_scopes_envvar <- "OTEL_R_EMIT_SCOPES"
otel_suppress_scopes_envvar <- "OTEL_R_SUPPRESS_SCOPES"
is_scope_on <- function(scope) {
  inc <- get_env(otel_emit_scopes_envvar)
  exc <- get_env(otel_suppress_scopes_envvar)
  # shortcut for most common case
  if (is.null(inc) && is.null(exc)) {
    return(TRUE)
  }

  if (!is.null(inc)) {
    inc <- trimws(strsplit(inc, ",")[[1]])
  }
  if (!is.null(exc)) {
    exc <- trimws(strsplit(exc, ",")[[1]])
  }

  flt <- glob_filter(c(scope$name, scope$package), inc, exc)
  length(flt) > 0
}
