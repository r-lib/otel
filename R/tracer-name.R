#' Default tracer name (and meter and logger name) for an R package
#'
#' Exporters, like the ones in the otelsdk package, can use this function
#' to determine the default tracer name, if the instrumentation author
#' hasn't specified one. If you are an instrumentation author, you probably
#' do not need to call this function directly, but do read on to learn
#' about choosing and setting the tracer name.
#'
#' ## About tracer names
#'
#' The name of a tracer identifies an OpenTelemetry instrumentation scope.
#' Instrumentation scopes can be used to organize the collected telemetry
#' data. otel can also use instrumentation scopes to suppress emitting
#' unneeded telemetry data, see '[Environment Variables]'.
#'
#' For the otel R package it makes sense to create a separate
#' instrumentation scope for each R package that emits telemetry data.
#' otel can do this automatically, with some from the package author.
#'
#' ## Setting the tracer name
#'
#' As a package author, you can define the `otel_tracer_name` symbol in
#' your package and set it do the desired tracer name. For example, the
#' callr package has this in an `.R` file:
#' ```
#' otel_tracer_name <- "org.r-lib.callr"
#' ```
#' See below for tips on choosing a tracer name.
#'
#' If you don't like the default tracer name, you can call [get_tracer()]
#' (or [get_logger()] or [get_meter()] manually with the desired name.
#'
#' ## Automatic tracer name detection in otel
#'
#' This is the detailed algorithm that otel uses in `default_tracer_name`:
#' - Using [base::topenv()] it finds the calling package (or other top
#'   level environment), recursively.
#' - It ignores the otel and otelsdk packages while searching.
#' - If it finds the base environment or the global environment, then
#'   it checks for the `otel_tracer_name` global variable (in the global
#'   environment). If that exists, then it must be a scalar string and it
#'   is used as the tracer name. Otherwise `org.project.R` is used as the
#'   tracer name.
#' - Otherwise it looks for the `otel_tracer_name` symbol inside the top
#'   level environment it has found. If this symbol exists then it must be
#'   a string scalar and otel will use it as the tracer name.
#' - If this symbol does not exist, then otel will use
#'   `r.package.<environment-name>` as the tracer name.
#'   `<environment-name>` is usually the package name.
#'
#' ## Choosing a tracer name
#'
#' The [OpenTelemetry specification](
#'   https://opentelemetry.io/docs/specs/otel/trace/api/#get-a-tracer
#' ) recommends using a tracer name that identifies the instrumentation
#' scope, i.e. your package.
#'
#' Some tips on choosing the tracer name:
#' - If your R package can be associated with a URL, you can use the
#'   "reverse" of that URL. E.g. since the callr package's online manual
#'   is at https://callr.r-lib.org, it can use `org.r-lib.callr`.
#' - If your R package belongs to your company, you can use the "reverse"
#'   of the company URL, possibly with an additional prefix. E.g. for the
#'   shiny R package by Posit, `co.posit.r-package.shiny` seems like a
#'   good name.
#' - If you don't set `otel_tracer_name`, then `default_tracer_name` will
#'   use `r.package.<package-name>` as the tracer name.
#'
#'
#' @param name Custom tracer name. If `NULL` then otel will construct a
#'   a tracer (meter, logger) name according to the algorithm detailed
#'   below.
#'
#' @return A list with entries:
#'
#'   * `name`: The supplied or auto-detected tracer name.
#'   * `package`: Auto-detected package name or `NA`.
#'   * `on`: Whether tracing is enabled for this package.
#'
#' @aliases otel_tracer_name
#' @export
#' @examples
#' default_tracer_name()

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
      nm <- get0("otel_tracer_name", .GlobalEnv, inherits = FALSE) %||%
        "org.r-project.R"
      ret <- list(name = nm, package = "R")
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
