trace_namespace <- function(pkg, include = NULL, exclude = NULL) {
  msg("Instrumenting {.pkg {pkg}}.")
  ns <- asNamespace(pkg)
  trace_env(ns, name = pkg, include = include, exclude = exclude)
}

trace_env <- function(
  env,
  name = NULL,
  include = NULL,
  exclude = NULL
) {
  nms <- glob_filter(ls(env), include, exclude)
  lapply(nms, function(nm) {
    obj <- get(nm, envir = env)
    if (!is.function(obj)) {
      return()
    }
    span_name <- paste0(name, "::", nm)
    suppressMessages(trace(
      nm,
      tracer = substitute(
        {
          # nocov start
          .__span <- otel::start_span(sn, tracer = "org.r-lib.otel")
          .__scope <- .__span$activate(NULL)
          # nocov end
        },
        list(sn = span_name)
      ),
      exit = quote({
        # nocov start
        try(.__span$deactivate(.__scope))
        try(.__span$end())
        # nocov end
      }),
      print = FALSE,
      where = env
    ))
    NULL
  })
  invisible()
}
