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
  for (nm in nms) {
    obj <- get(nm, envir = env)
    if (!is.function(obj)) {
      next
    }
    span_name <- paste0(name, "::", nm)
    tr1 <- substitute(
      .__span <- otel::get_tracer("org.r-lib.otel")$start_as_active_span(
        sn,
        tracer_name = "org.r-lib.otel",
        scope = NULL
      ),
      list(sn = span_name)
    )
    suppressMessages(trace(
      nm,
      tr1,
      exit = quote(try(.__span$end())),
      print = FALSE,
      where = env
    ))
  }
}
