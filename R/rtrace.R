trace_namespace <- function(pkg) {
  msg("Instrumenting {.pkg {pkg}}.")
  ns <- asNamespace(pkg)
  trace_env(ns, name = pkg)
}

trace_env <- function(
    env, name = NULL, include_pattern = NULL, exclude_pattern = NULL) {
  nms <- ls(env)
  if (!is.null(include_pattern)) {
    nms <- grep(utils::glob2rx(include_pattern), nms, value = TRUE)
  }
  if (!is.null(exclude_pattern)) {
    nms <- nms[!grepl(utils::glob2rx(include_pattern), nms)]
  }
  for (nm in nms) {
    obj <- get(nm, envir = env)
    if (!is.function(obj)) next
    span_name <- paste0(name, "::", nm)
    tr1 <- substitute(
      .__span <- otel::start_span(sn, scope = NULL),
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
