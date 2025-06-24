#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Default tracer, meter, logger name for an R package
#'
#' The otel R package uses the `otel_tracer_name` variable from your
#' package when it deducts the tracer (and meter and logger) name to use
#' if [start_span()] (etc.) is called from your package.
#'
#' We suggest that you set this internal variable in your package to avoid
#' having to specify the tracer name for every [start_span()] call.
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
#' - If you don't set `otel_tracer_name`, then otel will use
#'   `r.package.<package-name>` as the tracer name.
#'
#' @name otel_tracer_name
NULL
