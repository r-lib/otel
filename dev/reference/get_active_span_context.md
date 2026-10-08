# Returns the active span context

This is sometimes useful for logs or metrics, to associate logging and
metrics reporting with traces.

## Usage

``` r
get_active_span_context(tracer = NULL)
```

## Arguments

- tracer:

  Tracer object
  ([otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md))
  or tracer name to use. If `NULL`, then otel uses an internal tracer.
  You usually do not need to set this, the active span does not depend
  on the tracer.

  Passing a tracer might give the wrong result. If it is a no-op tracer,
  then the result is an invalid span (context), or no HTTP headers, even
  if there is an active span. This happens if the tracer's scope is
  turned off, e.g. via the `OTEL_R_SUPPRESS_SCOPES` environment
  variable, or if the tracer was created before tracing was turned on.
  The default `NULL` does not have this problem.

## Value

The active span context, an
[otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)
object. If there is no active span context, then an invalid span context
is returned, i.e. `spc$is_valid()` will be `FALSE` for the returned
`spc`.

## Details

Note that logs and metrics instruments automatically use the current
span context, so often you don't need to call this function explicitly.

## Examples

``` r
fun <- function() {
  otel::start_local_active_span("fun")
  fun2()
}
fun2 <- function() {
  otel::log("Log message", span_context = otel::get_active_span_context())
}
fun()
```
