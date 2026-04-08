# Check if tracing is active

Checks whether OpenTelemetry tracing is active. This can be useful to
avoid unnecessary computation when tracing is inactive.

## Usage

``` r
is_tracing_enabled(tracer = NULL)
```

## Arguments

- tracer:

  Tracer object
  ([otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md)).
  It can also be a tracer name, the instrumentation scope, or `NULL` for
  determining the tracer name automatically. Passed to
  [`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md)
  if not a tracer object.

## Value

`TRUE` is OpenTelemetry tracing is active, `FALSE` otherwise.

## Details

It calls
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md)
with `name` and then it calls the tracer's `$is_enabled()` method.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/dev/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md),
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/dev/reference/tracing-constants.md),
[`with_active_span()`](https://otel.r-lib.org/dev/reference/with_active_span.md)

## Examples

``` r
fun <- function() {
  if (otel::is_tracing_enabled()) {
    xattr <- calculate_some_extra_attributes()
    otel::start_local_active_span("fun", attributes = xattr)
  }
  # ...
}
```
