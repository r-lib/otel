# End an OpenTelemetry span

Spans created with
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
end automatically by default. You must end every other span manually, by
calling `end_span`, or using the `end_on_exit` argument of
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/dev/reference/with_active_span.md).

## Usage

``` r
end_span(span, status_code = NULL)
```

## Arguments

- span:

  The span to end.

- status_code:

  Span status code to set before ending the span. Possible values:
  unset, ok, error. See
  [span_status_codes](https://otel.r-lib.org/dev/reference/tracing-constants.md).
  If `NULL`, the status is not changed.

## Value

Nothing.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/dev/reference/zci.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/dev/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/dev/reference/tracing-constants.md),
[`with_active_span()`](https://otel.r-lib.org/dev/reference/with_active_span.md)

## Examples

``` r
fun <- function() {
  # start span, do not activate
  spn <- otel::start_span("myfun")
  # do not leak resources
  on.exit(otel::end_span(spn), add = TRUE)
  myfun <- function() {
     # activate span for this function
     otel::local_active_span(spn)
     # create child span
     spn2 <- otel::start_local_active_span("myfun/2")
  }

  myfun2 <- function() {
    # activate span for this function
    otel::local_active_span(spn)
    # create child span
    spn3 <- otel::start_local_active_span("myfun/3")
  }
  myfun()
  myfun2()
  end_span(spn)
}
fun()
```
