# Evaluate R code with an active OpenTelemetry span

Activates the span for evaluating an R expression.

Usually you need this function for spans created with
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
which does not activate the new span. Usually you don't need it for
spans created with
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
because it activates the new span automatically.

## Usage

``` r
with_active_span(span, expr, end_on_exit = FALSE)
```

## Arguments

- span:

  The OpenTelemetry span to activate.

- expr:

  R expression to evaluate.

- end_on_exit:

  Whether to end after evaluating the R expression.

## Value

The return value of `expr`.

## Details

After `expr` is evaluated (or an error occurs), the span is deactivated
and the previously active span will be active again, if there was any.

It is possible to activate the same span for multiple R frames.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/dev/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/dev/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/dev/reference/tracing-constants.md)

Other tracing for concurrent code:
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md)

## Examples

``` r
fun <- function() {
  # start span, do not activate
  spn <- otel::start_span("myfun")
  # do not leak resources
  on.exit(otel::end_span(spn), add = TRUE)
  myfun <- function() {
     otel::with_active_span(spn, {
       # create child span
       spn2 <- otel::start_local_active_span("myfun/2")
     })
  }

  myfun2 <- function() {
    otel::with_active_span(spn, {
      # create child span
      spn3 <- otel::start_local_active_span("myfun/3")
    })
  }
  myfun()
  myfun2()
  end_span(spn)
}
fun()
```
