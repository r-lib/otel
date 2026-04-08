# Activate an OpenTelemetry span for an R scope

Activates the span for the caller (or other) frame.

Usually you need this function for spans created with
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
which does not activate the new span. Usually you don't need it for
spans created with
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
because it activates the new span automatically.

## Usage

``` r
local_active_span(span, end_on_exit = FALSE, activation_scope = parent.frame())
```

## Arguments

- span:

  The OpenTelemetry span to activate.

- end_on_exit:

  Whether to end the span when exiting the activation scope.

- activation_scope:

  The scope to activate the span for, defaults to the caller frame.

## Value

Nothing.

## Details

When the frame ends, the span is deactivated and the previously active
span will be active again, if there was any.

It is possible to activate the same span for multiple R frames.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/dev/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/dev/reference/is_tracing_enabled.md),
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/dev/reference/tracing-constants.md),
[`with_active_span()`](https://otel.r-lib.org/dev/reference/with_active_span.md)

Other tracing for concurrent code:
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
