# Start and activate a span

Creates, starts and activates an OpenTelemetry span.

Usually you want this functions instead of
[`start_span()`](https://otel.r-lib.org/reference/start_span.md), which
does not activate the new span.

## Usage

``` r
start_local_active_span(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL,
  activation_scope = parent.frame(),
  end_on_exit = TRUE
)
```

## Arguments

- name:

  Name of the span. If not specified it will be `"<NA>"`.

- attributes:

  Span attributes. OpenTelemetry supports the following R types as
  attributes: \`character, logical, double, integer. You may use
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  to convert other R types to OpenTelemetry attributes.

- links:

  A named list of links to other spans. Every link must be an
  OpenTelemetry span
  ([otel_span](https://otel.r-lib.org/reference/otel_span.md)) object,
  or a list with a span object as the first element and named span
  attributes as the rest.

- options:

  A named list of span options. May include:

  - `start_system_time`: Start time in system time.

  - `start_steady_time`: Start time using a steady clock.

  - `parent`: A parent span or span context. If it is `NA`, then the
    span has no parent and it will be a root span. If it is `NULL`, then
    the current context is used, i.e. the active span, if any.

  - `kind`: Span kind, one of
    [span_kinds](https://otel.r-lib.org/reference/tracing-constants.md):
    "internal", "server", "client", "producer", "consumer".

- ...:

  Additional arguments are passed to the
  [`start_span()`](https://otel.r-lib.org/reference/start_span.md)
  method of the tracer.

- tracer:

  A tracer object or the name of the tracer to use, see
  [`get_tracer()`](https://otel.r-lib.org/reference/get_tracer.md). If
  `NULL` then
  [`default_tracer_name()`](https://otel.r-lib.org/reference/default_tracer_name.md)
  is used.

- activation_scope:

  The R scope to activate the span for. Defaults to the caller frame.

- end_on_exit:

  Whether to also end the span when the activation scope exits.

## Value

The new OpenTelemetry span object (of class
[otel_span](https://otel.r-lib.org/reference/otel_span.md)), invisibly.
See [otel_span](https://otel.r-lib.org/reference/otel_span.md) for
information about the returned object.

## Details

If `end_on_exit` is `TRUE` (the default), then it also ends the span
when the activation scope finishes.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md),
[`start_span()`](https://otel.r-lib.org/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/reference/tracing-constants.md),
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md)

## Examples

``` r
fn1 <- function() {
  otel::start_local_active_span("fn1")
  fn2()
}
fn2 <- function() {
  otel::start_local_active_span("fn2")
}
fn1()
```
