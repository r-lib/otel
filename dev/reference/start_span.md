# Start an OpenTelemetry span.

Creates a new OpenTelemetry span and starts it, without activating it.

Usually you want
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
instead of `start_span`.
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
also activates the span for the caller frame, and ends the span when the
caller frame exits.

## Usage

``` r
start_span(
  name = NULL,
  attributes = NULL,
  links = NULL,
  options = NULL,
  ...,
  tracer = NULL
)
```

## Arguments

- name:

  Name of the span. If not specified it will be `"<NA>"`.

- attributes:

  Span attributes. OpenTelemetry supports the following R types as
  attributes: \`character, logical, double, integer. You may use
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  to convert other R types to OpenTelemetry attributes.

- links:

  A named list of links to other spans. Every link must be an
  OpenTelemetry span
  ([otel_span](https://otel.r-lib.org/dev/reference/otel_span.md))
  object, or a list with a span object as the first element and named
  span attributes as the rest.

- options:

  A named list of span options. May include:

  - `start_system_time`: Start time in system time.

  - `start_steady_time`: Start time using a steady clock.

  - `parent`: A parent span or span context. If it is `NA`, then the
    span has no parent and it will be a root span. If it is `NULL`, then
    the current context is used, i.e. the active span, if any.

  - `kind`: Span kind, one of
    [span_kinds](https://otel.r-lib.org/dev/reference/tracing-constants.md):
    "internal", "server", "client", "producer", "consumer".

- ...:

  Additional arguments are passed to the `start_span()` method of the
  tracer.

- tracer:

  A tracer object or the name of the tracer to use, see
  [`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md).
  If `NULL` then
  [`default_tracer_name()`](https://otel.r-lib.org/dev/reference/default_tracer_name.md)
  is used.

## Value

An OpenTelemetry span
([otel_span](https://otel.r-lib.org/dev/reference/otel_span.md)).

## Details

Only use `start_span()` is you need to manage the span's activation
manually. Otherwise use
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md).

You must end the span by calling
[`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md).
Alternatively you can also end it with
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/dev/reference/with_active_span.md)
by setting `end_on_exit = TRUE`.

It is a good idea to end spans created with `start_span()` in an
[`base::on.exit()`](https://rdrr.io/r/base/on.exit.html) call.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/dev/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/dev/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/dev/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md),
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
