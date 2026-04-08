# OpenTelemetry Tracer Object

[otel_tracer_provider](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md)
-\> otel_tracer -\>
[otel_span](https://otel.r-lib.org/dev/reference/otel_span.md) -\>
[otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_tracer objects directly.
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
(and
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md))
automatically sets up the tracer and uses it to create spans.

A tracer object is created by calling the
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md)
method of an
[otel_tracer_provider](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md).

You can use the
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md)
method of the tracer object to create a span.

Typically there is a separate tracer object for each instrumented R
package.

## Methods

### `tracer$start_span()`

Creates and starts a new span.

It does not activate the new span.

It is equivalent to the
[`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md)
function.

#### Usage

    tracer_start_span(
      name = NULL,
      attributes = NULL,
      links = NULL,
      options = NULL
    )

#### Arguments

- `name`: Name of the span. If not specified it will be `"<NA>"`.

- `attributes`: Span attributes. OpenTelemetry supports the following R
  types as attributes: \`character, logical, double, integer. You may
  use
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  to convert other R types to OpenTelemetry attributes.

- `links`: A named list of links to other spans. Every link must be an
  OpenTelemetry span
  ([otel_span](https://otel.r-lib.org/dev/reference/otel_span.md))
  object, or a list with a span object as the first element and named
  span attributes as the rest.

- `options`: A named list of span options. May include:

  - `start_system_time`: Start time in system time.

  - `start_steady_time`: Start time using a steady clock.

  - `parent`: A parent span or span context. If it is `NA`, then the
    span has no parent and it will be a root span. If it is `NULL`, then
    the current context is used, i.e. the active span, if any.

  - `kind`: Span kind, one of
    [span_kinds](https://otel.r-lib.org/dev/reference/tracing-constants.md):
    "internal", "server", "client", "producer", "consumer".

#### Value

A new [otel_span](https://otel.r-lib.org/dev/reference/otel_span.md)
object.

### `tracer$is_enabled()`

Whether the tracer is active and recording traces.

This is equivalent to the
[`is_tracing_enabled()`](https://otel.r-lib.org/dev/reference/is_tracing_enabled.md)
function.

#### Usage

    tracer$is_enabled()

#### Value

Logical scalar.

### `tracer$flush()`

Flush the tracer provider: force any buffered spans to flush. Tracer
providers might not implement this method.

#### Usage

    tracer$flush()

#### Value

Nothing.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md),
[`otel_span`](https://otel.r-lib.org/dev/reference/otel_span.md),
[`otel_span_context`](https://otel.r-lib.org/dev/reference/otel_span_context.md),
[`otel_tracer_provider`](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md),
[`tracer_provider_noop`](https://otel.r-lib.org/dev/reference/tracer_provider_noop.md)

## Examples

``` r
tp <- get_default_tracer_provider()
trc <- tp$get_tracer()
trc$is_enabled()
#> [1] FALSE
```
