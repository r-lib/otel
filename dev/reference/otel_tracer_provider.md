# OpenTelemetry Tracer Provider Object

otel_tracer_provider -\>
[otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md) -\>
[otel_span](https://otel.r-lib.org/dev/reference/otel_span.md) -\>
[otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)

## Value

Not applicable.

## Details

The tracer provider defines how traces are exported when collecting
telemetry data. It is unlikely that you'd need to use tracer provider
objects directly.

Usually there is a single tracer provider for an R app or script.

Typically the tracer provider is created automatically, at the first
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
or [`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md)
call. otel decides which tracer provider class to use based on
[Environment
Variables](https://otel.r-lib.org/dev/reference/environmentvariables.md).

## Implementations

Note that this list is updated manually and may be incomplete.

- [tracer_provider_noop](https://otel.r-lib.org/dev/reference/tracer_provider_noop.md):
  No-op tracer provider, used when no traces are emitted.

- [otelsdk::tracer_provider_file](https://otelsdk.r-lib.org/reference/tracer_provider_file.html):
  Save traces to a JSONL file.

- [otelsdk::tracer_provider_http](https://otelsdk.r-lib.org/reference/tracer_provider_http.html):
  Send traces to a collector over HTTP/OTLP.

- [otelsdk::tracer_provider_memory](https://otelsdk.r-lib.org/reference/tracer_provider_memory.html):
  Collect emitted traces in memory. For testing.

- [otelsdk::tracer_provider_stdstream](https://otelsdk.r-lib.org/reference/tracer_provider_stdstream.html):
  Write traces to standard output or error or to a file. Primarily for
  debugging.

## Methods

### `tracer_provider$get_tracer()`

Get or create a new tracer object.

#### Usage

    tracer_provider$get_tracer(
      name = NULL,
      version = NULL,
      schema_url = NULL,
      attributes = NULL
    )

#### Arguments

- `name`: Tracer name, see
  [`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md).

- `version`: Optional. Specifies the version of the instrumentation
  scope if the scope has a version (e.g. R package version). Example
  value: `"1.0.0"`.

- `schema_url`: Optional. Specifies the Schema URL that should be
  recorded in the emitted telemetry.

- `attributes`: Optional. Specifies the instrumentation scope attributes
  to associate with emitted telemetry. See
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  for allowed values. You can also use
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  to convert R objects to OpenTelemetry attributes.

#### Value

Returns an OpenTelemetry tracer
([otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md))
object.

#### See also

[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md).

### `tracer_provider$flush()`

Force any buffered spans to flush. Tracer providers might not implement
this method.

#### Usage

    tracer_provider$flush()

#### Value

Nothing.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md),
[`otel_span`](https://otel.r-lib.org/dev/reference/otel_span.md),
[`otel_span_context`](https://otel.r-lib.org/dev/reference/otel_span_context.md),
[`otel_tracer`](https://otel.r-lib.org/dev/reference/otel_tracer.md),
[`tracer_provider_noop`](https://otel.r-lib.org/dev/reference/tracer_provider_noop.md)

## Examples

``` r
tp <- otel::get_default_tracer_provider()
trc <- tp$get_tracer()
trc$is_enabled()
#> [1] FALSE
```
