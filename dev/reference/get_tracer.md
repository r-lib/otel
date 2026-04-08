# Get a tracer from the default tracer provider

Calls
[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md)
to get the default tracer provider. Then calls its `$get_tracer()`
method to create a new tracer.

## Usage

``` r
get_tracer(
  name = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
)
```

## Arguments

- name:

  Name of the new tracer. If missing, then deduced automatically using
  [`default_tracer_name()`](https://otel.r-lib.org/dev/reference/default_tracer_name.md).
  Make sure you read the manual page of
  [`default_tracer_name()`](https://otel.r-lib.org/dev/reference/default_tracer_name.md)
  before using this argument.

- version:

  Optional. Specifies the version of the instrumentation scope if the
  scope has a version (e.g. R package version). Example value:
  `"1.0.0"`.

- schema_url:

  Optional. Specifies the Schema URL that should be recorded in the
  emitted telemetry.

- attributes:

  Optional. Specifies the instrumentation scope attributes to associate
  with emitted telemetry.

- ...:

  Additional arguments are passed to the `get_tracer()` method of the
  provider.

- provider:

  Tracer provider to use. If `NULL`, then it uses
  [`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md)
  to get a tracer provider.

## Value

An OpenTelemetry tracer, an
[otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md)
object.

## Details

Usually you do not need to call this function directly, because
[`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
calls it for you.

Calling `get_tracer()` multiple times with the same `name` (or same
auto-deduced name) will return the same (internal) tracer object. (Even
if the R external pointer objects representing them are different.)

A tracer is only deleted if its tracer provider is deleted and garbage
collected.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md),
[`otel_span`](https://otel.r-lib.org/dev/reference/otel_span.md),
[`otel_span_context`](https://otel.r-lib.org/dev/reference/otel_span_context.md),
[`otel_tracer`](https://otel.r-lib.org/dev/reference/otel_tracer.md),
[`otel_tracer_provider`](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md),
[`tracer_provider_noop`](https://otel.r-lib.org/dev/reference/tracer_provider_noop.md)

## Examples

``` r
myfun <- function() {
  trc <- otel::get_tracer()
  spn <- trc$start_span()
  on.exit(otel::end_span(spn), add = TRUE)
  otel::local_active_span(spn, end_on_exit = TRUE)
}
myfun()
```
