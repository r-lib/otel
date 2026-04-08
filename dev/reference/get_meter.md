# Get a meter from the default meter provider

Get a meter from the default meter provider

## Usage

``` r
get_meter(
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

  Name of the new tracer. If missing, then deduced automatically.

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

  Additional arguments are passed to the `get_meter()` method of the
  provider.

- provider:

  Meter provider to use. If `NULL`, then it uses
  [`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md)
  to get a tracer provider.

## Value

An [otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md)
object.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
myfun <- function() {
  mtr <- otel::get_meter()
  ctr <- mtr$create_counter("session-count")
  ctr$add(1)
}
myfun()
```
