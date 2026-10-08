# OpenTelemetry meter provider objects

otel_meter_provider -\>
[otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md) -\>
[otel_counter](https://otel.r-lib.org/dev/reference/otel_counter.md),
[otel_up_down_counter](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md),
[otel_histogram](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)

## Value

Not applicable.

## Details

The meter provider defines how metrics are exported when collecting
telemetry data. It is unlikely that you need to use meter provider
objects directly.

Usually there is a single meter provider for an R app or script.

Typically the meter provider is created automatically, at the first
[`counter_add()`](https://otel.r-lib.org/dev/reference/counter_add.md),
[`up_down_counter_add()`](https://otel.r-lib.org/dev/reference/up_down_counter_add.md),
[`histogram_record()`](https://otel.r-lib.org/dev/reference/histogram_record.md),
[`gauge_record()`](https://otel.r-lib.org/dev/reference/gauge_record.md)
or [`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md)
call. otel decides which meter provider class to use based on
'[Environment
Variables](https://otel.r-lib.org/dev/reference/environmentvariables.md)'.

## Implementations

Note that this list is updated manually and may be incomplete.

- [meter_provider_noop](https://otel.r-lib.org/dev/reference/meter_provider_noop.md):
  No-op meter provider, used when no metrics are emitted.

- [otelsdk::meter_provider_file](https://otelsdk.r-lib.org/reference/meter_provider_file.html):
  Save metrics to a JSONL file.

- [otelsdk::meter_provider_http](https://otelsdk.r-lib.org/reference/meter_provider_http.html):
  Send metrics to a collector over HTTP/OTLP.

- [otelsdk::meter_provider_memory](https://otelsdk.r-lib.org/reference/meter_provider_memory.html):
  Collect emitted metrics in memory. For testing.

- [otelsdk::meter_provider_stdstream](https://otelsdk.r-lib.org/reference/meter_provider_stdstream.html):
  Write metrics to standard output or error or to a file. Primarily for
  debugging.

## Methods

### `meter_provider$get_meter()`

Get or create a new meter object.

#### Usage

    meter_provider$get_meter(
      name = NULL,
      version = NULL,
      schema_url = NULL,
      attributes = NULL
    )

#### Arguments

- `name`: Meter name, see
  [`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md).

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

Returns an OpenTelemetry meter
([otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md))
object.

#### See also

[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md).

### `meter_provider$flush()`

Force any buffered metrics to flush. Meter providers might not implement
this method.

#### Usage

    meter_provider$flush()

#### Value

Nothing.

### `meter_provider$shutdown()`

Stop the meter provider. Stops collecting and emitting measurements.

#### Usage

    meter_provider$shutdown()

#### Value

Nothing

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
mp <- otel::get_default_meter_provider()
mtr <- mp$get_meter()
mtr$is_enabled()
#> [1] FALSE
```
