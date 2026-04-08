# Get the default meter provider

The meter provider defines how metrics are exported when collecting
telemetry data. It is unlikely that you need to call this function
directly, but read on to learn how to configure which exporter to use.

## Usage

``` r
get_default_meter_provider()
```

## Value

The default meter provider, an
[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)
object.

## Details

If there is no default set currently, then it creates and sets a
default.

The default meter provider is created based on the
OTEL_R_METRICS_EXPORTER environment variable. This environment variable
is specifically for R applications with OpenTelemetry support.

If this is not set, then the generic OTEL_METRICS_EXPORTER environment
variable is used. This applies to all applications that support
OpenTelemetry and use the OpenTelemetry SDK.

The following values are allowed:

- `none`: no metrics are exported.

- `stdout` or `console`: uses
  [otelsdk::meter_provider_stdstream](https://otelsdk.r-lib.org/reference/meter_provider_stdstream.html),
  to write metrics to the standard output.

- `stderr`: uses
  [otelsdk::meter_provider_stdstream](https://otelsdk.r-lib.org/reference/meter_provider_stdstream.html),
  to write metrics to the standard error.

- `http` or `otlp`: uses
  [otelsdk::meter_provider_http](https://otelsdk.r-lib.org/reference/meter_provider_http.html),
  to send metrics through HTTP, using the OpenTelemetry Protocol (OTLP).

- `otlp/file` uses
  [otelsdk::meter_provider_file](https://otelsdk.r-lib.org/reference/meter_provider_file.html)
  to write metrics to a JSONL file.

- `<package>::<provider>`: will select the `<provider>` object from the
  `<package>` package to use as a meter provider. It calls
  `<package>::<provider>$new()` to create the new meter provider. If
  this fails for some reason, e.g. the package is not installed, then it
  throws an error.

## See also

Other low level metrics API:
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
get_default_meter_provider()
#> <otel_meter_provider_noop/otel_meter_provider>
#> methods:
#>   get_meter(name, version, schema_url, attributes)
#>   flush(timeout)
#>   shutdown(timeout)
#>   get_metrics()
```
