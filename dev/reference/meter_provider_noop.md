# No-op Meter Provider

This is the meter provider
([otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md))
otel uses when metrics collection is disabled.

## Value

Not applicable.

## Details

All methods are no-ops or return objects that are also no-ops.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
meter_provider_noop$new()
#> <otel_meter_provider_noop/otel_meter_provider>
#> methods:
#>   get_meter(name, version, schema_url, attributes)
#>   flush(timeout)
#>   shutdown(timeout)
#>   get_metrics()
```
