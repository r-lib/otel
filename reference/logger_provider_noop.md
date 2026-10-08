# No-op logger provider

This is the logger provider
([otel_logger_provider](https://otel.r-lib.org/reference/otel_logger_provider.md))
otel uses when logging is disabled.

## Value

Not applicable.

## Details

All methods are no-ops or return objects that are also no-ops.

## See also

Other low level logs API:
[`get_default_logger_provider()`](https://otel.r-lib.org/reference/get_default_logger_provider.md),
[`get_logger()`](https://otel.r-lib.org/reference/get_logger.md),
[`otel_logger`](https://otel.r-lib.org/reference/otel_logger.md),
[`otel_logger_provider`](https://otel.r-lib.org/reference/otel_logger_provider.md)

## Examples

``` r
logger_provider_noop$new()
#> <otel_logger_provider_noop/otel_logger_provider>
#> methods:
#>   get_logger(name, minimum_severity, version, schema_url, attributes)
#>   flush()
```
