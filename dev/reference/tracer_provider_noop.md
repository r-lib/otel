# No-op tracer provider

This is the tracer provider
([otel_tracer_provider](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md))
otel uses when tracing is disabled.

## Value

Not applicable.

## Details

All methods are no-ops or return objects that are also no-ops.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md),
[`otel_span`](https://otel.r-lib.org/dev/reference/otel_span.md),
[`otel_span_context`](https://otel.r-lib.org/dev/reference/otel_span_context.md),
[`otel_tracer`](https://otel.r-lib.org/dev/reference/otel_tracer.md),
[`otel_tracer_provider`](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md)

## Examples

``` r
tracer_provider_noop$new()
#> <otel_tracer_provider_noop/otel_tracer_provider>
#> methods:
#>   get_tracer(name, version, schema_url, attributes)
#>   flush()
#>   get_spans()
```
