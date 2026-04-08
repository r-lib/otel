# Pack the currently active span context into standard HTTP OpenTelemetry headers

The returned headers can be sent over HTTP, or set as environment
variables for subprocesses.

## Usage

``` r
pack_http_context()
```

## Value

A named character vector, with lowercase names. It might be an empty
vector, e.g. if tracing is disabled.

## See also

[`extract_http_context()`](https://otel.r-lib.org/dev/reference/extract_http_context.md)

## Examples

``` r
hdr <- otel::pack_http_context()
ctx <- otel::extract_http_context()
ctx$is_valid()
#> [1] FALSE
```
