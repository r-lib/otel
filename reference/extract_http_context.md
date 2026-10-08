# Extract a span context from HTTP headers received from a client

The return value can be used as the `parent` option when starting a
span.

## Usage

``` r
extract_http_context(headers)
```

## Arguments

- headers:

  A named list with one or two strings: `traceparent` is mandatory, and
  `tracestate` is optional.

## Value

And
[otel_span_context](https://otel.r-lib.org/reference/otel_span_context.md)
object.

## See also

[`pack_http_context()`](https://otel.r-lib.org/reference/pack_http_context.md)

## Examples

``` r
hdr <- otel::pack_http_context()
ctx <- otel::extract_http_context()
ctx$is_valid()
#> [1] FALSE
```
