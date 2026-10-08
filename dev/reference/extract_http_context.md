# Extract a span context from HTTP headers received from a client

The return value can be used as the `parent` option when starting a
span.

## Usage

``` r
extract_http_context(headers, tracer = NULL)
```

## Arguments

- headers:

  A named list with one or two strings: `traceparent` is mandatory, and
  `tracestate` is optional.

- tracer:

  Tracer object
  ([otel_tracer](https://otel.r-lib.org/dev/reference/otel_tracer.md))
  or tracer name to use. If `NULL`, then otel uses an internal tracer.
  You usually do not need to set this, the active span does not depend
  on the tracer.

  Passing a tracer might give the wrong result. If it is a no-op tracer,
  then the result is an invalid span (context), or no HTTP headers, even
  if there is an active span. This happens if the tracer's scope is
  turned off, e.g. via the `OTEL_R_SUPPRESS_SCOPES` environment
  variable, or if the tracer was created before tracing was turned on.
  The default `NULL` does not have this problem.

## Value

And
[otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)
object.

## See also

[`pack_http_context()`](https://otel.r-lib.org/dev/reference/pack_http_context.md)

## Examples

``` r
hdr <- otel::pack_http_context()
ctx <- otel::extract_http_context()
ctx$is_valid()
#> [1] FALSE
```
