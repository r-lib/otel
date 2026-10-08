# Pack the currently active span context into standard HTTP OpenTelemetry headers

The returned headers can be sent over HTTP, or set as environment
variables for subprocesses.

## Usage

``` r
pack_http_context(tracer = NULL)
```

## Arguments

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
