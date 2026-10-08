# Changelog

## otel (development version)

- [`get_active_span()`](https://otel.r-lib.org/dev/reference/get_active_span.md),
  [`get_active_span_context()`](https://otel.r-lib.org/dev/reference/get_active_span_context.md),
  [`pack_http_context()`](https://otel.r-lib.org/dev/reference/pack_http_context.md)
  and
  [`extract_http_context()`](https://otel.r-lib.org/dev/reference/extract_http_context.md)
  are now faster: they use a cached internal tracer, instead of looking
  up the tracer name from the call stack. They also have a new `tracer`
  argument, to use a specific tracer
  ([\#34](https://github.com/r-lib/otel/issues/34)).

## otel 0.2.0

CRAN release: 2025-08-29

- Zero Code Instrumentation (ZCI) works again
  ([\#20](https://github.com/r-lib/otel/issues/20)).

- The interpolation of R expressions in log messages is not supported
  any more ([\#21](https://github.com/r-lib/otel/issues/21)).

## otel 0.1.0

CRAN release: 2025-07-31

First release on CRAN.
