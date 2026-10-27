# Changelog

## otel (development version)

- [`start_local_active_span()`](https://otel.r-lib.org/dev/reference/start_local_active_span.md)
  and
  [`start_span()`](https://otel.r-lib.org/dev/reference/start_span.md)
  now use the name of the calling function as the span name, if `name`
  is not given. They also set the `code.function.name` attribute. This
  only happens if tracing is enabled, so it costs nothing if tracing is
  off ([\#45](https://github.com/r-lib/otel/issues/45)).

- Length one attribute values that are R arrays, e.g. created with
  [`as.array()`](https://rdrr.io/r/base/array.html), are now recorded as
  arrays, instead of scalars.
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  keeps them as arrays. This needs otelsdk 0.2.4.9004 or later
  ([\#48](https://github.com/r-lib/otel/issues/48)).

- Spans ended automatically, without an error, now keep the `unset`
  status, instead of being set to `ok`. Only end users should mark spans
  as `ok`, see the OpenTelemetry specification. This needs otelsdk
  0.2.4.9004 or later ([\#30](https://github.com/r-lib/otel/issues/30)).

- Spans now have a `set_attributes()` method, to set multiple attributes
  at once. It needs otelsdk 0.2.4.9004 or later
  ([\#35](https://github.com/r-lib/otel/issues/35)).

- `OTEL_R_SUPPRESS_SCOPES` now turns off a scope if either its scope
  name or its package name matches. Previously both had to match, and
  scopes created with an explicit tracer name could not be turned off at
  all ([\#50](https://github.com/r-lib/otel/issues/50)).

- [`default_tracer_name()`](https://otel.r-lib.org/dev/reference/default_tracer_name.md)
  (and so
  [`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md),
  [`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md)
  and
  [`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md))
  now detects the calling package even if a tracer name is given. Now
  `OTEL_R_EMIT_SCOPES` and `OTEL_R_SUPPRESS_SCOPES` can match these
  scopes by package name
  ([\#52](https://github.com/r-lib/otel/issues/52)).

- New `otlp/stdout` exporter for traces, logs and metrics. It writes
  OTLP JSON lines to the standard output. It needs otelsdk 0.2.4.9004 or
  later ([\#18](https://github.com/r-lib/otel/issues/18)).

- The `record_exception()` method of no-op spans now accepts the same
  arguments as the method of real spans: `error_condition`, `attributes`
  and `...` ([\#36](https://github.com/r-lib/otel/issues/36)).

- [`end_span()`](https://otel.r-lib.org/dev/reference/end_span.md) now
  has a `status_code` argument, to set the status of the span before
  ending it ([\#39](https://github.com/r-lib/otel/issues/39)).

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
