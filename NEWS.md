# otel (development version)

* Length one attribute values that are R arrays, e.g. created with
  `as.array()`, are now recorded as arrays, instead of scalars.
  `as_attributes()` keeps them as arrays.
  This needs otelsdk 0.2.4.9004 or later (#48).

* Spans ended automatically, without an error, now keep the `unset` status,
  instead of being set to `ok`. Only end users should mark spans as `ok`,
  see the OpenTelemetry specification. This needs otelsdk 0.2.4.9004 or
  later (#30).

* Spans now have a `set_attributes()` method, to set multiple attributes
  at once. It needs otelsdk 0.2.4.9004 or later (#35).

* `OTEL_R_SUPPRESS_SCOPES` now turns off a scope if either its scope name
  or its package name matches. Previously both had to match, and scopes
  created with an explicit tracer name could not be turned off at all (#50).

* `default_tracer_name()` (and so `get_tracer()`, `get_logger()` and
  `get_meter()`) now detects the calling package even if a tracer name is
  given. Now `OTEL_R_EMIT_SCOPES` and `OTEL_R_SUPPRESS_SCOPES` can match
  these scopes by package name (#52).

* New `otlp/stdout` exporter for traces, logs and metrics. It writes
  OTLP JSON lines to the standard output. It needs otelsdk 0.2.4.9004 or
  later (#18).

* The `record_exception()` method of no-op spans now accepts the same
  arguments as the method of real spans: `error_condition`, `attributes`
  and `...` (#36).

* `end_span()` now has a `status_code` argument, to set the status of the
  span before ending it (#39).

* `get_active_span()`, `get_active_span_context()`, `pack_http_context()`
  and `extract_http_context()` are now faster: they use a cached internal
  tracer, instead of looking up the tracer name from the call stack.
  They also have a new `tracer` argument, to use a specific tracer (#34).

# otel 0.2.0

* Zero Code Instrumentation (ZCI) works again (#20).

* The interpolation of R expressions in log messages is not supported
  any more (#21).

# otel 0.1.0

First release on CRAN.
