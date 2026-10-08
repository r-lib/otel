# otel (development version)

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
