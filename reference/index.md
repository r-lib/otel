# Package index

## Other Documentation

This is the reference manual of the otel package. Other forms of
documentation:

- [Getting Started](https://otel.r-lib.org/reference/gettingstarted.md),
  a tutorial and cookbook for instrumentation.
- [Collecting telemetry
  data](https://otelsdk.r-lib.org/reference/collecting.html), a tutorial
  and cookbook on telemetry data collection.

## Configuration

- [`default_tracer_name()`](https://otel.r-lib.org/reference/default_tracer_name.md)
  : Default tracer name (and meter and logger name) for an R package
- [`Environment Variables`](https://otel.r-lib.org/reference/environmentvariables.md)
  : Environment variables to configure otel

## Traces

### Trace API

- [`end_span()`](https://otel.r-lib.org/reference/end_span.md) : End an
  OpenTelemetry span
- [`is_tracing_enabled()`](https://otel.r-lib.org/reference/is_tracing_enabled.md)
  : Check if tracing is active
- [`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
  : Start and activate a span
- [`start_span()`](https://otel.r-lib.org/reference/start_span.md) :
  Start an OpenTelemetry span.
- [`invalid_trace_id`](https://otel.r-lib.org/reference/tracing-constants.md)
  [`invalid_span_id`](https://otel.r-lib.org/reference/tracing-constants.md)
  [`span_kinds`](https://otel.r-lib.org/reference/tracing-constants.md)
  [`span_status_codes`](https://otel.r-lib.org/reference/tracing-constants.md)
  : OpenTelemetry tracing constants
- [`Zero Code Instrumentation`](https://otel.r-lib.org/reference/zci.md)
  : Zero Code Instrumentation

### Concurrency

- [`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
  : Activate an OpenTelemetry span for an R scope
- [`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md)
  : Evaluate R code with an active OpenTelemetry span

### Low Level Trace API

- [`get_default_tracer_provider()`](https://otel.r-lib.org/reference/get_default_tracer_provider.md)
  : Get the default tracer provider
- [`get_tracer()`](https://otel.r-lib.org/reference/get_tracer.md) : Get
  a tracer from the default tracer provider
- [`otel_span`](https://otel.r-lib.org/reference/otel_span.md) :
  OpenTelemetry Span Object
- [`otel_span_context`](https://otel.r-lib.org/reference/otel_span_context.md)
  : An OpenTelemetry Span Context object
- [`otel_tracer`](https://otel.r-lib.org/reference/otel_tracer.md) :
  OpenTelemetry Tracer Object
- [`otel_tracer_provider`](https://otel.r-lib.org/reference/otel_tracer_provider.md)
  : OpenTelemetry Tracer Provider Object
- [`tracer_provider_noop`](https://otel.r-lib.org/reference/tracer_provider_noop.md)
  : No-op tracer provider

## Logs

### Logs API

- [`is_logging_enabled()`](https://otel.r-lib.org/reference/is_logging_enabled.md)
  : Check whether OpenTelemetry logging is active
- [`log()`](https://otel.r-lib.org/reference/log.md)
  [`log_trace()`](https://otel.r-lib.org/reference/log.md)
  [`log_debug()`](https://otel.r-lib.org/reference/log.md)
  [`log_info()`](https://otel.r-lib.org/reference/log.md)
  [`log_warn()`](https://otel.r-lib.org/reference/log.md)
  [`log_error()`](https://otel.r-lib.org/reference/log.md)
  [`log_fatal()`](https://otel.r-lib.org/reference/log.md) : Log an
  OpenTelemetry log message
- [`log_severity_levels`](https://otel.r-lib.org/reference/log_severity_levels.md)
  : OpenTelemetry log severity levels

### Low Level Logs API

- [`get_default_logger_provider()`](https://otel.r-lib.org/reference/get_default_logger_provider.md)
  : Get the default logger provider
- [`get_logger()`](https://otel.r-lib.org/reference/get_logger.md) : Get
  a logger from the default logger provider
- [`logger_provider_noop`](https://otel.r-lib.org/reference/logger_provider_noop.md)
  : No-op logger provider
- [`otel_logger`](https://otel.r-lib.org/reference/otel_logger.md) :
  OpenTelemetry Logger Object
- [`otel_logger_provider`](https://otel.r-lib.org/reference/otel_logger_provider.md)
  : OpenTelemetry Logger Provider Object

## Metrics

### Metrics API

- [`counter_add()`](https://otel.r-lib.org/reference/counter_add.md) :
  Increase an OpenTelemetry counter
- [`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md) :
  Record a value of an OpenTelemetry gauge
- [`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md)
  : Record a value of an OpenTelemetry histogram
- [`is_measuring_enabled()`](https://otel.r-lib.org/reference/is_measuring_enabled.md)
  : Check whether OpenTelemetry metrics collection is active
- [`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)
  : Increase or decrease an OpenTelemetry up-down counter

### Low Level Metrics API

- [`get_default_meter_provider()`](https://otel.r-lib.org/reference/get_default_meter_provider.md)
  : Get the default meter provider
- [`get_meter()`](https://otel.r-lib.org/reference/get_meter.md) : Get a
  meter from the default meter provider
- [`meter_provider_noop`](https://otel.r-lib.org/reference/meter_provider_noop.md)
  : No-op Meter Provider
- [`otel_counter`](https://otel.r-lib.org/reference/otel_counter.md) :
  OpenTelemetry Counter Object
- [`otel_gauge`](https://otel.r-lib.org/reference/otel_gauge.md) :
  OpenTelemetry Gauge Object
- [`otel_histogram`](https://otel.r-lib.org/reference/otel_histogram.md)
  : OpenTelemetry Histogram Object
- [`otel_meter`](https://otel.r-lib.org/reference/otel_meter.md) :
  OpenTelemetry Meter Object
- [`otel_meter_provider`](https://otel.r-lib.org/reference/otel_meter_provider.md)
  : OpenTelemetry meter provider objects
- [`otel_up_down_counter`](https://otel.r-lib.org/reference/otel_up_down_counter.md)
  : OpenTelemetry Up-Down Counter Object

## Utility Functions

- [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  : R objects as OpenTelemetry attributes
- [`get_active_span()`](https://otel.r-lib.org/reference/get_active_span.md)
  : Returns the active span, if any
- [`get_active_span_context()`](https://otel.r-lib.org/reference/get_active_span_context.md)
  : Returns the active span context

## Context Propagation

- [`extract_http_context()`](https://otel.r-lib.org/reference/extract_http_context.md)
  : Extract a span context from HTTP headers received from a client
- [`pack_http_context()`](https://otel.r-lib.org/reference/pack_http_context.md)
  : Pack the currently active span context into standard HTTP
  OpenTelemetry headers
