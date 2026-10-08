# format.otel_tracer_privider

    Code
      get_default_tracer_provider()
    Output
      <otel_tracer_provider_noop/otel_tracer_provider>
      methods:
        get_tracer(name, version, schema_url, attributes)
        flush()
        get_spans()

# format.otel_tracer

    Code
      get_default_tracer_provider()$get_tracer()
    Output
      <otel_tracer_noop/otel_tracer>
      methods:
        start_span(name, attributes, links, options)
        is_enabled()
        flush()

# format.otel_span

    Code
      trc$start_span()
    Output
      <otel_span_noop/otel_span>
      name: <NA>
      methods:
        add_event(name, attributes, timestamp)
        end(options, status_code)
        get_context()
        is_recording()
        record_exception(error_condition, attributes, ...)
        set_attribute(name, value)
        set_status(status_code, description)
        update_name(name)

# format.otel_span_context

    Code
      trc$start_span()$get_context()
    Output
      <otel_span_context_noop/otel_span_context>
      methods:
        get_span_id()
        get_trace_flags()
        get_trace_id()
        is_remote()
        is_sampled()
        is_valid()
        to_http_headers()

# format.otel_logger_provider

    Code
      get_default_logger_provider()
    Output
      <otel_logger_provider_noop/otel_logger_provider>
      methods:
        get_logger(name, minimum_severity, version, schema_url, attributes)
        flush()

# format.otel_logger

    Code
      get_default_logger_provider()$get_logger()
    Output
      <otel_logger_noop/otel_logger>
      methods:
        is_enabled()
        get_minimum_severity()
        set_minimum_severity(minimum_severity)
        log(msg, severity, span_context, attributes, ..., .envir)
        trace(msg, span_context, attributes, ..., .envir)
        debug(msg, span_context, attributes, ..., .envir)
        info(msg, span_context, attributes, ..., .envir)
        warn(msg, span_context, attributes, ..., .envir)
        error(msg, span_context, attributes, ..., .envir)
        fatal(msg, span_context, attributes, ..., .envir)

# format.otel_meter_provider

    Code
      get_default_meter_provider()
    Output
      <otel_meter_provider_noop/otel_meter_provider>
      methods:
        get_meter(name, version, schema_url, attributes)
        flush(timeout)
        shutdown(timeout)
        get_metrics()

# format.otel_meter

    Code
      get_default_meter_provider()$get_meter()
    Output
      <otel_meter_noop/otel_meter>
      methods:
        create_counter(name, description, unit)
        create_up_down_counter(name, description, unit)
        create_histogram(name, description, unit)
        create_gauge(name, description, unit)

# format.otel_counter

    Code
      mtr$create_counter("ctr")
    Output
      <otel_counter_noop/otel_counter>
      methods:
        add(value, attributes, span_context)

# format.otel_up_down_counter

    Code
      mtr$create_up_down_counter("ctr")
    Output
      <otel_up_down_counter_noop/otel_up_down_counter>
      methods:
        add(value, attributes, span_context)

# format.otel_histogram

    Code
      mtr$create_histogram("hst")
    Output
      <otel_histogram_noop/otel_histogram>
      methods:
        record(value, attributes, span_context)

# format.otel_gauge

    Code
      mtr$create_gauge("gge")

