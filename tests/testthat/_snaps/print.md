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

