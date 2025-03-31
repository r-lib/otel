# get_default_traver_provider

    Code
      tp <- get_default_tracer_provider()
    Message
      OpenTelemetry error: nope

---

    Code
      get_default_tracer_provider_dev()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! nope

# setup_default_tracer_provider

    Code
      setup_default_tracer_provider()
    Condition
      Warning in `setup_default_tracer_provider()`:
      OpenTelemetry: Jaeger trace exporter is not supported yet

---

    Code
      setup_default_tracer_provider()
    Condition
      Warning in `setup_default_tracer_provider()`:
      OpenTelemetry: Zipkin trace exporter is not supported yet

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Unknown OpenTelemetry exporter from R_OTEL_TRACES_EXPORTER environment variable: invalid

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter bad_package::tracer_provider from R_OTEL_TRACES_EXPORTER environment variable, cannot load package bad_package.

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter otel::no_such_object from R_OTEL_TRACES_EXPORTER environment variable, cannot find provider no_such_object in package otel.

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter otel::is_string from R_OTEL_TRACES_EXPORTER environment variable, it is not a list or environment with a 'new' member.

