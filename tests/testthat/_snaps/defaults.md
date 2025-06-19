# get_default_tracer_provider

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
      ! Unknown OpenTelemetry exporter from OTEL_R_TRACES_EXPORTER environment variable: invalid

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter bad_package::tracer_provider from OTEL_R_TRACES_EXPORTER environment variable, cannot load package bad_package.

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter otel::no_such_object from OTEL_R_TRACES_EXPORTER environment variable, cannot find provider no_such_object in package otel.

---

    Code
      setup_default_tracer_provider()
    Condition
      Error in `setup_default_tracer_provider()`:
      ! Cannot set trace exporter otel::is_string from OTEL_R_TRACES_EXPORTER environment variable, it is not a list or environment with a 'new' member.

# get_default_logger_provider

    Code
      tp <- get_default_logger_provider()
    Message
      OpenTelemetry error: nope

---

    Code
      get_default_logger_provider_dev()
    Condition
      Error in `setup_default_logger_provider()`:
      ! nope

# setup_default_logger_provider

    Code
      setup_default_logger_provider()
    Condition
      Error in `setup_default_logger_provider()`:
      ! Unknown OpenTelemetry exporter from OTEL_R_LOGS_EXPORTER environment variable: invalid

---

    Code
      setup_default_logger_provider()
    Condition
      Error in `setup_default_logger_provider()`:
      ! Cannot set logs exporter bad_package::logger_provider from OTEL_R_LOGS_EXPORTER environment variable, cannot load package bad_package.

---

    Code
      setup_default_logger_provider()
    Condition
      Error in `setup_default_logger_provider()`:
      ! Cannot set logs exporter otel::no_such_object from OTEL_R_LOGS_EXPORTER environment variable, cannot find provider no_such_object in package otel.

---

    Code
      setup_default_logger_provider()
    Condition
      Error in `setup_default_logger_provider()`:
      ! Cannot set logs exporter otel::is_string from OTEL_R_LOGS_EXPORTER environment variable, it is not a list or environment with a 'new' member.

# get_default_meter_provider

    Code
      tp <- get_default_meter_provider()
    Message
      OpenTelemetry error: nope

---

    Code
      get_default_meter_provider_dev()
    Condition
      Error in `setup_default_meter_provider()`:
      ! nope

# setup_default_meter_provider

    Code
      setup_default_meter_provider()
    Condition
      Warning in `setup_default_meter_provider()`:
      OpenTelemetry: Prometheus trace exporter is not supported yet

---

    Code
      setup_default_meter_provider()
    Condition
      Error in `setup_default_meter_provider()`:
      ! Unknown OpenTelemetry exporter from OTEL_R_METRICS_EXPORTER environment variable: invalid

---

    Code
      setup_default_meter_provider()
    Condition
      Error in `setup_default_meter_provider()`:
      ! Cannot set metrics exporter bad_package::meter_provider from OTEL_R_METRICS_EXPORTER environment variable, cannot load package bad_package.

---

    Code
      setup_default_meter_provider()
    Condition
      Error in `setup_default_meter_provider()`:
      ! Cannot set metrics exporter otel::no_such_object from OTEL_R_METRICS_EXPORTER environment variable, cannot find provider no_such_object in package otel.

---

    Code
      setup_default_meter_provider()
    Condition
      Error in `setup_default_meter_provider()`:
      ! Cannot set metrics exporter otel::is_string from OTEL_R_METRICS_EXPORTER environment variable, it is not a list or environment with a 'new' member.

