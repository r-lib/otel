# get_default_tracer

    Code
      trc <- get_tracer()
    Message
      OpenTelemetry error: nope

---

    Code
      get_tracer_dev()
    Condition
      Error in `get_default_tracer_provider()`:
      ! x

# get_default_logger

    Code
      lgr <- get_logger()
    Message
      OpenTelemetry error: nope

---

    Code
      get_logger_dev()
    Condition
      Error in `get_default_logger_provider()`:
      ! x

# get_default_meter

    Code
      mtr <- get_meter()
    Message
      OpenTelemetry error: nope

---

    Code
      get_meter_dev()
    Condition
      Error in `get_default_meter_provider()`:
      ! x

# start_span

    Code
      span4 <- start_span()
    Message
      OpenTelemetry error: nope

---

    Code
      span4 <- start_span_dev()
    Condition
      Error in `get_tracer()`:
      ! nope

