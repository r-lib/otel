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

# get_current_span_context

    Code
      spc2 <- get_active_span_context()
    Message
      Opentelemetry error: nope!

---

    Code
      get_active_span_context_dev()
    Condition
      Error in `get_tracer()`:
      ! nope!

---

    Code
      get_active_span_context_dev()
    Condition
      Error in `trc$get_active_span_context()`:
      ! nope!

# log

    Code
      lgr2 <- log("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# counter_add

    Code
      mtr2 <- counter_add("cx")
    Message
      Opentelemetry error: not today

---

    Code
      counter_add_dev("cx")
    Condition
      Error in `invisible()`:
      ! sorry

# up_down_counter_add

    Code
      mtr2 <- up_down_counter_add("cx")
    Message
      Opentelemetry error: not today

---

    Code
      up_down_counter_add_dev("cx")
    Condition
      Error in `invisible()`:
      ! sorry

# histogram_record

    Code
      mtr2 <- histogram_record("cx")
    Message
      Opentelemetry error: not today

---

    Code
      histogram_record_dev("cx")
    Condition
      Error in `invisible()`:
      ! sorry

# gauge_record

    Code
      mtr2 <- gauge_record("cx")
    Message
      Opentelemetry error: not today

---

    Code
      gauge_record_dev("cx")
    Condition
      Error in `invisible()`:
      ! sorry

# extract_http_context

    Code
      extract_http_context_dev(c("does not matter"))
    Condition
      Error in `trc$extract_http_context()`:
      ! no context

