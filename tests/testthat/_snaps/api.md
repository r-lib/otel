# is_tracing

    Code
      is_tracing()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_tracing_dev()
    Condition
      Error in `get_tracer()`:
      ! nope

# is_logging

    Code
      is_logging()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_logging_dev()
    Condition
      Error in `get_logger()`:
      ! nope

# is_measuring

    Code
      is_measuring()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_measuring_dev()
    Condition
      Error in `get_meter()`:
      ! nope

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

# start_session

    Code
      sessx <- start_session()
    Message
      OpenTelemetry error: no session

---

    Code
      start_session_dev()
    Condition
      Error in `get_tracer()`:
      ! no session

# local_session

    Code
      local_session(sess)
    Message
      OpenTelemetry error: no!

---

    Code
      local_session_dev(sess)
    Condition
      Error in `session$activate()`:
      ! no!

# with_session

    Code
      ret <- with_session(sess, 1 + 1)
    Message
      OpenTelemetry error: no!

---

    Code
      with_session_dev(sess, 1 + 1)
    Condition
      Error in `session$activate()`:
      ! no!

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

# log_trace

    Code
      lgr2 <- log_trace("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_trace_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# log_debug

    Code
      lgr2 <- log_debug("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_debug_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# log_info

    Code
      lgr2 <- log_info("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_info_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# log_warn

    Code
      lgr2 <- log_warn("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_warn_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# log_error

    Code
      lgr2 <- log_error("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_error_dev("nothing")
    Condition
      Error in `lgr$log()`:
      ! no

# log_fatal

    Code
      lgr2 <- log_fatal("another nothing")
    Message
      Opentelemetry error: denied!

---

    Code
      log_fatal_dev("nothing")
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

# pack_http_context

    Code
      pack_http_context()
    Message
      Opentelemetry error: sorry
    Output
      named character(0)

---

    Code
      pack_http_context_dev()
    Condition
      Error in `get_tracer()`:
      ! sorry

# extract_http_context

    Code
      spc2 <- extract_http_context(c("does not matter"))
    Message
      Opentelemetry error: out of context

---

    Code
      extract_http_context_dev(c("does not matter"))
    Condition
      Error in `trc$extract_http_context()`:
      ! no context

