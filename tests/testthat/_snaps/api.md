# is_tracing_enabled

    Code
      is_tracing_enabled()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_tracing_enabled_dev()
    Condition
      Error in `get_tracer()`:
      ! nope

# is_logging_enabled

    Code
      is_logging_enabled()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_logging_enabled_dev()
    Condition
      Error in `get_logger()`:
      ! unused argument (logger)

# is_measuring_enabled

    Code
      is_measuring_enabled()
    Message
      OpenTelemetry error: nope
    Output
      [1] FALSE

---

    Code
      is_measuring_enabled_dev()
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

# start_local_active_span

    Code
      span4 <- start_local_active_span()
    Message
      OpenTelemetry error: nope

---

    Code
      span4 <- start_local_active_span_dev()
    Condition
      Error in `get_tracer()`:
      ! nope

# start_span

    Code
      sessx <- start_span()
    Message
      OpenTelemetry error: nope

---

    Code
      start_span_dev()
    Condition
      Error in `get_tracer()`:
      ! nope

# end_span

    Code
      end_span(span)
    Message
      OpenTelemetry error: not yet

---

    Code
      end_span_dev(span)
    Condition
      Error in `identity()`:
      ! not yet

# local_active_span

    Code
      local_active_span(sess)
    Message
      OpenTelemetry error: no!

---

    Code
      local_active_span_dev(sess)
    Condition
      Error in `span$activate()`:
      ! no!

# with_active_span

    Code
      ret <- with_active_span(sess, 1 + 1)
    Message
      OpenTelemetry error: no!

---

    Code
      with_active_span_dev(sess, 1 + 1)
    Condition
      Error in `span$activate()`:
      ! no!

# get_current_span_context

    Code
      spc2 <- get_active_span_context()
    Message
      OpenTelemetry error: nope!

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
      OpenTelemetry error: denied!

---

    Code
      log_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_trace

    Code
      lgr2 <- log_trace("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_trace_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_debug

    Code
      lgr2 <- log_debug("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_debug_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_info

    Code
      lgr2 <- log_info("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_info_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_warn

    Code
      lgr2 <- log_warn("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_warn_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_error

    Code
      lgr2 <- log_error("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_error_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# log_fatal

    Code
      lgr2 <- log_fatal("another nothing")
    Message
      OpenTelemetry error: denied!

---

    Code
      log_fatal_dev("nothing")
    Condition
      Error in `logger$log()`:
      ! no

# counter_add

    Code
      mtr2 <- counter_add("cx")
    Message
      OpenTelemetry error: not today

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
      OpenTelemetry error: not today

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
      OpenTelemetry error: not today

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
      OpenTelemetry error: not today

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
      OpenTelemetry error: sorry
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
      OpenTelemetry error: out of context

---

    Code
      extract_http_context_dev(c("does not matter"))
    Condition
      Error in `trc$extract_http_context()`:
      ! no context

