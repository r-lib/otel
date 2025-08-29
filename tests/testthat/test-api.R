test_that("is_tracing_enabled", {
  fake(is_tracing_enabled, "get_tracer", list(is_enabled = function() FALSE))
  expect_false(is_tracing_enabled())

  fake(is_tracing_enabled, "get_tracer", function(name) stop("nope"))
  expect_snapshot(is_tracing_enabled())

  fake(
    is_tracing_enabled_dev,
    "get_tracer",
    list(is_enabled = function() FALSE)
  )
  expect_false(is_tracing_enabled_dev())

  fake(is_tracing_enabled_dev, "get_tracer", function(name) stop("nope"))
  expect_snapshot(error = TRUE, is_tracing_enabled_dev())
})

test_that("is_logging_enabled", {
  fake(
    is_logging_enabled,
    "get_logger",
    structure(
      list(is_enabled = function(...) FALSE),
      class = "otel_logger_noop"
    )
  )
  expect_false(is_logging_enabled())

  fake(
    is_logging_enabled,
    "get_logger",
    structure(list(is_enabled = function(...) TRUE), class = "otel_logger")
  )
  expect_true(is_logging_enabled())

  fake(is_logging_enabled, "get_logger", function(...) stop("nope"))
  expect_snapshot(is_logging_enabled())

  fake(
    is_logging_enabled_dev,
    "get_logger",
    structure(
      list(is_enabled = function(...) FALSE),
      class = "otel_logger_noop"
    )
  )
  expect_false(is_logging_enabled_dev())

  fake(
    is_logging_enabled_dev,
    "get_logger",
    structure(list(is_enabled = function(...) TRUE), class = "otel_logger")
  )
  expect_true(is_logging_enabled_dev())

  fake(is_logging_enabled_dev, "get_logger", function() stop("nope"))
  expect_snapshot(error = TRUE, is_logging_enabled_dev())
})

test_that("is_measuring_enabled", {
  fake(
    is_measuring_enabled,
    "get_meter",
    structure(list(is_enabled = function() FALSE), class = "otel_meter_noop")
  )
  expect_false(is_measuring_enabled())

  fake(
    is_measuring_enabled,
    "get_meter",
    structure(list(is_enabled = function() TRUE), class = "otel_meter")
  )
  expect_true(is_measuring_enabled())

  fake(is_measuring_enabled, "get_meter", function(...) stop("nope"))
  expect_snapshot(is_measuring_enabled())

  fake(
    is_measuring_enabled_dev,
    "get_meter",
    structure(list(is_enabled = function() FALSE), class = "otel_meter_noop")
  )
  expect_false(is_measuring_enabled_dev())

  fake(
    is_measuring_enabled_dev,
    "get_meter",
    structure(list(is_enabled = function() TRUE), class = "otel_meter")
  )
  expect_true(is_measuring_enabled_dev())

  fake(is_measuring_enabled_dev, "get_meter", function(...) stop("nope"))
  expect_snapshot(error = TRUE, is_measuring_enabled_dev())
})

test_that("get_default_tracer", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  trc <- get_tracer()
  expect_s3_class(trc, "otel_tracer")
  expect_s3_class(
    get_default_tracer_provider(),
    "otel_tracer_provider_noop"
  )

  the$tracer_provider <- NULL
  trc <- get_tracer_dev()
  expect_s3_class(trc, "otel_tracer")
  expect_s3_class(
    get_default_tracer_provider(),
    "otel_tracer_provider_noop"
  )

  fake(get_tracer, "get_default_tracer_provider", function() stop("nope"))
  expect_snapshot({
    trc <- get_tracer()
  })
  expect_equal(trc, tracer_noop$new())

  fake(get_tracer_dev, "get_default_tracer_provider", function() stop("x"))
  expect_snapshot(error = TRUE, {
    get_tracer_dev()
  })
})

test_that("get_default_logger", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_logs_exporter_envvar_r)
  )

  lgr <- get_logger()
  expect_s3_class(lgr, "otel_logger")
  expect_s3_class(
    get_default_logger_provider(),
    "otel_logger_provider_noop"
  )

  the$logger_provider <- NULL
  lgr <- get_logger_dev()
  expect_s3_class(lgr, "otel_logger")
  expect_s3_class(
    get_default_logger_provider(),
    "otel_logger_provider_noop"
  )

  fake(get_logger, "get_default_logger_provider", function() stop("nope"))
  expect_snapshot(
    lgr <- get_logger()
  )
  expect_equal(lgr, logger_noop$new())

  fake(get_logger_dev, "get_default_logger_provider", function() stop("x"))
  expect_snapshot(error = TRUE, {
    get_logger_dev()
  })
})

test_that("get_default_meter", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_metrics_exporter_envvar_r)
  )

  mtr <- get_meter()
  expect_s3_class(mtr, "otel_meter")
  expect_s3_class(
    get_default_meter_provider(),
    "otel_meter_provider_noop"
  )

  the$meter_provider <- NULL
  mtr <- get_meter_dev()
  expect_s3_class(mtr, "otel_meter")
  expect_s3_class(
    get_default_meter_provider(),
    "otel_meter_provider_noop"
  )

  fake(get_meter, "get_default_meter_provider", function() stop("nope"))
  expect_snapshot(
    mtr <- get_meter()
  )
  expect_equal(mtr, meter_noop$new())

  fake(get_meter_dev, "get_default_meter_provider", function() stop("x"))
  expect_snapshot(error = TRUE, {
    get_meter_dev()
  })
})

test_that("start_local_active_span", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  span <- start_local_active_span()
  expect_s3_class(span, "otel_span_noop")
  spand <- start_local_active_span_dev()
  expect_s3_class(spand, "otel_span_noop")

  fake(start_local_active_span, "get_tracer", function(...) stop("nope"))
  expect_snapshot({
    span4 <- start_local_active_span()
  })
  expect_s3_class(span4, "otel_span_noop")

  fake(start_local_active_span_dev, "get_tracer", function(...) stop("nope"))
  expect_snapshot(error = TRUE, {
    span4 <- start_local_active_span_dev()
  })
})

test_that("start_span", {
  local_otel_off()
  sess <- start_span()
  expect_s3_class(sess, "otel_span_noop")
  sessd <- start_span_dev()
  expect_s3_class(sessd, "otel_span_noop")

  fake(start_span, "get_tracer", function(...) stop("nope"))
  expect_snapshot(sessx <- start_span())
  expect_s3_class(sessx, "otel_span_noop")

  fake(start_span_dev, "get_tracer", function(...) stop("nope"))
  expect_snapshot(error = TRUE, start_span_dev())
})

test_that("end_span", {
  local_otel_off()
  span <- start_local_active_span()
  expect_s3_class(span, "otel_span")
  end_span(span)
  end_span(span)

  span <- start_local_active_span()
  expect_s3_class(span, "otel_span")
  end_span_dev(span)
  end_span_dev(span)

  fake(end_span, "identity", function(...) stop("not yet"))
  expect_snapshot(end_span(span))

  fake(end_span_dev, "identity", function(...) stop("not yet"))
  expect_snapshot(error = TRUE, end_span_dev(span))
})

test_that("local_active_span", {
  local_otel_off()
  sess <- get_tracer("org.r-lib.otel")$start_span()
  expect_silent(local_active_span(sess))
  expect_silent(local_active_span_dev(sess))

  sess$activate <- function(...) stop("no!")
  expect_snapshot(local_active_span(sess))

  expect_snapshot(error = TRUE, local_active_span_dev(sess))
})

test_that("with_active_span", {
  local_otel_off()
  sess <- get_tracer("org.r-lib.otel")$start_span()
  expect_silent(ret <- with_active_span(sess, 1 + 1))
  expect_equal(ret, 2)
  expect_silent(ret <- with_active_span_dev(sess, 1 + 1))
  expect_equal(ret, 2)

  sess$activate <- function(...) stop("no!")
  expect_snapshot(ret <- with_active_span(sess, 1 + 1))
  expect_equal(ret, 2)

  expect_snapshot(error = TRUE, with_active_span_dev(sess, 1 + 1))
})

test_that("get_current_span_context", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  spc <- get_active_span_context()
  expect_s3_class(spc, "otel_span_context")
  expect_s3_class(spc, "otel_span_context_noop")
  expect_false(spc$is_valid())

  # recover from error
  fake(get_active_span_context, "get_tracer", function() stop("nope!"))
  expect_snapshot({
    spc2 <- get_active_span_context()
  })
  expect_s3_class(spc2, "otel_span_context_noop")

  # error
  fake(get_active_span_context_dev, "get_tracer", function() stop("nope!"))
  expect_snapshot(error = TRUE, {
    get_active_span_context_dev()
  })

  # error 2
  fake(
    get_active_span_context_dev,
    "get_tracer",
    function() list(get_active_span_context = function() stop("nope!"))
  )
  expect_snapshot(error = TRUE, {
    get_active_span_context_dev()
  })
})

test_that("get_active_span", {
  local_otel_off()

  spn <- get_active_span()
  expect_s3_class(spn, "otel_span")
  expect_s3_class(spn, "otel_span_noop")
  expect_false(spn$get_context()$is_valid())

  # recover from error
  fake(get_active_span, "get_tracer", function() stop("ouch!"))
  expect_snapshot({
    spn2 <- get_active_span()
  })
  expect_s3_class(spn2, "otel_span_noop")

  # error
  fake(get_active_span_dev, "get_tracer", function() stop("nope!"))
  expect_snapshot(error = TRUE, {
    get_active_span_dev()
  })

  # error 2
  fake(
    get_active_span_dev,
    "get_tracer",
    function() list(get_active_span = function() stop("nope!"))
  )
  expect_snapshot(error = TRUE, {
    get_active_span_dev()
  })
})

test_that("log", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_logs_exporter_envvar_r)
  )

  lgr <- log("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_dev, "get_logger", function() list(log = function(...) stop("no")))
  expect_snapshot(error = TRUE, {
    log_dev("nothing")
  })
})

test_that("log_trace", {
  local_otel_off()
  lgr <- log_trace("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_trace, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_trace("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_trace_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_trace_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_trace_dev("nothing")
  })
})

test_that("log_debug", {
  local_otel_off()
  lgr <- log_debug("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_debug, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_debug("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_debug_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_debug_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_debug_dev("nothing")
  })
})

test_that("log_info", {
  local_otel_off()
  lgr <- log_info("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_info, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_info("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_info_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_info_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_info_dev("nothing")
  })
})

test_that("log_warn", {
  local_otel_off()
  lgr <- log_warn("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_warn, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_warn("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_warn_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_warn_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_warn_dev("nothing")
  })
})

test_that("log_error", {
  local_otel_off()
  lgr <- log_error("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_error, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_error("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_error_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_error_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_error_dev("nothing")
  })
})

test_that("log_fatal", {
  local_otel_off()
  lgr <- log_fatal("log message going nowhere")
  expect_s3_class(lgr, "otel_logger_noop")

  # error
  fake(log_fatal, "get_logger", function() stop("denied!"))
  expect_snapshot({
    lgr2 <- log_fatal("another nothing")
  })
  expect_s3_class(lgr, "otel_logger_noop")

  lgr3 <- log_fatal_dev("Still nowhere")
  expect_s3_class(lgr3, "otel_logger_noop")

  # error
  fake(log_fatal_dev, "get_logger", function() {
    list(log = function(...) stop("no"))
  })
  expect_snapshot(error = TRUE, {
    log_fatal_dev("nothing")
  })
})

test_that("counter_add", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_metrics_exporter_envvar_r)
  )

  mtr <- counter_add("cx")
  expect_s3_class(mtr, "otel_counter_noop")

  # error
  fake(counter_add, "get_meter", function() stop("not today"))
  expect_snapshot({
    mtr2 <- counter_add("cx")
  })
  expect_s3_class(mtr2, "otel_counter_noop")

  mtr3 <- counter_add_dev("cx")
  expect_s3_class(mtr3, "otel_counter_noop")

  # error
  fake(counter_add_dev, "invisible", function(x) stop("sorry"))
  expect_snapshot(error = TRUE, {
    counter_add_dev("cx")
  })
})

test_that("up_down_counter_add", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_metrics_exporter_envvar_r)
  )

  mtr <- up_down_counter_add("cx")
  expect_s3_class(mtr, "otel_up_down_counter_noop")

  # error
  fake(up_down_counter_add, "get_meter", function() stop("not today"))
  expect_snapshot({
    mtr2 <- up_down_counter_add("cx")
  })
  expect_s3_class(mtr2, "otel_up_down_counter_noop")

  mtr3 <- up_down_counter_add_dev("cx")
  expect_s3_class(mtr3, "otel_up_down_counter_noop")

  # error
  fake(up_down_counter_add_dev, "invisible", function(x) stop("sorry"))
  expect_snapshot(error = TRUE, {
    up_down_counter_add_dev("cx")
  })
})

test_that("histogram_record", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_metrics_exporter_envvar_r)
  )

  mtr <- histogram_record("cx")
  expect_s3_class(mtr, "otel_histogram_noop")

  # error
  fake(histogram_record, "get_meter", function() stop("not today"))
  expect_snapshot({
    mtr2 <- histogram_record("cx")
  })
  expect_s3_class(mtr2, "otel_histogram_noop")

  mtr3 <- histogram_record_dev("cx")
  expect_s3_class(mtr3, "otel_histogram_noop")

  # error
  fake(histogram_record_dev, "invisible", function(x) stop("sorry"))
  expect_snapshot(error = TRUE, {
    histogram_record_dev("cx")
  })
})

test_that("gauge_record", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_metrics_exporter_envvar_r)
  )

  mtr <- gauge_record("cx")
  expect_s3_class(mtr, "otel_gauge_noop")

  # error
  fake(gauge_record, "get_meter", function() stop("not today"))
  expect_snapshot({
    mtr2 <- gauge_record("cx")
  })
  expect_s3_class(mtr2, "otel_gauge_noop")

  mtr3 <- gauge_record_dev("cx")
  expect_s3_class(mtr3, "otel_gauge_noop")

  # error
  fake(gauge_record_dev, "invisible", function(x) stop("sorry"))
  expect_snapshot(error = TRUE, {
    gauge_record_dev("cx")
  })
})

test_that("pack_http_context", {
  local_otel_off()
  fake(
    pack_http_context,
    "get_tracer",
    list(get_active_span_context = function() {
      list(to_http_headers = function() c(FOO = "bar"))
    })
  )
  expect_equal(pack_http_context(), c(FOO = "bar"))

  fake(
    pack_http_context_dev,
    "get_tracer",
    list(get_active_span_context = function() {
      list(to_http_headers = function() c(FOO = "bar"))
    })
  )
  expect_equal(pack_http_context_dev(), c(FOO = "bar"))

  fake(pack_http_context, "get_tracer", function() stop("sorry"))
  expect_snapshot(pack_http_context())

  fake(pack_http_context_dev, "get_tracer", function() stop("sorry"))
  expect_snapshot(error = TRUE, pack_http_context_dev())
})

test_that("extract_http_context", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  spc <- extract_http_context(c(traceparent = "something"))
  expect_s3_class(spc, "otel_span_context_noop")

  # error
  fake(extract_http_context, "get_tracer", function() stop("out of context"))
  expect_snapshot(spc2 <- extract_http_context(c("does not matter")))
  expect_s3_class(spc2, "otel_span_context_noop")

  spc3 <- extract_http_context_dev(c("does not matter"))
  expect_s3_class(spc3, "otel_span_context_noop")

  # error
  fake(
    extract_http_context_dev,
    "get_tracer",
    function() list(extract_http_context = function(...) stop("no context"))
  )
  expect_snapshot(error = TRUE, {
    extract_http_context_dev(c("does not matter"))
  })
})
