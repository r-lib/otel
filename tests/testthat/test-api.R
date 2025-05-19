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

test_that("start_span", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  span <- start_span()
  expect_s3_class(span, "otel_span_noop")
  spand <- start_span_dev()
  expect_s3_class(spand, "otel_span_noop")

  span2 <- start_span(session = "foo")
  expect_s3_class(span2, "otel_span_noop")
  span2d <- start_span_dev(session = "foo")
  expect_s3_class(span2d, "otel_span_noop")

  span3 <- start_span(
    session = structure(list(), class = "ShinySession")
  )
  expect_s3_class(span3, "otel_span_noop")
  span3d <- start_span_dev(
    session = structure(list(), class = "ShinySession")
  )
  expect_s3_class(span3d, "otel_span_noop")

  fake(start_span, "get_tracer", function() stop("nope"))
  expect_snapshot({
    span4 <- start_span()
  })
  expect_s3_class(span, "otel_span_noop")

  fake(start_span_dev, "get_tracer", function() stop("nope"))
  expect_snapshot(error = TRUE, {
    span4 <- start_span_dev()
  })
})
