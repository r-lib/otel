test_that("get_default_tracer", {
  the$tracer_provider <- NULL
  on.exit(the$tracer_provider <- NULL, add = TRUE)
  withr::local_envvar(
    structure("none", names = default_tracer_exporter_envvar_r)
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

test_that("start_span", {
  the$tracer_provider <- NULL
  on.exit(the$tracer_provider <- NULL, add = TRUE)
  withr::local_envvar(
    structure("none", names = default_tracer_exporter_envvar_r)
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
