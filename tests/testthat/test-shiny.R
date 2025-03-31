test_that("start_shiny_app", {
  skip("unfinished")
  the$tracer_provider <- NULL
  on.exit(the$tracer_provider <- NULL, add = TRUE)
  withr::local_envvar(
    structure("none", names = default_tracer_exporter_envvar_r)
  )

  trc <- start_shiny_app()
  expect_s3_class(trc, "otel_tracer_noop")
  trc2 <- start_shiny_app_dev()
  expect_s3_class(trc2, "otel_tracer_noop")

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  the$tracer_provider <- otelsdk::tracer_provider_stdstream$new(tmp)
  the$span_app <- NULL

  fake(start_shiny_app, "shiny::onStop", function(...) NULL)
  on.exit(the$span_app$end(), add = TRUE)
  trc <- start_shiny_app()
  expect_s3_class(trc, "otel_tracer")
})
