test_that("multiplication works", {
  expect_equal(the$mode, "prod")
  expect_equal(
    get_default_tracer_provider,
    get_default_tracer_provider_safe
  )
  expect_equal(get_default_tracer, get_default_tracer_safe)
  expect_equal(start_shiny_app, start_shiny_app_safe)
  expect_equal(start_shiny_session, start_shiny_session_safe)
  expect_equal(start_span, start_span_safe)
})
