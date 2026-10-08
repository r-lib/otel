test_that("format.otel_tracer_privider", {
  local_otel_off()
  expect_snapshot(get_default_tracer_provider())
})

test_that("format.otel_tracer", {
  local_otel_off()
  expect_snapshot(get_default_tracer_provider()$get_tracer())
})

test_that("format.otel_span", {
  local_otel_off()
  trc <- get_default_tracer_provider()$get_tracer()
  expect_snapshot(trc$start_span())
})

test_that("format.otel_span_context", {
  local_otel_off()
  trc <- get_default_tracer_provider()$get_tracer()
  expect_snapshot(trc$start_span()$get_context())
})

test_that("format.otel_logger_provider", {
  local_otel_off()
  expect_snapshot(get_default_logger_provider())
})

test_that("format.otel_logger", {
  local_otel_off()
  expect_snapshot(get_default_logger_provider()$get_logger())
})

test_that("format.otel_meter_provider", {
  local_otel_off()
  expect_snapshot(get_default_meter_provider())
})

test_that("format.otel_meter", {
  local_otel_off()
  expect_snapshot(get_default_meter_provider()$get_meter())
})

test_that("format.otel_counter", {
  local_otel_off()
  mtr <- get_default_meter_provider()$get_meter()
  expect_snapshot(mtr$create_counter("ctr"))
})

test_that("format.otel_up_down_counter", {
  local_otel_off()
  mtr <- get_default_meter_provider()$get_meter()
  expect_snapshot(mtr$create_up_down_counter("ctr"))
})

test_that("format.otel_histogram", {
  local_otel_off()
  mtr <- get_default_meter_provider()$get_meter()
  expect_snapshot(mtr$create_histogram("hst"))
})

test_that("format.otel_gauge", {
  local_otel_off()
  mtr <- get_default_meter_provider()$get_meter()
  expect_snapshot(mtr$create_gauge("gge"))
})
