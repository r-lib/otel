test_that("meter_provider_noop", {
  mp <- meter_provider_noop$new()
  expect_s3_class(mp$get_meter("test"), "otel_meter_noop")
  expect_equal(mp$flush(), mp)
  expect_equal(mp$shutdown(), mp)
  expect_equal(mp$get_metrics(), list())
})

test_that("meter_noop", {
  mp <- meter_provider_noop$new()
  mtr <- mp$get_meter("test")
  expect_s3_class(mtr$create_counter("i1"), "otel_counter_noop")
  expect_s3_class(mtr$create_up_down_counter("i2"), "otel_up_down_counter_noop")
  expect_s3_class(mtr$create_histogram("i3"), "otel_histogram_noop")
  expect_s3_class(mtr$create_gauge("i4"), "otel_gauge_noop")
})

test_that("counter_noop", {
  mp <- meter_provider_noop$new()
  mtr <- mp$get_meter("test")
  inst <- mtr$create_counter("i1")
  expect_equal(inst$add(), inst)
})

test_that("up_down_counter_noop", {
  mp <- meter_provider_noop$new()
  mtr <- mp$get_meter("test")
  inst <- mtr$create_up_down_counter("i1")
  expect_equal(inst$add(), inst)
})

test_that("histogram_noop", {
  mp <- meter_provider_noop$new()
  mtr <- mp$get_meter("test")
  inst <- mtr$create_histogram("i1")
  expect_equal(inst$record(), inst)
})

test_that("gauge_noop", {
  mp <- meter_provider_noop$new()
  mtr <- mp$get_meter("test")
  inst <- mtr$create_gauge("i1")
  expect_equal(inst$record(), inst)
})
