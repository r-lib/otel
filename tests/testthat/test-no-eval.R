test_that("attributes", {
  local_otel_off()

  expect_silent(start_span("sp", attributes = stop("oh!")))

  expect_silent(start_local_active_span(
    "sp",
    attributes = stop("please ignore")
  ))

  expect_silent(get_meter("org.r-lib.otel")$create_counter("ctr")$add(
    "ctr",
    attributes = stop("don't mind me")
  ))
  expect_silent(get_meter("org.r-lib.otel")$create_gauge("gge")$record(
    attributes = stop("keep on")
  ))
  expect_silent(get_meter("org.r-lib.otel")$create_histogram("hst")$record(
    attributes = stop("and on")
  ))
  expect_silent(get_meter("org.r-lib.otel")$create_up_down_counter("udc")$add(
    "udc",
    attributes = stop("...")
  ))

  trc <- get_tracer()
  expect_silent(trc$start_span("sp", attributes = stop("never mind me")))

  mtr <- get_meter()
  ctr <- mtr$create_counter("ctr")
  expect_silent(ctr$add(attributes = stop("not important")))
  udc <- mtr$create_up_down_counter("udc")
  expect_silent(udc$add(attributes = stop("look away")))
  hst <- mtr$create_histogram("hst")
  expect_silent(hst$record(attributes = stop("close your eyes")))
  gge <- mtr$create_gauge("gge")
  expect_silent(gge$record(attributes = stop("no problem here")))
})

test_that("instrument description and unit", {
  local_otel_off()

  mtr <- get_meter()
  expect_silent(mtr$create_counter("ctr", description = stop("no!")))
  expect_silent(mtr$create_counter("ctr", unit = stop("no!")))
  expect_silent(mtr$create_up_down_counter("udc", description = stop("nah-ah")))
  expect_silent(mtr$create_up_down_counter("udc", unit = stop("nah-ah")))
  expect_silent(mtr$create_histogram("hst", description = stop("nay")))
  expect_silent(mtr$create_histogram("hst", unit = stop("nay")))
  expect_silent(mtr$create_gauge("gge", description = stop("not today")))
  expect_silent(mtr$create_gauge("gge", unit = stop("not today")))
})
