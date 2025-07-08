test_that("start_shiny_app", {
  local_otel_cache()
  withr::local_envvar(
    structure("none", names = default_traces_exporter_envvar_r)
  )

  trc <- start_shiny_app()
  expect_s3_class(trc, "otel_tracer_noop")
  trc2 <- start_shiny_app_dev()
  expect_s3_class(trc2, "otel_tracer_noop")

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  the$tracer_provider <- otelsdk::tracer_provider_stdstream$new(list(
    output = tmp
  ))

  fake(start_shiny_app, "shiny::onStop", function(fn) fn())
  trc <- start_shiny_app()
  expect_s3_class(trc, "otel_tracer")

  fake(start_shiny_app, "get_tracer", function(...) stop("oops"))
  expect_snapshot(trc <- start_shiny_app())
  expect_equal(trc, tracer_noop$new())
})

test_that("start_shiny_app_dev", {
  local_otel_cache()
  fake(start_shiny_app_dev, "get_tracer", function(...) stop("oops"))
  expect_snapshot(error = TRUE, start_shiny_app_dev())
})

test_that("start_shiny_app_dev 2", {
  local_otel_cache()
  fake(start_shiny_app_dev, "get_tracer", function(...) {
    list(
      start_span = function(...) {
        message("start_span")
        list(end = function() message("end_span"))
      },
      is_enabled = function() TRUE
    )
  })
  fake(start_shiny_app_dev, "shiny::onStop", function(fn) fn())
  expect_snapshot(start_shiny_app_dev())
})

test_that("start_shiny_session", {
  local_otel_cache()

  # tracing disabled
  fake(
    start_shiny_session,
    "get_tracer",
    function(...) tracer_noop$new()
  )
  spn <- start_shiny_session(list())
  expect_s3_class(spn, "otel_span_noop")

  # tracing enabled
  fake(start_shiny_session, "get_tracer", function(...) {
    fake_trc <- new.env()
    fake_trc$is_enabled <- function() TRUE
    fake_trc$start_span <- function(...) {
      message("start session")
      list(..., end = function() {
        message("end session")
      })
    }
    fake_trc
  })
  ssn <- list(
    userData = new.env(parent = emptyenv()),
    onSessionEnded = function(fn) fn()
  )
  attr <- list(
    PATH_INFO = "path-info",
    HTTP_HOST = "http-host",
    HTTP_ORIGIN = "http-origin",
    QUERY_STRING = "query-string",
    SERVER_PORT = 11122
  )
  opts <- list(parent = span_noop$new())
  expect_snapshot({
    spn <- start_shiny_session(session = ssn, attributes = attr, options = opts)
  })
  expect_snapshot({
    spn$attributes
  })

  # error
  fake(start_shiny_session, "get_tracer", function(...) stop("boo!"))
  expect_snapshot(spn <- start_shiny_session())
  expect_equal(spn, span_noop$new())
})

test_that("start_shiny_session_dev", {
  local_otel_cache()
  fake(start_shiny_session_dev, "get_tracer", function(...) stop("oops"))
  expect_snapshot(error = TRUE, start_shiny_session_dev())
})

test_that("start_shiny_session_dev 2", {
  local_otel_cache()

  # tracing disabled
  fake(
    start_shiny_session_dev,
    "get_tracer",
    function(...) tracer_noop$new()
  )
  spn <- start_shiny_session_dev(list())
  expect_s3_class(spn, "otel_span_noop")

  # tracing enabled
  fake(start_shiny_session_dev, "get_tracer", function(...) {
    fake_trc <- new.env()
    fake_trc$is_enabled <- function() TRUE
    fake_trc$start_span <- function(...) {
      message("start session")
      list(..., end = function() {
        message("end session")
      })
    }
    fake_trc
  })
  ssn <- list(
    userData = new.env(parent = emptyenv()),
    onSessionEnded = function(fn) fn()
  )
  attr <- list(
    PATH_INFO = "path-info",
    HTTP_HOST = "http-host",
    HTTP_ORIGIN = "http-origin",
    QUERY_STRING = "query-string",
    SERVER_PORT = 11122
  )
  opts <- list(parent = span_noop$new())
  expect_snapshot({
    spn <- start_shiny_session_dev(
      session = ssn,
      attributes = attr,
      options = opts
    )
  })
  expect_snapshot({
    spn$attributes
  })
})
