test_that("get_default_traver_provider", {
  on.exit(the$tracer_provider <- NULL, add = TRUE)
  the$tracer_provider <- "foobar"
  expect_equal(get_default_tracer_provider(), "foobar")
  expect_equal(get_default_tracer_provider_dev(), "foobar")
  fake(
    get_default_tracer_provider,
    "setup_default_tracer_provider",
    function() {
      the$tracer_provider <- "new"
    }
  )
  fake(
    get_default_tracer_provider_dev,
    "setup_default_tracer_provider",
    function() {
      the$tracer_provider <- "new"
    }
  )
  expect_equal(get_default_tracer_provider(), "foobar")
  expect_equal(get_default_tracer_provider_dev(), "foobar")
  the$tracer_provider <- NULL
  expect_equal(get_default_tracer_provider(), "new")
  expect_equal(get_default_tracer_provider_dev(), "new")

  fake(
    get_default_tracer_provider,
    "setup_default_tracer_provider",
    function() stop("nope")
  )
  fake(
    get_default_tracer_provider_dev,
    "setup_default_tracer_provider",
    function() stop("nope")
  )
  the$tracer_provider <- NULL
  expect_snapshot({
    tp <- get_default_tracer_provider()
  })
  expect_s3_class(tp, "otel_tracer_provider_noop")
  expect_snapshot(error = TRUE, {
    get_default_tracer_provider_dev()
  })
})

test_that("setup_default_tracer_provider", {
  on.exit(the$tracer_provider <- NULL, add = TRUE)
  set_ev <- function(x, wh = c("r", "generic", "both")) {
    wh <- match.arg(wh)
    ev <- c(
      if (wh %in% c("r", "both")) {
        structure(x, names = default_tracer_exporter_envvar_r)
      },
      if (wh %in% c("generic", "both")) {
        structure(x, names = default_tracer_exporter_envvar)
      }
    )
    withr::local_envvar(ev, .local_envir = parent.frame())
  }

  set_ev(NA_character_, "both")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_noop")

  set_ev("none")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_noop")

  set_ev("console")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_stdstream")

  set_ev("stdout")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_stdstream")

  set_ev("stderr")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_stdstream")

  set_ev("otlp")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_http")

  set_ev("http")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_http")

  set_ev("jaeger")
  expect_snapshot(setup_default_tracer_provider())
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_noop")

  set_ev("zipkin")
  expect_snapshot(setup_default_tracer_provider())
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_noop")

  set_ev("invalid")
  expect_snapshot(error = TRUE, {
    setup_default_tracer_provider()
  })

  # fall back to generic env var
  set_ev(NA_character_)
  set_ev("http", "generic")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_http")

  set_ev("otelsdk::tracer_provider_http")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_http")

  set_ev(NA_character_, "both")
  set_ev("bad_package::tracer_provider")
  expect_snapshot(error = TRUE, {
    setup_default_tracer_provider()
  })

  set_ev("otel::no_such_object")
  expect_snapshot(error = TRUE, {
    setup_default_tracer_provider()
  })

  set_ev("otel::is_string")
  expect_snapshot(error = TRUE, {
    setup_default_tracer_provider()
  })
})
