test_that("set_default_service_name", {
  withr::local_envvar(OTEL_SERVICE_NAME = "ok")
  set_default_service_name()
  expect_equal(get_env("OTEL_SERVICE_NAME"), "ok")

  withr::local_envvar(OTEL_SERVICE_NAME = NA_character_)
  set_default_service_name()
  expect_equal(get_env("OTEL_SERVICE_NAME"), "R")
})

test_that("get_default_tracer_provider", {
  local_otel_cache()
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
  skip_on_cran()
  local_otel_cache()
  set_ev <- function(x, wh = c("r", "generic", "both")) {
    wh <- match.arg(wh)
    ev <- c(
      if (wh %in% c("r", "both")) {
        structure(x, names = default_traces_exporter_envvar_r)
      },
      if (wh %in% c("generic", "both")) {
        structure(x, names = default_traces_exporter_envvar)
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

  set_ev("otlp/file")
  setup_default_tracer_provider()
  expect_s3_class(the$tracer_provider, "otel_tracer_provider_file")

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

test_that("get_default_logger_provider", {
  local_otel_cache()
  the$logger_provider <- "foobar"
  expect_equal(get_default_logger_provider(), "foobar")
  expect_equal(get_default_logger_provider_dev(), "foobar")
  fake(
    get_default_logger_provider,
    "setup_default_logger_provider",
    function() {
      the$logger_provider <- "new"
    }
  )
  fake(
    get_default_logger_provider_dev,
    "setup_default_logger_provider",
    function() {
      the$logger_provider <- "new"
    }
  )
  expect_equal(get_default_logger_provider(), "foobar")
  expect_equal(get_default_logger_provider_dev(), "foobar")
  the$logger_provider <- NULL
  expect_equal(get_default_logger_provider(), "new")
  expect_equal(get_default_logger_provider_dev(), "new")

  fake(
    get_default_logger_provider,
    "setup_default_logger_provider",
    function() stop("nope")
  )
  fake(
    get_default_logger_provider_dev,
    "setup_default_logger_provider",
    function() stop("nope")
  )
  the$logger_provider <- NULL
  expect_snapshot({
    tp <- get_default_logger_provider()
  })
  expect_s3_class(tp, "otel_logger_provider_noop")
  expect_snapshot(error = TRUE, {
    get_default_logger_provider_dev()
  })
})

test_that("setup_default_logger_provider", {
  skip_on_cran()
  local_otel_cache()
  set_ev <- function(x, wh = c("r", "generic", "both")) {
    wh <- match.arg(wh)
    ev <- c(
      if (wh %in% c("r", "both")) {
        structure(x, names = default_logs_exporter_envvar_r)
      },
      if (wh %in% c("generic", "both")) {
        structure(x, names = default_logs_exporter_envvar)
      }
    )
    withr::local_envvar(ev, .local_envir = parent.frame())
  }

  set_ev(NA_character_, "both")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_noop")

  set_ev("none")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_noop")

  set_ev("console")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_stdstream")

  set_ev("stdout")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_stdstream")

  set_ev("stderr")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_stdstream")

  set_ev("otlp/file")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_file")

  set_ev("otlp")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_http")

  set_ev("http")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_http")

  set_ev("invalid")
  expect_snapshot(error = TRUE, {
    setup_default_logger_provider()
  })

  # fall back to generic env var
  set_ev(NA_character_)
  set_ev("http", "generic")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_http")

  set_ev("otelsdk::logger_provider_http")
  setup_default_logger_provider()
  expect_s3_class(the$logger_provider, "otel_logger_provider_http")

  set_ev(NA_character_, "both")
  set_ev("bad_package::logger_provider")
  expect_snapshot(error = TRUE, {
    setup_default_logger_provider()
  })

  set_ev("otel::no_such_object")
  expect_snapshot(error = TRUE, {
    setup_default_logger_provider()
  })

  set_ev("otel::is_string")
  expect_snapshot(error = TRUE, {
    setup_default_logger_provider()
  })
})

test_that("get_default_meter_provider", {
  local_otel_cache()
  the$meter_provider <- "foobar"
  expect_equal(get_default_meter_provider(), "foobar")
  expect_equal(get_default_meter_provider_dev(), "foobar")
  fake(
    get_default_meter_provider,
    "setup_default_meter_provider",
    function() {
      the$meter_provider <- "new"
    }
  )
  fake(
    get_default_meter_provider_dev,
    "setup_default_meter_provider",
    function() {
      the$meter_provider <- "new"
    }
  )
  expect_equal(get_default_meter_provider(), "foobar")
  expect_equal(get_default_meter_provider_dev(), "foobar")
  the$meter_provider <- NULL
  expect_equal(get_default_meter_provider(), "new")
  expect_equal(get_default_meter_provider_dev(), "new")

  fake(
    get_default_meter_provider,
    "setup_default_meter_provider",
    function() stop("nope")
  )
  fake(
    get_default_meter_provider_dev,
    "setup_default_meter_provider",
    function() stop("nope")
  )
  the$meter_provider <- NULL
  expect_snapshot({
    tp <- get_default_meter_provider()
  })
  expect_s3_class(tp, "otel_meter_provider_noop")
  expect_snapshot(error = TRUE, {
    get_default_meter_provider_dev()
  })
})

test_that("setup_default_meter_provider", {
  skip_on_cran()
  local_otel_cache()
  set_ev <- function(x, wh = c("r", "generic", "both")) {
    wh <- match.arg(wh)
    ev <- c(
      if (wh %in% c("r", "both")) {
        structure(x, names = default_metrics_exporter_envvar_r)
      },
      if (wh %in% c("generic", "both")) {
        structure(x, names = default_metrics_exporter_envvar)
      }
    )
    withr::local_envvar(ev, .local_envir = parent.frame())
  }

  set_ev(NA_character_, "both")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_noop")

  set_ev("none")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_noop")

  set_ev("console")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_stdstream")

  set_ev("stdout")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_stdstream")

  set_ev("stderr")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_stdstream")

  set_ev("otlp")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_http")

  set_ev("otlp/file")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_file")

  set_ev("http")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_http")

  set_ev("prometheus")
  expect_snapshot(setup_default_meter_provider())
  expect_s3_class(the$meter_provider, "otel_meter_provider_noop")

  set_ev("invalid")
  expect_snapshot(error = TRUE, {
    setup_default_meter_provider()
  })

  # fall back to generic env var
  set_ev(NA_character_)
  set_ev("http", "generic")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_http")

  set_ev("otelsdk::meter_provider_http")
  setup_default_meter_provider()
  expect_s3_class(the$meter_provider, "otel_meter_provider_http")

  set_ev(NA_character_, "both")
  set_ev("bad_package::meter_provider")
  expect_snapshot(error = TRUE, {
    setup_default_meter_provider()
  })

  set_ev("otel::no_such_object")
  expect_snapshot(error = TRUE, {
    setup_default_meter_provider()
  })

  set_ev("otel::is_string")
  expect_snapshot(error = TRUE, {
    setup_default_meter_provider()
  })
})
