test_that("safe functions are used in prod", {
  expect_equal(the$mode, "prod")
  expect_equal(
    get_default_tracer_provider,
    get_default_tracer_provider_safe
  )
  expect_equal(get_tracer, get_tracer_safe)
})

test_that("otel_save_cache, otel_restore_cache", {
  on.exit(otel_clean_cache(), add = TRUE)
  otel_clean_cache()
  the[["tracer_provider"]] <- "bar"
  the[["meter_provider"]] <- 1:10
  the[["mode"]] <- "prod"
  env <- otel_save_cache()
  expect_true(is.environment(env))
  expect_snapshot(as.list(env))
  otel_clean_cache()
  otel_restore_cache(env)
  expect_true(is.environment(the))
  expect_snapshot(as.list(the))
})

test_that("setup_dev_env", {
  check_prod <- function(env) {
    expect_null(env$get_default_tracer_provider)
    expect_null(env$get_default_logger_provider)
    expect_null(env$get_default_meter_provider)
    expect_null(env$start_shiny_app)
    expect_null(env$start_shiny_session)
    expect_null(env$start_span)
  }

  check_dev <- function(env) {
    expect_equal(
      env$get_default_tracer_provider,
      get_default_tracer_provider_dev
    )
    expect_equal(
      env$get_default_logger_provider,
      get_default_logger_provider_dev
    )
    expect_equal(
      env$get_default_meter_provider,
      get_default_meter_provider_dev
    )
  }

  withr::local_envvar(OTEL_ENV = NA_character_)
  fake <- new.env(parent = emptyenv())
  setup_dev_env(fake)
  check_prod(fake)

  withr::local_envvar(OTEL_ENV = "prod")
  fake <- new.env(parent = emptyenv())
  setup_dev_env(fake)
  check_prod(fake)

  withr::local_envvar(OTEL_ENV = "dev")
  fake <- new.env(parent = emptyenv())
  setup_dev_env(fake)
  check_dev(fake)
})

test_that("setup_r_trace", {
  # env var not set
  fake(setup_r_trace, "get_tracer", function(...) stop("no"))
  withr::local_envvar(OTEL_R_INSTRUMENT_PKGS = NA_character_)
  expect_silent(setup_r_trace())

  # tracer not enabled
  fake(
    setup_r_trace,
    "get_tracer",
    function(...) list(is_enabled = function() FALSE)
  )
  fake(
    setup_r_trace,
    "trace_namespace",
    function(...) stop("nono")
  )
  withr::local_envvar(OTEL_R_INSTRUMENT_PKGS = "foobar")
  expect_silent(setup_r_trace())

  # tracer enabled, package already loaded
  fake(
    setup_r_trace,
    "get_tracer",
    function(...) list(is_enabled = function() TRUE)
  )
  fake(setup_r_trace, "loadedNamespaces", "ok")
  fake(setup_r_trace, "trace_namespace", function(...) res <<- list(...))
  fake(setup_r_trace, "setHook", function(...) stop("not yet"))
  withr::local_envvar(OTEL_R_INSTRUMENT_PKGS = "ok")
  res <- NULL
  setup_r_trace()
  expect_snapshot(res)

  # inclusions, exclusions
  withr::local_envvar(
    OTEL_R_INSTRUMENT_PKGS_OK_INCLUDE = "inc*",
    OTEL_R_INSTRUMENT_PKGS_OK_EXCLUDE = "exclude.*"
  )
  res <- NULL
  setup_r_trace()
  expect_snapshot(res)

  # not loaded, set hook
  fake(setup_r_trace, "loadedNamespaces", "nothere")
  fake(setup_r_trace, "setHook", function(...) res <<- list(...))
  res <- NULL
  setup_r_trace()
  expect_snapshot({
    res[[1]]
    body(res[[2]])
  })
})
