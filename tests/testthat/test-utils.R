test_that("%||%", {
  expect_equal(NULL %||% "foo", "foo")
  expect_equal(NULL %||% NULL, NULL)
  expect_equal("foo" %||% stop("no!"), "foo")
})

test_that("errmsg", {
  expect_snapshot(errmsg("this is a ", "message instead of an error"))
})

test_that("msg", {
  loadNamespace("cli")
  expect_snapshot(msg("just being busy"))
  fake(msg, "loadedNamespaces", character())
  expect_snapshot(msg("nothing to see here"))
})

test_that("get_env", {
  withr::local_envvar(
    THIS_IS_SET = "foo",
    THIS_IS_NOT = NA_character_
  )
  expect_equal(get_env("THIS_IS_SET"), "foo")
  expect_null(get_env("THIS_IS_NOT"))
})

test_that("glob_filter", {
  fns <- c(
    "default_logs_exporter_envvar",
    "default_logs_exporter_envvar_r",
    "errmsg",
    "friendly_type",
    "gauge",
    "get_default_logger_provider",
    "get_default_logger_provider_dev"
  )

  expect_equal(glob_filter(fns), fns)

  expect_snapshot({
    glob_filter(fns, include = "default_*")
    glob_filter(fns, include = c("default_*", "?auge"))
    glob_filter(fns, exclude = c("errmsg", "get_*"))
    glob_filter(fns, include = c("default_*", "?auge"), exclude = "*_r")
  })
})

test_that("get_env_count", {
  withr::local_envvar(FOO = 10)
  expect_equal(get_env_count("FOO", stop("no")), 10L)

  withr::local_envvar(FOO = "inf")
  expect_equal(get_env_count("FOO", stop("no")), Inf)

  withr::local_envvar(FOO = "bad")
  expect_equal(get_env_count("FOO", 1L), 1L)
  expect_equal(get_env_count("FOO", "1"), 1L)

  withr::local_envvar(FOO = "-100")
  expect_equal(get_env_count("FOO", 1L), 1L)
  expect_equal(get_env_count("FOO", "1"), 1L)

  expect_snapshot(error = TRUE, {
    get_env_count("FOO", -1L)
    get_env_count("FOO", "bad-default")
  })
})
