test_that("default_tracer_name", {
  skip_on_cran()
  withr::local_envvar(structure(
    NA_character_,
    names = otel_emit_scopes_envvar
  ))
  withr::local_envvar(structure(
    NA_character_,
    names = otel_suppress_scopes_envvar
  ))

  expect_equal(
    default_tracer_name("me-me-me"),
    list(name = "me-me-me", package = NA_character_, on = TRUE)
  )

  r <- list(name = "org.r-project.R", package = "R", on = TRUE)
  # otel is ignored
  fake(default_tracer_name, "topenv", asNamespace("otel"))
  expect_equal(default_tracer_name(), r)

  # otelsdk is ignored
  fake(default_tracer_name, "topenv", asNamespace("otelsdk"))
  expect_equal(default_tracer_name(), r)

  # base env -> R
  fake(default_tracer_name, "topenv", baseenv())
  expect_equal(default_tracer_name(), r)

  # global env -> R
  fake(default_tracer_name, "topenv", globalenv())
  expect_equal(default_tracer_name(), r)

  # package with 'otel_tracer_name'
  fake(default_tracer_name, "topenv", asNamespace("testthat"))
  fake(default_tracer_name, "get0", "custom-name")
  expect_equal(
    default_tracer_name(),
    list(name = "custom-name", package = "testthat", on = TRUE)
  )

  # pakage without 'otel_tracer_name'
  fake(default_tracer_name, "topenv", asNamespace("testthat"))
  fake(default_tracer_name, "get0", NULL)
  expect_equal(
    default_tracer_name(),
    list(name = "r.package.testthat", package = "testthat", on = TRUE)
  )
})

test_that("is_scope_on", {
  withr::local_envvar(structure(
    NA_character_,
    names = otel_emit_scopes_envvar
  ))
  withr::local_envvar(structure(
    NA_character_,
    names = otel_suppress_scopes_envvar
  ))
  expect_true(is_scope_on(list(name = "foo", package = "bar")))

  # only the ones that are explicitly included
  withr::local_envvar(structure("foo", names = otel_emit_scopes_envvar))
  expect_true(is_scope_on(list(name = "foo")))
  expect_true(is_scope_on(list(package = "foo")))
  expect_false(is_scope_on(list(name = "fooo")))
  expect_false(is_scope_on(list(name = "fo")))
  expect_false(is_scope_on(list(package = "fooo")))
  expect_false(is_scope_on(list(package = "fo")))

  withr::local_envvar(structure("foo,bar", names = otel_emit_scopes_envvar))
  expect_true(is_scope_on(list(name = "foo")))
  expect_true(is_scope_on(list(package = "foo")))
  expect_true(is_scope_on(list(name = "bar")))
  expect_true(is_scope_on(list(package = "bar")))
  expect_false(is_scope_on(list(name = "fooo")))
  expect_false(is_scope_on(list(name = "fo")))
  expect_false(is_scope_on(list(package = "fooo")))
  expect_false(is_scope_on(list(package = "fo")))

  withr::local_envvar(structure("foo*,ba?", names = otel_emit_scopes_envvar))
  expect_true(is_scope_on(list(name = "foo")))
  expect_true(is_scope_on(list(package = "foo")))
  expect_true(is_scope_on(list(name = "bar")))
  expect_true(is_scope_on(list(package = "bar")))
  expect_true(is_scope_on(list(name = "baz")))
  expect_true(is_scope_on(list(package = "baz")))
  expect_true(is_scope_on(list(name = "fooo")))
  expect_false(is_scope_on(list(name = "fo")))
  expect_true(is_scope_on(list(package = "fooo")))
  expect_false(is_scope_on(list(package = "fo")))
  expect_false(is_scope_on(list(package = "barz")))

  # exclude some
  withr::local_envvar(structure(
    NA_character_,
    names = otel_emit_scopes_envvar
  ))
  withr::local_envvar(structure("bar", names = otel_suppress_scopes_envvar))
  expect_true(is_scope_on(list(name = "foo")))
  expect_true(is_scope_on(list(package = "foo")))
  expect_false(is_scope_on(list(name = "bar")))
  expect_false(is_scope_on(list(package = "bar")))
  expect_true(is_scope_on(list(name = "fooo")))
  expect_true(is_scope_on(list(package = "fooo")))

  # include some, exclude some
  withr::local_envvar(structure("foo*", names = otel_emit_scopes_envvar))
  withr::local_envvar(structure("foo?", names = otel_suppress_scopes_envvar))
  expect_true(is_scope_on(list(name = "foo")))
  expect_true(is_scope_on(list(package = "foo")))
  expect_true(is_scope_on(list(name = "foobar")))
  expect_true(is_scope_on(list(package = "foobar")))
  expect_false(is_scope_on(list(name = "fooz")))
  expect_false(is_scope_on(list(package = "fooz")))
})
