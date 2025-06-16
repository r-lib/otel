test_that("simple", {
  withr::local_envvar(
    OTEL_ATTRIBUTE_COUNT_LIMIT = NA_character_,
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT = NA_character_
  )

  ok <- list(a = "", b = FALSE, c = 1:3 / 2, d = 1:4)
  expect_equal(as_attributes(ok), ok)
})

test_that("not a list", {
  expect_snapshot(error = TRUE, {
    as_attributes(1:10)
  })
})

test_that("count limit", {
  withr::local_envvar(
    OTEL_ATTRIBUTE_COUNT_LIMIT = 5,
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT = NA_character_
  )

  lst <- list(a = 1, b = 2, c = 3, d = 4, e = 5)
  lst2 <- list(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  expect_equal(as_attributes(lst), lst)
  expect_equal(as_attributes(lst2), lst)
})

test_that("value length limit", {
  withr::local_envvar(
    OTEL_ATTRIBUTE_COUNT_LIMIT = NA_character_,
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT = 3
  )

  lst <- list(a = 1:3, b = as.character(1:3))
  lst2 <- list(a = 1:10, b = as.character(1:10))
  expect_equal(as_attributes(lst), lst)
  expect_equal(as_attributes(lst2), lst)
})

test_that("fix bad names", {
  withr::local_envvar(
    OTEL_ATTRIBUTE_COUNT_LIMIT = NA_character_,
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT = NA_character_
  )

  expect_equal(
    as_attributes(list(1, 2, 3)),
    list("1" = 1, "2" = 2, "3" = 3)
  )

  expect_equal(
    as_attributes(list(1, 2, 3)),
    list("1" = 1, "2" = 2, "3" = 3)
  )

  expect_equal(
    as_attributes(list(a = 1, 2, a = 3)),
    list(a = 1, "2" = 2, a.1 = 3)
  )
})

test_that("non-finite real values", {
  expect_equal(
    as_attributes(list(a = c(1, NA_real_))),
    list(a = c("1", "NA"))
  )
  expect_equal(
    as_attributes(list(a = c(1, NaN))),
    list(a = c("1", "NaN"))
  )
  expect_equal(
    as_attributes(list(a = c(1, Inf))),
    list(a = c("1", "Inf"))
  )
  expect_equal(
    as_attributes(list(a = c(1, -Inf))),
    list(a = c("1", "-Inf"))
  )
})

test_that("print unsupported types", {
  withr::local_envvar(
    OTEL_ATTRIBUTE_COUNT_LIMIT = NA_character_,
    OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT = NA_character_
  )

  expect_snapshot(
    as_attributes(list(mtcars[1:2, 1:4]))
  )
})
