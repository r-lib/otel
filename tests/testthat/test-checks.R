test_that("is_string", {
  expect_true(is_string("foo"))
  expect_true(is_string(c(name = "foo")))

  expect_false(is_string(1))
  expect_false(is_string(letters))
  expect_false(is_string(NA_character_))
  expect_false(is_string(character()))
})

test_that("as_string", {
  expect_null(as_string(NULL))
  expect_equal(as_string("foo"), "foo")
  expect_equal(as_string(c(a = "1")), c(a = "1"))

  s1 <- 1
  s2 <- character()
  s3 <- letters[1:2]
  s4 <- NULL
  expect_snapshot(error = TRUE, {
    as_string(s1)
    as_string(s2)
    as_string(s3)
    as_string(s4, null = FALSE)
  })
})
