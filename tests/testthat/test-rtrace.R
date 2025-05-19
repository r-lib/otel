test_that("trace_namespace", {
  loadNamespace("cli")
  fake(trace_namespace, "trace_env", function(...) res <<- list(...))
  res <- NULL
  expect_snapshot(trace_namespace("otel"))
})

test_that("trace_env", {
  env <- new.env(parent = emptyenv())
  env$f <- function() "dummy"
  env$obj <- "not-a-function"
  trace_env(env, "pkg")

  expect_snapshot(
    {
      print(env$f, useSource = FALSE)
      env$obj
    },
    transform = transform_env_address
  )
})
