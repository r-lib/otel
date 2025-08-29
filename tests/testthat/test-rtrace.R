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

  skip_on_covr()
  expect_snapshot(
    {
      print(env$f, useSource = FALSE)
      env$obj
    },
    transform = transform_env_address,
    variant = if (Sys.getenv("TESTTHAT_COVERAGE") == "otel") "cov" else "nocov"
  )
})

test_that("integration test", {
  skip_on_cran()
  skip_on_covr()

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  mkdirp(lib)
  install.packages(
    test_path("fixtures/oteltest"),
    lib = lib,
    repos = NULL,
    type = "source",
    quiet = TRUE,
    INSTALL_opts = c("--no-staged-install", "--no-test-load")
  )

  withr::local_libpaths(lib, action = "prefix")
  script <- system.file("runme.R", package = "oteltest")

  # test w/o instrumenting
  env_wo <- c(callr::rcmd_safe_env(), OTEL_TRACES_EXPORTER = "none")
  expect_snapshot({
    callr::rscript(script, env = env_wo)
  })

  # w/ ZCI
  traces <- tempfile(fileext = ".jsonl")
  on.exit(unlink(traces), add = TRUE)
  env <- c(
    callr::rcmd_safe_env(),
    OTEL_TRACES_EXPORTER = "otlp/file",
    OTEL_EXPORTER_OTLP_TRACES_FILE = traces,
    OTEL_R_INSTRUMENT_PKGS = "oteltest",
    OTEL_ENV = "dev"
  )
  expect_snapshot({
    callr::rscript(script, env = env)
  })

  expect_true(file.exists(traces))
  lns <- readLines(traces)
  spns <- lapply(lns, jsonlite::fromJSON, simplifyVector = FALSE)
  spns <- lapply(spns, function(x) {
    x$resourceSpans[[1]]$scopeSpans[[1]]$spans[[1]]
  })

  # create a tree of spans
  ids <- map_chr(spns, "[[", "spanId")
  prns <- map_chr(spns, function(x) x$parentSpanId %||% NA_character_)
  children <- tapply(ids, prns, c, simplify = FALSE)
  spnstree <- data.frame(
    id = ids,
    children = I(unname(children[ids])),
    label = map_chr(spns, "[[", "name")
  )
  root <- ids[is.na(prns)][1]

  expect_snapshot({
    cli::tree(spnstree, root = root)
  })
})
