test_that("safe API is up to date", {
  testthat::skip_on_covr()
  tmp <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp, tmp2)), add = TRUE)
  withr::local_envvar(OTEL_SAFE_API_OUTPUT_FILE = tmp)

  pkg <- test_pkg_root()
  withr::local_dir(pkg)

  # this does not work in the installed package
  if (!file.exists("tools/template/safe.R")) {
    testthat::skip("does not work in installed package")
  }

  callr::rscript("tools/template/safe.R", show = FALSE)
  crt <- readLines(file.path("R", "api-safe.R"), warn = FALSE)
  upd <- readLines(tmp)
  expect_equal(upd, crt)
})
