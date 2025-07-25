test_that("dev API is up to date", {
  testthat::skip_on_covr()
  skip_on_cran()
  tmp <- tempfile()
  tmp2 <- tempfile()
  on.exit(unlink(c(tmp, tmp2)), add = TRUE)
  withr::local_envvar(OTEL_DEV_API_OUTPUT_FILE = tmp)

  pkg <- test_pkg_root()
  withr::local_dir(pkg)

  # this does not work in the installed package
  if (!file.exists("tools/template/dev.R")) {
    testthat::skip("does not work in installed package")
  }

  callr::rscript("tools/template/dev.R", show = FALSE)
  crt <- readLines(file.path("R", "api-dev.R"), warn = FALSE)
  upd <- readLines(tmp)
  expect_equal(upd, crt)
})
