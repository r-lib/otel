test_that("code formatting", {
  skip_on_cran()
  skip_on_covr()
  # always run locally, but on the CI only on macOS
  if (Sys.getenv("CI") != "" && Sys.info()[["sysname"]] != "Darwin") {
    skip("Only run code formatting check on macOS")
  }

  pkg <- test_path("../../")
  if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
    pkg <- file.path(pkg, "00_pkg_src", .packageName)
    file.copy(file.path(pkg, "inst", "air.toml"), pkg)
    on.exit(unlink(file.path(pkg, "air.toml")), add = TRUE)
  }

  if (Sys.which("air") == "") {
    stop("Could not find air installation")
  }

  expect_snapshot(invisible(processx::run(
    "air",
    c("format", "--check", pkg),
    echo = TRUE,
    error_on_status = FALSE
  )))
})
