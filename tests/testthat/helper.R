test_pkg_root <- function() {
  pkg <- test_path("../../")
  if (!file.exists(file.path(pkg, "DESCRIPTION"))) {
    pkg <- file.path(pkg, "00_pkg_src", .packageName)
  }
  pkg
}
