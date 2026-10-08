# code formatting

    Code
      invisible(processx::run("air", c("format", "--check", pkg), echo = TRUE,
      error_on_status = FALSE))

