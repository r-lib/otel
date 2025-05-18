output_file <- Sys.getenv("OTEL_DEV_API_OUTPUT_FILE", "R/api-dev.R")

r_files <- dir("R")
all_lines <- character()
for (r_file in r_files) {
  lns <- readLines(file.path("R", r_file), warn = FALSE)
  if (!any(grepl("^# safe start", lns))) next
  cli::cli_alert_info("Processing {.file {r_file}}.")
  stts <- grep("^# safe start", lns)
  ends <- grep("^# safe end", lns)
  if (
    length(stts) != length(ends) ||
      any(stts >= ends)
  ) {
    stop("Invalid #safe markers in file ", r_file)
  }

  for (idx in seq_along(stts)) {
    fnlns <- lns[(stts[idx] + 1):(ends[idx] - 1)]
    fnlns[1] <- sub(
      " <- function",
      "_dev <- function",
      fnlns[1],
      fixed = TRUE
    )
    fnlns <- grep("# safe$", fnlns, invert = TRUE, value = TRUE)
    all_lines <- c(all_lines, "", fnlns)
  }
}

writeLines(all_lines, output_file)
