# Check whether OpenTelemetry logging is active

This is useful for avoiding computation when logging is inactive.

## Usage

``` r
is_logging_enabled(severity = "info", logger = NULL)
```

## Arguments

- severity:

  Check if logs are emitted at this severity level.

- logger:

  Logger object
  ([otel_logger](https://otel.r-lib.org/dev/reference/otel_logger.md)),
  or a logger name, the instrumentation scope, to pass to
  [`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md).

## Value

`TRUE` is OpenTelemetry logging is active, `FALSE` otherwise.

## Details

It calls
[`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md)
with `name` and then it calls the logger's `$is_enabled()` method.

## See also

'[Environment
Variables](https://otel.r-lib.org/dev/reference/environmentvariables.md)'
needed to enable OpenTelemetry logging.

Other OpenTelemetry logs API:
[`log()`](https://otel.r-lib.org/dev/reference/log.md),
[`log_severity_levels`](https://otel.r-lib.org/dev/reference/log_severity_levels.md)

## Examples

``` r
fun <- function() {
  if (otel::is_logging_enabled()) {
    xattr <- calculate_some_extra_attributes()
    otel::log("Starting fun", attributes = xattr)
  }
  # ...
}
```
