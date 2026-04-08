# Log an OpenTelemetry log message

Log an OpenTelemetry log message

## Usage

``` r
log(msg, ..., severity = "info", logger = NULL)

log_trace(msg, ..., logger = NULL)

log_debug(msg, ..., logger = NULL)

log_info(msg, ..., logger = NULL)

log_warn(msg, ..., logger = NULL)

log_error(msg, ..., logger = NULL)

log_fatal(msg, ..., logger = NULL)
```

## Arguments

- msg:

  Log message.

- ...:

  Additional arguments are passed to the `$log()` method of the logger.

- severity:

  Log severity, a string, one of "trace", "trace2", "trace3", "trace4",
  "debug", "debug2", "debug3", "debug4", "info", "info2", "info3",
  "info4", "warn", "warn2", "warn3", "warn4", "error", "error2",
  "error3", "error4", "fatal", "fatal2", "fatal3", "fatal4".

- logger:

  Logger to use. If not an OpenTelemetry logger object
  ([otel_logger](https://otel.r-lib.org/dev/reference/otel_logger.md)),
  then it passed to
  [`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md)
  to get a logger.

## Value

The logger, invisibly.

## Details

`log_trace()` is the same as `log()` with `severity_level` "trace".

`log_debug()` is the same as `log()` with `severity_level` "debug".

`log_info()` is the same as `log()` with `severity_level` "info".

`log_warn()` is the same as `log()` with `severity_level` "warn".

`log_error)` is the same as `log()` with `severity_level` "error".

`log_fatal()` is the same as `log()` with `severity_level` "fatal".

## See also

Other OpenTelemetry logs API:
[`is_logging_enabled()`](https://otel.r-lib.org/dev/reference/is_logging_enabled.md),
[`log_severity_levels`](https://otel.r-lib.org/dev/reference/log_severity_levels.md)

## Examples

``` r
host <- "my.db.host"
port <- 6667
otel::log("Connecting to database at {host}:{port}")
```
