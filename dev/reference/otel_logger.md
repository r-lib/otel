# OpenTelemetry Logger Object

[otel_logger_provider](https://otel.r-lib.org/dev/reference/otel_logger_provider.md)
-\> otel_logger

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_logger objects directly.
[`log()`](https://otel.r-lib.org/dev/reference/log.md) automatically
sets up the logger for emitting the logs.

A logger object is created by calling the
[`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md)
method of an
[otel_logger_provider](https://otel.r-lib.org/dev/reference/otel_logger_provider.md).

You can use the [`log()`](https://otel.r-lib.org/dev/reference/log.md)
method of the logger object to emit logs.

Typically there is a separate logger object for each instrumented R
package.

## Methods

### `logger$is_enabled()`

Whether the logger is active and emitting logs at a certain severity
level.

This is equivalent to the
[`is_logging_enabled()`](https://otel.r-lib.org/dev/reference/is_logging_enabled.md)
function.

#### Usage

    logger$is_enabled(severity = "info", event_id = NULL)

#### Arguments

- `severity`: Check if logs are emitted at this severity level.

- `event_id`: Not implemented yet.

#### Value

Logical scalar.

### `logger$get_minimum_severity()`

Get the current minimum severity at which the logger is emitting logs.

#### Usage

    logger_get_minimum_severity()

#### Value

Named integer scalar.

### `logger$set_minimum_severiry()`

Set the minimum severity for emitting logs.

#### Usage

    logger$set_minimum_severity(minimum_severity)

#### Arguments

- `minimum_severity`: Log severity, a string, one of "trace", "trace2",
  "trace3", "trace4", "debug", "debug2", "debug3", "debug4", "info",
  "info2", "info3", "info4", "warn", "warn2", "warn3", "warn4", "error",
  "error2", "error3", "error4", "fatal", "fatal2", "fatal3", "fatal4".

#### Value

Nothing.

### `logger$log()`

Log an OpenTelemetry log message.

#### Usage

    logger$log(
      msg = "",
      severity = "info",
      span_context = NULL,
      span_id = NULL,
      trace_id = NULL,
      trace_flags = NULL,
      timestamp = SYs.time(),
      observed_timestamp = NULL,
      attributes = NULL,
      .envir = parent.frame()
    )

#### Arguments

- `msg`: Log message, may contain R expressions to evaluate within
  braces.

- `severity`: Log severity, a string, one of "trace", "trace2",
  "trace3", "trace4", "debug", "debug2", "debug3", "debug4", "info",
  "info2", "info3", "info4", "warn", "warn2", "warn3", "warn4", "error",
  "error2", "error3", "error4", "fatal", "fatal2", "fatal3", "fatal4".

- `span_context`: An
  [otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)
  object to associate the log message with a span.

- `span_id`: Alternatively to `span_context`, you can also specify
  `span_id`, `trace_id` and `trace_flags` to associate a log message
  with a span.

- `trace_id`: Alternatively to `span_context`, you can also specify
  `span_id`, `trace_id` and `trace_flags` to associate a log message
  with a span.

- `trace_flags`: Alternatively to `span_context`, you can also specify
  `span_id`, `trace_id` and `trace_flags` to associate a log message
  with a span.

- `timestamp`: Time stamp, defaults to the current time. This is the
  time the logged event occurred.

- `observed_timestamp`: Observed time stamp, this is the time the event
  was observed.

- `attributes`: Optional attributes, see
  [`as_attributes()`](https://otel.r-lib.org/dev/reference/as_attributes.md)
  for the possible values.

#### Value

The logger object, invisibly.

### `logger$trace()`

The same as `logger$log()`, with `severity = "trace"`.

### `logger$debug()`

The same as `logger$log()`, with `severity = "debug"`.

### `logger$info()`

The same as `logger$log()`, with `severity = "info"`.

### `logger$warn()`

The same as `logger$log()`, with `severity = "warn"`.

### `logger$error()`

The same as `logger$log()`, with `severity = "error"`.

### `logger$fatal()`

The same as `logger$log()`, with `severity = "fatal"`.

## See also

Other low level logs API:
[`get_default_logger_provider()`](https://otel.r-lib.org/dev/reference/get_default_logger_provider.md),
[`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md),
[`logger_provider_noop`](https://otel.r-lib.org/dev/reference/logger_provider_noop.md),
[`otel_logger_provider`](https://otel.r-lib.org/dev/reference/otel_logger_provider.md)

## Examples

``` r
lp <- get_default_logger_provider()
lgr <- lp$get_logger()
platform <- utils::sessionInfo()$platform
lgr$log("This is a log message from {platform}.", severity = "trace")
```
