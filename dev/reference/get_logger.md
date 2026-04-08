# Get a logger from the default logger provider

Get a logger from the default logger provider

## Usage

``` r
get_logger(
  name = NULL,
  minimum_severity = NULL,
  version = NULL,
  schema_url = NULL,
  attributes = NULL,
  ...,
  provider = NULL
)
```

## Arguments

- name:

  Name of the new tracer. If missing, then deduced automatically.

- minimum_severity:

  A log level, the minimum severity log messages to log. See
  [log_severity_levels](https://otel.r-lib.org/dev/reference/log_severity_levels.md).

- version:

  Optional. Specifies the version of the instrumentation scope if the
  scope has a version (e.g. R package version). Example value:
  `"1.0.0"`.

- schema_url:

  Optional. Specifies the Schema URL that should be recorded in the
  emitted telemetry.

- attributes:

  Optional. Specifies the instrumentation scope attributes to associate
  with emitted telemetry.

- ...:

  Additional arguments are passed to the `get_logger()` method of the
  provider.

- provider:

  Tracer provider to use. If `NULL`, then it uses
  [`get_default_tracer_provider()`](https://otel.r-lib.org/dev/reference/get_default_tracer_provider.md)
  to get a tracer provider.

## Value

An [otel_logger](https://otel.r-lib.org/dev/reference/otel_logger.md)
object.

## See also

Other low level logs API:
[`get_default_logger_provider()`](https://otel.r-lib.org/dev/reference/get_default_logger_provider.md),
[`logger_provider_noop`](https://otel.r-lib.org/dev/reference/logger_provider_noop.md),
[`otel_logger`](https://otel.r-lib.org/dev/reference/otel_logger.md),
[`otel_logger_provider`](https://otel.r-lib.org/dev/reference/otel_logger_provider.md)

## Examples

``` r
myfun <- function() {
  lgr <- otel::get_logger()
  otel::log("Log message", logger = lgr)
}
myfun()
```
