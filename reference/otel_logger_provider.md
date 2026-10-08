# OpenTelemetry Logger Provider Object

otel_logger_provider -\>
[otel_logger](https://otel.r-lib.org/reference/otel_logger.md)

## Value

Not applicable.

## Details

The logger provider defines how logs are exported when collecting
telemetry data. It is unlikely that you need to use logger provider
objects directly.

Usually there is a single logger provider for an R app or script.

Typically the logger provider is created automatically, at the first
[`log()`](https://otel.r-lib.org/reference/log.md) call. otel decides
which logger provider class to use based on [Environment
Variables](https://otel.r-lib.org/reference/environmentvariables.md).

## Implementations

Note that this list is updated manually and may be incomplete.

- [logger_provider_noop](https://otel.r-lib.org/reference/logger_provider_noop.md):
  No-op logger provider, used when no logs are emitted.

- [otelsdk::logger_provider_file](https://otelsdk.r-lib.org/reference/logger_provider_file.html):
  Save logs to a JSONL file.

- [otelsdk::logger_provider_http](https://otelsdk.r-lib.org/reference/logger_provider_http.html):
  Send logs to a collector over HTTP/OTLP.

- [otelsdk::logger_provider_stdstream](https://otelsdk.r-lib.org/reference/logger_provider_stdstream.html):
  Write logs to standard output or error or to a file. Primarily for
  debugging.

## Methods

### `logger_provider$get_logger()`

Get or create a new logger object.

#### Usage

    logger_provider$get_logger(
     name = NULL,
     version = NULL,
     schema_url = NULL,
     attributes = NULL
    )

#### Arguments

- `name` Logger name. It makes sense to reuse the tracer name as the
  logger name. See
  [`get_logger()`](https://otel.r-lib.org/reference/get_logger.md) and
  [`default_tracer_name()`](https://otel.r-lib.org/reference/default_tracer_name.md).

- `version`: Optional. Specifies the version of the instrumentation
  scope if the scope has a version (e.g. R package version). Example
  value: `"1.0.0"`.

- `schema_url`: Optional. Specifies the Schema URL that should be
  recorded in the emitted telemetry.

- `attributes`: Optional. Specifies the instrumentation scope attributes
  to associate with emitted telemetry. See
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  for allowed values. You can also use
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  to convert R objects to OpenTelemetry attributes.

#### Value

An OpenTelemetry logger
([otel_logger](https://otel.r-lib.org/reference/otel_logger.md)) object.

#### See also

[`get_default_logger_provider()`](https://otel.r-lib.org/reference/get_default_logger_provider.md),
[`get_logger()`](https://otel.r-lib.org/reference/get_logger.md).

### `logger_provider$flush()`

Force any buffered logs to flush. Logger providers might not implement
this method.

#### Usage

    logger_provider$flush()

#### Value

Nothing.

## See also

Other low level logs API:
[`get_default_logger_provider()`](https://otel.r-lib.org/reference/get_default_logger_provider.md),
[`get_logger()`](https://otel.r-lib.org/reference/get_logger.md),
[`logger_provider_noop`](https://otel.r-lib.org/reference/logger_provider_noop.md),
[`otel_logger`](https://otel.r-lib.org/reference/otel_logger.md)

## Examples

``` r
lp <- otel::get_default_logger_provider()
lgr <- lp$get_logger()
lgr$is_enabled()
#> [1] FALSE
```
