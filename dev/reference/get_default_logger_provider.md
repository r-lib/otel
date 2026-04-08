# Get the default logger provider

The logger provider defines how logs are exported when collecting
telemetry data. It is unlikely that you need to call this function
directly, but read on to learn how to configure which exporter to use.

## Usage

``` r
get_default_logger_provider()
```

## Value

The default logger provider, an
[otel_logger_provider](https://otel.r-lib.org/dev/reference/otel_logger_provider.md)
object.

## Details

If there is no default set currently, then it creates and sets a
default.

The default logger provider is created based on the OTEL_R_LOGS_EXPORTER
environment variable. This environment variable is specifically for R
applications with OpenTelemetry support.

If this is not set, then the generic OTEL_LOGS_EXPORTER environment
variable is used. This applies to all applications that support
OpenTelemetry and use the OpenTelemetry SDK.

The following values are allowed:

- `none`: no traces are exported.

- `stdout` or `console`: uses
  [otelsdk::logger_provider_stdstream](https://otelsdk.r-lib.org/reference/logger_provider_stdstream.html),
  to write traces to the standard output.

- `stderr`: uses
  [otelsdk::logger_provider_stdstream](https://otelsdk.r-lib.org/reference/logger_provider_stdstream.html),
  to write traces to the standard error.

- `http` or `otlp`: uses
  [otelsdk::logger_provider_http](https://otelsdk.r-lib.org/reference/logger_provider_http.html),
  to send traces through HTTP, using the OpenTelemetry Protocol (OTLP).

- `otlp/file` uses
  [otelsdk::logger_provider_file](https://otelsdk.r-lib.org/reference/logger_provider_file.html)
  to write logs to a JSONL file.

- `<package>::<provider>`: will select the `<provider>` object from the
  `<package>` package to use as a logger provider. It calls
  `<package>::<provider>$new()` to create the new logger provider. If
  this fails for some reason, e.g. the package is not installed, then it
  throws an error.

## See also

Other low level logs API:
[`get_logger()`](https://otel.r-lib.org/dev/reference/get_logger.md),
[`logger_provider_noop`](https://otel.r-lib.org/dev/reference/logger_provider_noop.md),
[`otel_logger`](https://otel.r-lib.org/dev/reference/otel_logger.md),
[`otel_logger_provider`](https://otel.r-lib.org/dev/reference/otel_logger_provider.md)

## Examples

``` r
get_default_logger_provider()
#> <otel_logger_provider_noop/otel_logger_provider>
#> methods:
#>   get_logger(name, minimum_severity, version, schema_url, attributes)
#>   flush()
```
