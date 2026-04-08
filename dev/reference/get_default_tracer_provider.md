# Get the default tracer provider

The tracer provider defines how traces are exported when collecting
telemetry data. It is unlikely that you need to call this function
directly, but read on to learn how to configure which exporter to use.

## Usage

``` r
get_default_tracer_provider()
```

## Value

The default tracer provider, an
[otel_tracer_provider](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md)
object. See
[otel_tracer_provider](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md)
for its methods.

## Details

If there is no default set currently, then it creates and sets a
default.

The default tracer provider is created based on the
OTEL_R_TRACES_EXPORTER environment variable. This environment variable
is specifically for R applications with OpenTelemetry support.

If this is not set, then the generic OTEL_TRACES_EXPORTER environment
variable is used. This applies to all applications that support
OpenTelemetry and use the OpenTelemetry SDK.

The following values are allowed:

- `none`: no traces are exported.

- `stdout` or `console`: uses
  [otelsdk::tracer_provider_stdstream](https://otelsdk.r-lib.org/reference/tracer_provider_stdstream.html),
  to write traces to the standard output.

- `stderr`: uses
  [otelsdk::tracer_provider_stdstream](https://otelsdk.r-lib.org/reference/tracer_provider_stdstream.html),
  to write traces to the standard error.

- `http` or `otlp`: uses
  [otelsdk::tracer_provider_http](https://otelsdk.r-lib.org/reference/tracer_provider_http.html),
  to send traces through HTTP, using the OpenTelemetry Protocol (OTLP).

- `otlp/file` uses
  [otelsdk::tracer_provider_file](https://otelsdk.r-lib.org/reference/tracer_provider_file.html)
  to write traces to a JSONL file.

- `<package>::<provider>`: will select the `<provider>` object from the
  `<package>` package to use as a tracer provider. It calls
  `<package>::<provider>$new()` to create the new tracer provider. If
  this fails for some reason, e.g. the package is not installed, then it
  throws an error.

## See also

Other low level trace API:
[`get_tracer()`](https://otel.r-lib.org/dev/reference/get_tracer.md),
[`otel_span`](https://otel.r-lib.org/dev/reference/otel_span.md),
[`otel_span_context`](https://otel.r-lib.org/dev/reference/otel_span_context.md),
[`otel_tracer`](https://otel.r-lib.org/dev/reference/otel_tracer.md),
[`otel_tracer_provider`](https://otel.r-lib.org/dev/reference/otel_tracer_provider.md),
[`tracer_provider_noop`](https://otel.r-lib.org/dev/reference/tracer_provider_noop.md)

## Examples

``` r
get_default_tracer_provider()
#> <otel_tracer_provider_noop/otel_tracer_provider>
#> methods:
#>   get_tracer(name, version, schema_url, attributes)
#>   flush()
#>   get_spans()
```
