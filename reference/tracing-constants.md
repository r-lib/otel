# OpenTelemetry tracing constants

Various constants related OpenTelemetry tracing.

## Usage

``` r
invalid_trace_id

invalid_span_id

span_kinds

span_status_codes
```

## Value

Not applicable.

## Details

### `invalid_trace_id`

`invalid_trace_id` is a string scalar, an invalid trace id. If there is
no active span, then
[`get_active_span_context()`](https://otel.r-lib.org/reference/get_active_span_context.md)
returns a span context that has an invalid trace id.

### `invalid_span_id`

`invalid_span_id` is a string scalar, an invalid span id. If there is no
active span, then
[`get_active_span_context()`](https://otel.r-lib.org/reference/get_active_span_context.md)
returns a span context that has an invalid span id.

### `span_kinds`

`span_kinds` is a character vector listing all possible span kinds. See
the [OpenTelemetry
specification](https://opentelemetry.io/docs/specs/otel/trace/api/#spankind)
for when to use which.

### `span_status_codes`

`span_status_codes` is a character vector listing all possible span
status codes. You can set the status code of a a span with the
`set_status()` method of
[otel_span](https://otel.r-lib.org/reference/otel_span.md) objects. If
not set explicitly, and the span is ended automatically (by
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md),
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md)),
then otel sets the status automatically to "ok" or "error", depending on
whether the span ended during handling an error.

## See also

Other OpenTelemetry trace API:
[`Zero Code Instrumentation`](https://otel.r-lib.org/reference/zci.md),
[`end_span()`](https://otel.r-lib.org/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/reference/start_span.md),
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md)

## Examples

``` r
invalid_trace_id
#> [1] "00000000000000000000000000000000"
invalid_span_id
#> [1] "0000000000000000"
span_kinds
#>    default                                             
#> "internal"   "server"   "client" "producer" "consumer" 
span_status_codes
#> default                 
#> "unset"    "ok" "error" 
```
