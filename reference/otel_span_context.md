# An OpenTelemetry Span Context object

[otel_tracer_provider](https://otel.r-lib.org/reference/otel_tracer_provider.md)
-\> [otel_tracer](https://otel.r-lib.org/reference/otel_tracer.md) -\>
[otel_span](https://otel.r-lib.org/reference/otel_span.md) -\>
otel_span_context

## Value

Not applicable.

## Details

This is a representation of a span that can be serialized, copied to
other processes, and it can be used to create new child spans.

## Methods

### `span_context$get_span_id()`

Get the id of the span.

#### Usage

    span_context$get_span_id()

#### Value

String scalar, a span id. For invalid spans it is
[invalid_span_id](https://otel.r-lib.org/reference/tracing-constants.md).

### `span_context$get_trace_flags()`

Get the trace flags of a span.

See the
[specification](https://w3c.github.io/trace-context/#trace-flags) for
more details on trace flags.

#### Usage

    span_context$get_trace_flags()

#### Value

A list with entries:

- `is_sampled`: logical flag, whether the trace of the span is sampled.
  If `FALSE` then the caller is not recording the trace. See details in
  the
  [specification](https://w3c.github.io/trace-context/#sampled-flag).

- `is_random`: logical flag, it specifies how trace ids are generated.
  See details in the
  [specification](https://w3c.github.io/trace-context/#random-trace-id-flag).

### `span_context$get_trace_id()`

Get the id of the trace the span belongs to.

#### Usage

    span_context$get_trace_id()

#### Value

A string scalar, a trace id. For invalid spans it is
[invalid_trace_id](https://otel.r-lib.org/reference/tracing-constants.md).

### `span_context$is_remote()`

Whether the span was propagated from a remote parent.

#### Usage

    span_context$is_remote()

#### Value

A logical scalar.

### `span_context$is_sampled()`

Whether the span is sampled. This is the same as the `is_sampled` trace
flags, see `get_trace_flags()` above.

#### Usage

    span_context$is_sampled()

#### Value

Logical scalar.

### `span_context$is_valid()`

Whether the span is valid. Sometimes otel functions return an invalid
span or a span context referring to an invalid span. E.g.
[`get_active_span_context()`](https://otel.r-lib.org/reference/get_active_span_context.md)
does that if there is no active span.

`is_valid()` checks if the span is valid.

An span id of an invalid span is
[invalid_span_id](https://otel.r-lib.org/reference/tracing-constants.md).

#### Usage

    span_context$is_valid()

#### Value

A logical scalar.

### `span_context$to_http_headers()`

Serialize the span context into one or more HTTP headers that can be
transmitted to other processes or servers, to create a distributed
trace.

The other process can deserialize these headers into a span context that
can be used to create new remote spans.

#### Usage

    span_context$to_http_headers()

#### Value

A named character vector, the HTTP header representation of the span
context. Usually includes a `traceparent` header. May include other
headers.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/reference/get_tracer.md),
[`otel_span`](https://otel.r-lib.org/reference/otel_span.md),
[`otel_tracer`](https://otel.r-lib.org/reference/otel_tracer.md),
[`otel_tracer_provider`](https://otel.r-lib.org/reference/otel_tracer_provider.md),
[`tracer_provider_noop`](https://otel.r-lib.org/reference/tracer_provider_noop.md)

## Examples

``` r
spc <- get_active_span_context()
spc$get_trace_flags()
#> list()
spc$get_trace_id()
#> [1] "00000000000000000000000000000000"
spc$get_span_id()
#> [1] "0000000000000000"
spc$is_remote()
#> [1] FALSE
spc$is_sampled()
#> [1] FALSE
spc$is_valid()
#> [1] FALSE
spc$to_http_headers()
#> named character(0)
```
