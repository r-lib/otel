# OpenTelemetry Up-Down Counter Object

[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)
-\> [otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md) -\>
[otel_counter](https://otel.r-lib.org/dev/reference/otel_counter.md),
otel_up_down_counter,
[otel_histogram](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_up_down_counter objects
directly.
[`up_down_counter_add()`](https://otel.r-lib.org/dev/reference/up_down_counter_add.md)
automatically sets up a meter and creates an up-down counter instrument,
as needed.

An up-down counter object is created by calling the
`create_up_down_counter()` method of an
[`otel_meter_provider()`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md).

You can use the `add()` method to increment or decrement the counter.

In R up-down counters are represented by double values.

## Methods

### `up_down_counter$add()`

Increment or decrement the up-down counter by a fixed amount.

#### Usage

    up_down_counter$add(value, attributes = NULL, span_context = NULL, ...)

#### Arguments

- `value`: Value to increment of decrement the up-down counter with.

- `attributes`: Additional attributes to add.

- `span_context`: Span context. If missing, the active context is used,
  if any.

#### Value

The up-down counter object itself, invisibly.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)

## Examples

``` r
mp <- get_default_meter_provider()
mtr <- mp$get_meter()
ctr <- mtr$create_up_down_counter("session")
ctr$add(1)
```
