# OpenTelemetry Counter Object

[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)
-\> [otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md) -\>
otel_counter,
[otel_up_down_counter](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md),
[otel_histogram](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_counter objects directly.
[`counter_add()`](https://otel.r-lib.org/dev/reference/counter_add.md)
automatically sets up a meter and creates a counter instrument, as
needed.

A counter object is created by calling the `create_counter()` method of
an
[`otel_meter_provider()`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md).

You can use the `add()` method to increment the counter by a positive
amount.

In R counters are represented by double values.

## Methods

### `counter$add()`

Increment the counter by a fixed amount.

#### Usage

    counter$add(value, attributes = NULL, span_context = NULL, ...)

#### Arguments

- `value`: Value to increment the counter with.

- `attributes`: Additional attributes to add.

- `span_context`: Span context. If missing, the active context is used,
  if any.

#### Value

The counter object itself, invisibly.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
mp <- get_default_meter_provider()
mtr <- mp$get_meter()
ctr <- mtr$create_counter("session")
ctr$add(1)
```
