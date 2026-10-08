# OpenTelemetry Gauge Object

[otel_meter_provider](https://otel.r-lib.org/reference/otel_meter_provider.md)
-\> [otel_meter](https://otel.r-lib.org/reference/otel_meter.md) -\>
[otel_counter](https://otel.r-lib.org/reference/otel_counter.md),
[otel_up_down_counter](https://otel.r-lib.org/reference/otel_up_down_counter.md),
[otel_histogram](https://otel.r-lib.org/reference/otel_histogram.md),
otel_gauge

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_gauge objects directly.
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md)
automatically sets up a meter and creates a gauge instrument, as needed.

A gauge object is created by calling the `create_gauge()` method of an
[`otel_meter_provider()`](https://otel.r-lib.org/reference/otel_meter_provider.md).

You can use the `record()` method to record the current value.

In R gauge values are represented by doubles.

## Methods

### `gauge$record()`

Update the statistics with the specified amount.

#### Usage

    gauge$record(value, attributes = NULL, span_context = NULL, ...)

#### Arguments

- `value`: A numeric value. The current absolute value.

- `attributes`: Additional attributes to add.

- `span_context`: Span context. If missing, the active context is used,
  if any.

#### Value

The gauge object itself, invisibly.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/reference/otel_counter.md),
[`otel_histogram`](https://otel.r-lib.org/reference/otel_histogram.md),
[`otel_meter`](https://otel.r-lib.org/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/reference/otel_up_down_counter.md)

## Examples

``` r
mp <- get_default_meter_provider()
mtr <- mp$get_meter()
gge <- mtr$create_gauge("response-time")
gge$record(1.123)
```
