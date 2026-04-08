# OpenTelemetry Histogram Object

[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)
-\> [otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md) -\>
[otel_counter](https://otel.r-lib.org/dev/reference/otel_counter.md),
[otel_up_down_counter](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md),
otel_histogram,
[otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_histogram objects directly.
[`histogram_record()`](https://otel.r-lib.org/dev/reference/histogram_record.md)
automatically sets up a meter and creates a histogram instrument, as
needed.

A histogram object is created by calling the `create_histogram()` method
of an
[`otel_meter_provider()`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md).

You can use the `record()` method to update the statistics with the
specified amount.

In R histogram values are represented by doubles.

## Methods

### `histogram$record()`

Update the statistics with the specified amount.

#### Usage

    histogram$record(value, attributes = NULL, span_context = NULL, ...)

#### Arguments

- `value`: A numeric value to record.

- `attributes`: Additional attributes to add.

- `span_context`: Span context. If missing, the active context is used,
  if any.

#### Value

The histogram object itself, invisibly.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_meter`](https://otel.r-lib.org/dev/reference/otel_meter.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
mp <- get_default_meter_provider()
mtr <- mp$get_meter()
hst <- mtr$create_histogram("response-time")
hst$record(1.123)
```
