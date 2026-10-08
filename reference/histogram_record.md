# Record a value of an OpenTelemetry histogram

Record a value of an OpenTelemetry histogram

## Usage

``` r
histogram_record(name, value, attributes = NULL, context = NULL, meter = NULL)
```

## Arguments

- name:

  Name of the histogram.

- value:

  Value to record.

- attributes:

  Additional attributes to add.

- context:

  Span context. If missing the active context is used, if any.

- meter:

  Meter object
  ([otel_meter](https://otel.r-lib.org/reference/otel_meter.md)).
  Otherwise it is passed to
  [`get_meter()`](https://otel.r-lib.org/reference/get_meter.md) to get
  a meter.

## Value

The histogram object
([otel_histogram](https://otel.r-lib.org/reference/otel_histogram.md)),
invisibly.

## See also

Other OpenTelemetry metrics instruments:
[`counter_add()`](https://otel.r-lib.org/reference/counter_add.md),
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)

Other OpenTelemetry metrics API:
[`counter_add()`](https://otel.r-lib.org/reference/counter_add.md),
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`is_measuring_enabled()`](https://otel.r-lib.org/reference/is_measuring_enabled.md),
[`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)

## Examples

``` r
otel::histogram_record("response-time", 0.2)
```
