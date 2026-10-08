# Increase or decrease an OpenTelemetry up-down counter

Increase or decrease an OpenTelemetry up-down counter

## Usage

``` r
up_down_counter_add(
  name,
  value = 1L,
  attributes = NULL,
  context = NULL,
  meter = NULL
)
```

## Arguments

- name:

  Name of the up-down counter.

- value:

  Value to add to or subtract from the counter, defaults to 1.

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

The up-down counter object
([otel_up_down_counter](https://otel.r-lib.org/reference/otel_up_down_counter.md)),
invisibly.

## See also

Other OpenTelemetry metrics instruments:
[`counter_add()`](https://otel.r-lib.org/reference/counter_add.md),
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md)

Other OpenTelemetry metrics API:
[`counter_add()`](https://otel.r-lib.org/reference/counter_add.md),
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md),
[`is_measuring_enabled()`](https://otel.r-lib.org/reference/is_measuring_enabled.md)

## Examples

``` r
otel::up_down_counter_add("session-count", 1)
```
