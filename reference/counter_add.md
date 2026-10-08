# Increase an OpenTelemetry counter

Increase an OpenTelemetry counter

## Usage

``` r
counter_add(name, value = 1L, attributes = NULL, context = NULL, meter = NULL)
```

## Arguments

- name:

  Name of the counter.

- value:

  Value to add to the counter, defaults to 1.

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

The counter object
([otel_counter](https://otel.r-lib.org/reference/otel_counter.md)),
invisibly.

## See also

Other OpenTelemetry metrics instruments:
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md),
[`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)

Other OpenTelemetry metrics API:
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md),
[`is_measuring_enabled()`](https://otel.r-lib.org/reference/is_measuring_enabled.md),
[`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)

## Examples

``` r
otel::counter_add("total-session-count", 1)
```
