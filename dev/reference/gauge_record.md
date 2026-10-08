# Record a value of an OpenTelemetry gauge

Record a value of an OpenTelemetry gauge

## Usage

``` r
gauge_record(name, value, attributes = NULL, context = NULL, meter = NULL)
```

## Arguments

- name:

  Name of the gauge

- value:

  Value to record.

- attributes:

  Additional attributes to add.

- context:

  Span context. If missing the active context is used, if any.

- meter:

  Meter object
  ([otel_meter](https://otel.r-lib.org/dev/reference/otel_meter.md)).
  Otherwise it is passed to
  [`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md) to
  get a meter.

## Value

The gauge object
([otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)),
invisibly.

## See also

'[Environment
Variables](https://otel.r-lib.org/dev/reference/environmentvariables.md)'
needed to enable OpenTelemetry recording.

Other OpenTelemetry metrics instruments:
[`counter_add()`](https://otel.r-lib.org/dev/reference/counter_add.md),
[`histogram_record()`](https://otel.r-lib.org/dev/reference/histogram_record.md),
[`up_down_counter_add()`](https://otel.r-lib.org/dev/reference/up_down_counter_add.md)

Other OpenTelemetry metrics API:
[`counter_add()`](https://otel.r-lib.org/dev/reference/counter_add.md),
[`histogram_record()`](https://otel.r-lib.org/dev/reference/histogram_record.md),
[`is_measuring_enabled()`](https://otel.r-lib.org/dev/reference/is_measuring_enabled.md),
[`up_down_counter_add()`](https://otel.r-lib.org/dev/reference/up_down_counter_add.md)

## Examples

``` r
otel::gauge_record("temperature", 27)
```
