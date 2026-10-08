# Check whether OpenTelemetry metrics collection is active

This is useful for avoiding computation when metrics collection is
inactive.

## Usage

``` r
is_measuring_enabled(meter = NULL)
```

## Arguments

- meter:

  Meter object
  ([otel_meter](https://otel.r-lib.org/reference/otel_meter.md)), or a
  meter name, the instrumentation scope, to pass to
  [`get_meter()`](https://otel.r-lib.org/reference/get_meter.md).

## Value

`TRUE` is OpenTelemetry metrics collection is active, `FALSE` otherwise.

## Details

It calls [`get_meter()`](https://otel.r-lib.org/reference/get_meter.md)
with `name` and then it calls the meter's `$is_enabled()` method.

## See also

Other OpenTelemetry metrics API:
[`counter_add()`](https://otel.r-lib.org/reference/counter_add.md),
[`gauge_record()`](https://otel.r-lib.org/reference/gauge_record.md),
[`histogram_record()`](https://otel.r-lib.org/reference/histogram_record.md),
[`up_down_counter_add()`](https://otel.r-lib.org/reference/up_down_counter_add.md)

## Examples

``` r
fun <- function() {
  if (otel::is_measuring_enabled()) {
    xattr <- calculate_some_extra_attributes()
    otel::counter_add("sessions", 1, attributes = xattr)
  }
  # ...
}
```
