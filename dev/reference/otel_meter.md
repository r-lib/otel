# OpenTelemetry Meter Object

[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md)
-\> otel_meter -\>
[otel_counter](https://otel.r-lib.org/dev/reference/otel_counter.md),
[otel_up_down_counter](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md),
[otel_histogram](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md)

## Value

Not applicable.

## Details

Usually you do not need to deal with otel_meter objects directly.
[`counter_add()`](https://otel.r-lib.org/dev/reference/counter_add.md),
[`up_down_counter_add()`](https://otel.r-lib.org/dev/reference/up_down_counter_add.md),
[`histogram_record()`](https://otel.r-lib.org/dev/reference/histogram_record.md)
and
[`gauge_record()`](https://otel.r-lib.org/dev/reference/gauge_record.md)
automatically set up the meter and uses it to create instruments.

A meter object is created by calling the
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md)
method of an
[otel_meter_provider](https://otel.r-lib.org/dev/reference/otel_meter_provider.md).

You can use the `create_counter()`, `create_up_down_counter()`,
`create_histogram()`, `create_gauge()` methods of the meter object to
create instruments.

Typically there is a separate meter object for each instrumented R
package.

## Methods

### `meter$is_enabled()`

Whether the meter is active and emitting measurements.

This is equivalent to the
[`is_measuring_enabled()`](https://otel.r-lib.org/dev/reference/is_measuring_enabled.md)
function.

#### Usage

    meter$is_enabled()

#### Value

Logical scalar.

### `meter$create_counter()`

Create a new [counter
instrument](https://opentelemetry.io/docs/specs/otel/metrics/api/#counter).

#### Usage

    create_counter(name, description = NULL, unit = NULL)

#### Arguments

- `name`: Name of the instrument.

- `description`: Optional description.

- `unit`: Optional measurement unit. If specified, it should use units
  from [Unified Code for Units of Measure](https://ucum.org/), according
  to the [OpenTelemetry semantic
  conventions](https://opentelemetry.io/docs/specs/semconv/general/metrics/#instrument-units).

#### Value

An OpenTelemetry counter
([otel_counter](https://otel.r-lib.org/dev/reference/otel_counter.md))
object.

### `meter$create_up_down_counter()`

Create a new [up-down counter
instrument](https://opentelemetry.io/docs/specs/otel/metrics/api/#updowncounter).

#### Usage

    create_up_down_counter(name, description = NULL, unit = NULL)

#### Arguments

- `name`: Name of the instrument.

- `description`: Optional description.

- `unit`: Optional measurement unit. If specified, it should use units
  from [Unified Code for Units of Measure](https://ucum.org/), according
  to the [OpenTelemetry semantic
  conventions](https://opentelemetry.io/docs/specs/semconv/general/metrics/#instrument-units).

#### Value

An OpenTelemetry counter
([otel_up_down_counter](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md))
object.

### `meter$create_histogram()`

Create a new
[histogram](https://opentelemetry.io/docs/specs/otel/metrics/api/#histogram).

#### Usage

    create_histogram(name, description = NULL, unit = NULL)

#### Arguments

- `name`: Name of the instrument.

- `description`: Optional description.

- `unit`: Optional measurement unit. If specified, it should use units
  from [Unified Code for Units of Measure](https://ucum.org/), according
  to the [OpenTelemetry semantic
  conventions](https://opentelemetry.io/docs/specs/semconv/general/metrics/#instrument-units).

#### Value

An OpenTelemetry histogram
([otel_histogram](https://otel.r-lib.org/dev/reference/otel_histogram.md))
object.

### `meter$create_gauge()`

Create a new
[gauge](https://opentelemetry.io/docs/specs/otel/metrics/api/#gauge).

#### Usage

    create_gauge(name, description = NULL, unit = NULL)

#### Arguments

- `name`: Name of the instrument.

- `description`: Optional description.

- `unit`: Optional measurement unit. If specified, it should use units
  from [Unified Code for Units of Measure](https://ucum.org/), according
  to the [OpenTelemetry semantic
  conventions](https://opentelemetry.io/docs/specs/semconv/general/metrics/#instrument-units).

#### Value

An OpenTelemetry gauge
([otel_gauge](https://otel.r-lib.org/dev/reference/otel_gauge.md))
object.

## See also

Other low level metrics API:
[`get_default_meter_provider()`](https://otel.r-lib.org/dev/reference/get_default_meter_provider.md),
[`get_meter()`](https://otel.r-lib.org/dev/reference/get_meter.md),
[`meter_provider_noop`](https://otel.r-lib.org/dev/reference/meter_provider_noop.md),
[`otel_counter`](https://otel.r-lib.org/dev/reference/otel_counter.md),
[`otel_gauge`](https://otel.r-lib.org/dev/reference/otel_gauge.md),
[`otel_histogram`](https://otel.r-lib.org/dev/reference/otel_histogram.md),
[`otel_meter_provider`](https://otel.r-lib.org/dev/reference/otel_meter_provider.md),
[`otel_up_down_counter`](https://otel.r-lib.org/dev/reference/otel_up_down_counter.md)

## Examples

``` r
mp <- get_default_meter_provider()
mtr <- mp$get_meter()
ctr <- mtr$create_counter("session")
ctr$add(1)
```
