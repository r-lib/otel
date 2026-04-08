# Returns the active span, if any

This is sometimes useful, to add additional attributes or links to the
currently active span.

## Usage

``` r
get_active_span()
```

## Value

The active span, an
[otel_span](https://otel.r-lib.org/dev/reference/otel_span.md) object,
if any, or an invalid span if there is no active span.

## Examples

``` r
fun <- function() {
  otel::start_local_active_span("fun")
  spn <- otel::get_active_span()
  spn$set_attribute("key", "attribute-value")
}
fun()
```
