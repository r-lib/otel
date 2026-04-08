# Returns the active span context

This is sometimes useful for logs or metrics, to associate logging and
metrics reporting with traces.

## Usage

``` r
get_active_span_context()
```

## Value

The active span context, an
[otel_span_context](https://otel.r-lib.org/dev/reference/otel_span_context.md)
object. If there is no active span context, then an invalid span context
is returned, i.e. `spc$is_valid()` will be `FALSE` for the returned
`spc`.

## Details

Note that logs and metrics instruments automatically use the current
span context, so often you don't need to call this function explicitly.

## Examples

``` r
fun <- function() {
  otel::start_local_active_span("fun")
  fun2()
}
fun2 <- function() {
  otel::log("Log message", span_context = otel::get_active_span_context())
}
fun()
```
