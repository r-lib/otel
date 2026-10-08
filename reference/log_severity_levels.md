# OpenTelemetry log severity levels

A named integer vector, the severity levels in numeric form. The names
are the severity levels in text form. otel functions accept both forms
as severity levels, but the text form is more readable.

## Value

Not applicable.

## See also

Other OpenTelemetry logs API:
[`is_logging_enabled()`](https://otel.r-lib.org/reference/is_logging_enabled.md),
[`log()`](https://otel.r-lib.org/reference/log.md)

## Examples

``` r
log_severity_levels
#>  trace trace2 trace3 trace4  debug debug2 debug3 debug4   info  info2 
#>      1      2      3      4      5      6      7      8      9     10 
#>  info3  info4   warn  warn2  warn3  warn4  error error2 error3 error4 
#>     11     12     13     14     15     16     17     18     19     20 
#>  fatal fatal2 fatal3 fatal4 
#>     21     22     23     24 
```
