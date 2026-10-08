# Zero Code Instrumentation

otel supports zero-code instrumentation (ZCI) via the
`OTEL_INSTRUMENT_R_PKGS` environment variable. Set this to a comma
separated list of package names, the packages that you want to
instrument. Then otel will hook up
[`base::trace()`](https://rdrr.io/r/base/trace.html) to produce
OpenTelemetry output from every function of these packages.

## Value

Not applicable.

## Details

By default all functions of the listed packages are instrumented. To
instrument a subset of all functions set the
`OTEL_INSTRUMENT_R_PKGS_<PKG>_INCLUDE` environment variable to a list of
glob expressions. `<PKG>` is the package name in all capital letters.
Only functions that match to at least one glob expression will be
instrumented.

To exclude functions from instrumentation, set the
`OTEL_INSTRUMENT_R_PKGS_<PKG>_EXCLUDE` environment variable to a list of
glob expressions. `<PKG>` is the package name in all capital letters.
Functions that match to at least one glob expression will not be
instrumented. Inclusion globs are applied before exclusion globs.

### Caveats

If the user calls [`base::trace()`](https://rdrr.io/r/base/trace.html)
on an instrumented function, that deletes the instrumentation, since the
second [`base::trace()`](https://rdrr.io/r/base/trace.html) call
overwrites the first.

## See also

[Environment
Variables](https://otel.r-lib.org/reference/environmentvariables.md)

Other OpenTelemetry trace API:
[`end_span()`](https://otel.r-lib.org/reference/end_span.md),
[`is_tracing_enabled()`](https://otel.r-lib.org/reference/is_tracing_enabled.md),
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md),
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md),
[`start_span()`](https://otel.r-lib.org/reference/start_span.md),
[`tracing-constants`](https://otel.r-lib.org/reference/tracing-constants.md),
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md)

## Examples

``` r
# To run an R script with ZCI:
# OTEL_TRACES_EXPORTER=http OTEL_INSTRUMENT_R_PKGS=dplyr,tidyr R -q -f script.R
```
