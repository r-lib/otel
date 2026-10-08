# Environment variables to configure otel

This manual page contains the environment variables you can use to
configure the otel package.

See also the [Environment
Variables](https://otelsdk.r-lib.org/reference/environmentvariables.html)
in the otelsdk package, which is charge of the data collection
configuration.

You need set these environment variables when configuring the collection
of telemetry data, unless noted otherwise.

## Value

Not applicable.

## Production or Development Environment

- `OTEL_ENV`

  By default otel runs in production mode. In production mode otel
  functions never error. Errors in the telemetry code will not stop the
  monitored application.

  This behavior is not ideal for development, where one would prefer to
  catch errors early. Set

      OTEL_ENV=dev

  to run otel in development mode, where otel functions fail on error,
  make it easier to fix errors.

## Selecting Exporters

otel is responsible for selecting the providers to use for traces, logs
and metrics. You can use the environment variables below to point the
otel functions to the desired providers.

If none of these environment variables are set, then otel will not emit
any telemetry data.

- `OTEL_TRACES_EXPORTER`

  The name of the selected tracer provider. See
  [`get_default_tracer_provider()`](https://otel.r-lib.org/reference/get_default_tracer_provider.md)
  for the possible values.

- `OTEL_R_TRACES_EXPORTER`

  R specific version of `OTEL_TRACES_EXPORTER`.

- `OTEL_LOGS_EXPORTER`

  The name of the selected logger provider. See
  [`get_default_logger_provider()`](https://otel.r-lib.org/reference/get_default_logger_provider.md)
  for the possible values.

- `OTEL_R_LOGS_EXPORTER`

  R specific version of `OTEL_LOGS_EXPORTER`.

- `OTEL_METRICS_EXPORTER`

  The name of the selected meter provider. See
  [`get_default_meter_provider()`](https://otel.r-lib.org/reference/get_default_meter_provider.md)
  for the possible values.

- `OTEL_R_METRICS_EXPORTER`

  R specific version of `OTEL_METRICS_EXPORTER`.

## Suppressing Instrumentation Scopes (R Packages)

otel has two environment variables to fine tune which instrumentation
scopes (i.e. R packages, typically) emit telemetry data. By default,
i.e. if neither of these are set, all packages emit telemetry data.

- `OTEL_R_EMIT_SCOPES`

  Set this environment variable to a comma separated string of
  instrumentation scope names or R package names to restrict telemetry
  to these packages only. The name of the instrumentation scope is the
  same as the name of the tracer, logger or meter, see
  [`default_tracer_name()`](https://otel.r-lib.org/reference/default_tracer_name.md).

  You can mix package names and instrumentation scope names and you can
  also use wildcards (globbing). For example the value

  OTEL_R_EMIT_SCOPES="org.r-lib.\*,dplyr"

  selects all packages with an instrumentation scope that starts with
  `org.r-lib.` and also dplyr.

- `OTEL_R_SUPPRESS_SCOPES`

  Set this environment variable to a comma separated string of
  instrumentation scope names or R package names to suppress telemetry
  data from these packages. The name of the instrumentation scope is the
  same as the name of the tracer, logger or meter, see
  [`default_tracer_name()`](https://otel.r-lib.org/reference/default_tracer_name.md).

  You can mix package names and instrumentation scope names and you can
  also use wildcards (globbing). For example the value

  OTEL_R_SUPPRESS_SCOPES="org.r-lib.\*,dplyr"

  excludes packages with an instrumentation scope that starts with
  `org.r-lib.` and also dplyr.

## [Zero Code Instrumentation](https://otel.r-lib.org/reference/zci.md)

otel can instrument R packages for OpenTelemetry data collection without
changing their source code. This relies on changing the code of the R
functions manually using
[`base::trace()`](https://rdrr.io/r/base/trace.html) and can be
configured using environment variables.

- `OTEL_R_INSTRUMENT_PKGS`

  Set `OTEL_R_INSTRUMENT_PKGS` to a comma separated list of packages to
  instrument. The automatic instrumentation happens when the otel
  package is loaded, so in general it is best to set this environment
  variable before loading R.

- `OTEL_R_INSTRUMENT_PKGS_<pkg>_INCLUDE`

  For an automatically instrumented package, set this environment
  variable to only instrument a subset of its functions. It is parsed as
  a comma separated string of function names, which may also include `?`
  and `*` wildcards (globbing).

- `OTEL_R_INSTRUMENT_PKGS_<pkg>_EXCLUDE`

  For an automatically instrumented package, set this environment
  variable to exclude some functions from instrumentation. It has the
  same syntax as its `*_INCLUDE` pair. If both are set, then inclusion
  is applied and the exclusion.

## Attribute Limits

- `OTEL_ATTRIBUTE_COUNT_LIMIT`

  Set this environment variable to limit the number of attributes for a
  single span, log record, metric measurement, etc. If unset, the
  default limit is 128 attributes. Note that only attributes specified
  with
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  are subject to this environment variable.

- `OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT`

  Set this environment variable to limit the length of vectors in
  attributes for a single span, log record, metric measurement, etc. If
  unset, there is no limit on the lengths of vectors in attributes. Note
  that only attributes specified with
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  are subject to this environment variable.

## See also

[Environment
Variables](https://otelsdk.r-lib.org/reference/environmentvariables.html)
in otelsdk

## Examples

``` r
# To start an R session using the OTLP exporter:
# OTEL_TRACES_EXPORTER=http R -q -f script.R
```
