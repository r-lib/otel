
<!-- README.md is generated from README.Rmd. Please edit that file -->

# otel

> OpenTelemetry API for R packages and projects

<!-- badges: start -->

![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)
[![R-CMD-check](https://github.com/r-lib/otel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/otel/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/r-lib/otel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/otel/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-lib/otel/graph/badge.svg)](https://app.codecov.io/gh/r-lib/otel)
<!-- badges: end -->

High-quality, ubiquitous, and portable telemetry to enable effective
observability. [OpenTelemetry](https://opentelemetry.io/docs/) is a
collection of tools, APIs, and SDKs used to instrument, generate,
collect, and export telemetry data (metrics, logs, and traces) for
analysis in order to understand your software’s performance and
behavior.

Use this package as a dependency if you want to instrument your R
package or project for OpenTelemetry.

Use the [otelsdk](https://github.com/r-lib/otelsdk) to produce
OpenTelemetry output from an R package or project that was instrumented
with the otel package.

## Installation

> [!WARNING]
> This package is experimental and may introduce breaking
> changes any time. It probably works best with the latest commit of the
> [otelsdk](https://github.com/r-lib/otelsdk) package.

You can install the development version of otel from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("r-lib/otel")
```

## Usage

- Call `otel::start_span()` to create a span. By default the span ends
  when the called function returns.
- Use the `$set_attribute()`, `$add_event()`, `$add_link()` and
  `$set_status()` methods of a span to manipulate it.
- See the [otelsdk](https://github.com/r-lib/otelsdk) package for
  producing output from an instrumented R package or project.

## Zero-code instrumentation

otel supports zero-code instrumentation via the `OTEL_INSTRUMENT_R_PKGS`
environment variable. Set this to a comma separated list of package
names, the packages that you want to instrument. Then otel will hook up
`base::trace()` to produce OpenTelemetry output from every function of
these packages.

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

## Production and development sessions

Bye default otel and otelsdk run in production mode. In production mode
otel (and otelsdk) functions never error. This behavior does not help
catching errors early during the developent of the instrumented project.
Set the `OTEL_ENV` environment variable to `dev` to switch to
development mode, where otel and otelsdk functions fail on errors.

## Concurrency

To support concurrency on a single thread, e.g. a Shiny app serving
multiple requests at the same time, you can manage the lifetime and
actication of the OpenTelemetry spans manually. We recommend the
following practices for in-process concurrency:

- Create a new long lasting span with `otel::start_span(scope = NULL)`.
- Assign the returned span into the corresponding object of the
  concurrent and/or asynchronous computation. Every span has a finalizer
  that closes the span.
- When running code that belongs concurrent computation represented by
  the span, activate it for a specific R scope by calling
  `with_active_span()` or `local_active_span()`.
- When logging use `with_active_span()` or `local_active_span()` to
  activate the span that belongs to the log messages.
- Similarly, for metrics, call `with_active_span()` or
  `local_active_span()`. to link instruments to the correct span.
- When the concurrent computation ends, close the span manually with its
  `$end()` method. (Otherwise it would be only closed at the next
  garbage collection, assuming there are no references to it.)

## Shiny apps

otel has convenience functions to tie otel sessions to Shiny sessions:

- Use `start_shiny_app()` from the `global.R` file, before the app start
  up.
- Use `start_shiny_session()` from the server function, at the start of
  a new session, and pass the Shiny session object to it.
- Pass the OpenTelemetry session span to all `otel::start_span()` calls
  from reactive expressions, to make sure that they are logged to the
  correct session.

See the examples included in the otel package.

## License

MIT © Posit, PBC
