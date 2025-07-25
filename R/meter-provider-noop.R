#' OpenTelemetry meter provider objects
#'
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' The meter provider defines how metrics are exported when collecting
#' telemetry data. It is unlikely that you need to use meter provider
#' objects directly.
#'
#' Usually there is a single meter provider for an R app or script.
#'
#' Typically the meter provider is created automatically, at the first
#' [counter_add()], [up_down_counter_add()], [histogram_record()],
#' [gauge_record()] or [get_meter()] call. otel decides which meter
#' provider class to use based on [Environment Variables].
#'
#' # Implementations
#'
#' Note that this list is updated manually and may be incomplete.
#'
#' - [meter_provider_noop]: No-op meter provider, used when no metrics are
#'   emitted.
#' - [otelsdk::meter_provider_file]: Save metrics to a JSONL file.
#' - [otelsdk::meter_provider_http]: Send metrics to a collector over
#'   HTTP/OTLP.
#' - [otelsdk::meter_provider_memory]: Collect emitted metrics in memory.
#'   For testing.
#' - [otelsdk::meter_provider_stdstream]: Write metrics to standard output
#'   or error or to a file. Primarily for debugging.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `meter_provider$get_meter()`
#'
#' Get or create a new meter object.
#'
#' ### Usage
#'
#' ```r
#' meter_provider$get_meter(
#'   name = NULL,
#'   version = NULL,
#'   schema_url = NULL,
#'   attributes = NULL
#' )
#' ```
#'
#' ### Arguments
#'
#' - `name`: Meter name, see [get_meter()].
#' - `version`: Optional. Specifies the version of the instrumentation
#'   scope if the scope has a version (e.g. R package version).
#'   Example value: `"1.0.0"`.
#' - `schema_url`: Optional. Specifies the Schema URL that should be
#'   recorded in the emitted telemetry.
#' - `attributes`: Optional. Specifies the instrumentation scope
#'   attributes to associate with emitted telemetry. See [as_attributes()]
#'   for allowed values. You can also use [as_attributes()] to convert R
#'   objects to OpenTelemetry attributes.
#'
#' ### Value
#'
#' Returns an OpenTelemetry meter ([otel_meter]) object.
#'
#' ### See also
#'
#' [get_default_meter_provider()], [get_meter()].
#'
# -------------------------------------------------------------------------
#' ## `meter_provider$flush()`
#'
#' Force any buffered metrics to flush. Meter providers might not implement
#' this method.
#'
#' ### Usage
#'
#' ```
#' meter_provider$flush()
#' ```
#'
#' ### Value
#'
#' Nothing.
#'
# -------------------------------------------------------------------------
#' ## `meter_provider$shutdown()`
#'
#' Stop the meter provider. Stops collecting and emitting measurements.
#'
#' ### Usage
#'
#' ```r
#' meter_provider$shurdown()
#' ```
#'
#' ### Value
#'
#' Nothing
#'
#' @name otel_meter_provider
#' @family low level metrics API
#' @return Not applicable.
#' @examples
#' mp <- otel::get_default_meter_provider()
#' mtr <- mp$get_meter()
#' mtr$is_enabled()
NULL

#' No-op Meter Provider
#'
#' This is the meter provider ([otel_meter_provider]) otel uses when
#' metrics collection is disabled.
#'
#' All methods are no-ops or return objects that are also no-ops.
#'
#' @family low level metrics API
#' @usage NULL
#' @format NULL
#' @export
#' @return Not applicable.
#' @examples
#' meter_provider_noop$new()

meter_provider_noop <- list(
  new = function() {
    self <- structure(
      list(
        get_meter = function(
          name = NULL,
          version = NULL,
          schema_url = NULL,
          attributes = NULL
        ) {
          meter_noop$new(
            name = name,
            version = version,
            schema_url = schema_url,
            attributes = attributes
          )
        },
        flush = function(timeout = NULL, ...) {
          # noop
          invisible(self)
        },
        shutdown = function(timeout = NULL, ...) {
          # noop
          invisible(self)
        },
        get_metrics = function() {
          list()
        }
      ),
      class = c(
        "otel_meter_provider_noop",
        "otel_meter_provider"
      )
    )
    self
  }
)

#' OpenTelemetry Meter Object
#' @name otel_meter
#' @family low level metrics API
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' Usually you do not need to deal with otel_meter objects directly.
#' [counter_add()], [up_down_counter_add()], [histogram_record()] and
#' [gauge_record()] automatically set up the meter and uses it to create
#' instruments.
#'
#' A meter object is created by calling the `get_meter()` method of an
#' [otel_meter_provider].
#'
#' You can use the `create_counter()`, `create_up_down_counter()`,
#' `create_histogram()`, `create_gauge()` methods of the meter object to
#' create instruments.
#'
#' Typically there is a separate meter object for each instrumented R
#' package.
#'
#' # Methods
#'
# -------------------------------------------------------------------------
#' ## `meter$is_enabled()`
#'
#' Whether the meter is active and emitting measurements.
#'
#' This is equivalent to the [is_measuring_enabled()] function.
#'
#' ### Usage
#'
#' ```r
#' meter$is_enabled()
#' ```
#'
#' ### Value
#'
#' Logical scalar.
#'
# -------------------------------------------------------------------------
#' ## `meter$create_counter()`
#'
#' Create a new [counter instrument](
#'   https://opentelemetry.io/docs/specs/otel/metrics/api/#counter).
#'
#' ### Usage
#'
#' ```r
#' create_counter(name, description = NULL, unit = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `name`: Name of the instrument.
#' - `description`: Optional description.
#' - `unit`: `r doc_arg()[["unit"]]`
#'
#' ### Value
#'
#' An OpenTelemetry counter ([otel_counter]) object.
#'
# -------------------------------------------------------------------------
#' ## `meter$create_up_down_counter()`
#'
#' Create a new [up-down counter instrument](
#'   https://opentelemetry.io/docs/specs/otel/metrics/api/#updowncounter).
#'
#' ### Usage
#'
#' ```r
#' create_up_down_counter(name, description = NULL, unit = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `name`: Name of the instrument.
#' - `description`: Optional description.
#' - `unit`: `r doc_arg()[["unit"]]`
#'
#' ### Value
#'
#' An OpenTelemetry counter ([otel_up_down_counter]) object.
#'
# -------------------------------------------------------------------------
#' ## `meter$create_histogram()`
#'
#' Create a new [histogram](
#'   https://opentelemetry.io/docs/specs/otel/metrics/api/#histogram).
#'
#' ### Usage
#'
#' ```r
#' create_histogram(name, description = NULL, unit = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `name`: Name of the instrument.
#' - `description`: Optional description.
#' - `unit`: `r doc_arg()[["unit"]]`
#'
#' ### Value
#'
#' An OpenTelemetry histogram ([otel_histogram]) object.
#'
# -------------------------------------------------------------------------
#' ## `meter$create_gauge()`
#'
#' Create a new [gauge](
#'   https://opentelemetry.io/docs/specs/otel/metrics/api/#gauge).
#'
#' ### Usage
#'
#' ```r
#' create_gauge(name, description = NULL, unit = NULL)
#' ```
#'
#' ### Arguments
#'
#' - `name`: Name of the instrument.
#' - `description`: Optional description.
#' - `unit`: `r doc_arg()[["unit"]]`
#'
#' ### Value
#'
#' An OpenTelemetry gauge ([otel_gauge]) object.
#'
#' @return Not applicable.
#' @examples
#' mp <- get_default_meter_provider()
#' mtr <- mp$get_meter()
#' ctr <- mtr$create_counter("session")
#' ctr$add(1)
NULL

meter_noop <- list(
  new = function(
    name = NULL,
    version = NULL,
    schema_url = NULL,
    attributes = NULL
  ) {
    self <- structure(
      list(
        create_counter = function(
          name,
          description = NULL,
          unit = NULL
        ) {
          counter_noop$new(name, description, unit)
        },
        create_up_down_counter = function(
          name,
          description = NULL,
          unit = NULL
        ) {
          up_down_counter_noop$new(name, description, unit)
        },
        create_histogram = function(
          name,
          description = NULL,
          unit = NULL
        ) {
          histogram_noop$new(name, description, unit)
        },
        create_gauge = function(
          name,
          description = NULL,
          unit = NULL
        ) {
          gauge_noop$new(name, description, unit)
        },
        is_enabled = function() FALSE
      ),
      class = c("otel_meter_noop", "otel_meter")
    )
    self
  }
)

#' OpenTelemetry Counter Object
#'
#' @name otel_counter
#' @family low level metrics API
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' Usually you do not need to deal with otel_counter objects directly.
#' [counter_add()] automatically sets up a meter and creates a counter
#' instrument, as needed.
#'
#' A counter object is created by calling the `create_counter()` method
#' of an [otel_meter_provider()].
#'
#' You can use the `add()` method to increment the counter by a positive
#' amount.
#'
#' In R counters are represented by double values.
#'
#' # Methods
#'
#' ## `counter$add()`
#'
#' Increment the counter by a fixed amount.
#'
#' ### Usage
#'
#' ```r
#' counter$add(value, attributes = NULL, span_context = NULL, ...)
#' ```
#'
#' ### Arguments
#'
#' - `value`: Value to increment the counter with.
#' - `attributes`: Additional attributes to add.
#' - `span_context`: Span context. If missing, the active context is used,
#'   if any.
#'
#' ### Value
#'
#' The counter object itself, invisibly.
#'
#' @return Not applicable.
#' @examples
#' mp <- get_default_meter_provider()
#' mtr <- mp$get_meter()
#' ctr <- mtr$create_counter("session")
#' ctr$add(1)
NULL

counter_noop <- list(
  new = function(name = NULL, ...) {
    self <- structure(
      list(
        add = function(value, attributes = NULL, span_context = NULL, ...) {
          invisible(self)
        }
      ),
      class = c("otel_counter_noop", "otel_counter")
    )
    self
  }
)

#' OpenTelemetry Up-Down Counter Object
#'
#' @name otel_up_down_counter
#' @family low level metrics API
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' Usually you do not need to deal with otel_up_down_counter objects directly.
#' [up_down_counter_add()] automatically sets up a meter and creates an
#' up-down counter instrument, as needed.
#'
#' An up-down counter object is created by calling the
#' `create_up_down_counter()` method of an [otel_meter_provider()].
#'
#' You can use the `add()` method to increment or decrement the counter.
#'
#' In R up-down counters are represented by double values.
#'
#' # Methods
#'
#' ## `up_down_counter$add()`
#'
#' Increment or decrement the up-down counter by a fixed amount.
#'
#' ### Usage
#'
#' ```r
#' up_down_counter$add(value, attributes = NULL, span_context = NULL, ...)
#' ```
#'
#' ### Arguments
#'
#' - `value`: Value to increment of decrement the up-down counter with.
#' - `attributes`: Additional attributes to add.
#' - `span_context`: Span context. If missing, the active context is used,
#'   if any.
#'
#' ### Value
#'
#' The up-down counter object itself, invisibly.
#'
#' @return Not applicable.
#' @examples
#' mp <- get_default_meter_provider()
#' mtr <- mp$get_meter()
#' ctr <- mtr$create_up_down_counter("session")
#' ctr$add(1)
NULL

up_down_counter_noop <- list(
  new = function(name = NULL, ...) {
    self <- structure(
      list(
        add = function(value, attributes = NULL, span_context = NULL, ...) {
          invisible(self)
        }
      ),
      class = c("otel_up_down_counter_noop", "otel_up_down_counter")
    )
    self
  }
)

#' OpenTelemetry Histogram Object
#'
#' @name otel_histogram
#' @family low level metrics API
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' Usually you do not need to deal with otel_histogram objects directly.
#' [histogram_record()] automatically sets up a meter and creates a
#' histogram instrument, as needed.
#'
#' A histogram object is created by calling the `create_histogram()` method
#' of an [otel_meter_provider()].
#'
#' You can use the `record()` method to update the statistics with the
#' specified amount.
#'
#' In R histogram values are represented by doubles.
#'
#' # Methods
#'
#' ## `histogram$record()`
#'
#' Update the statistics with the specified amount.
#'
#' ### Usage
#'
#' ```r
#' histogram$record(value, attributes = NULL, span_context = NULL, ...)
#' ```
#'
#' ### Arguments
#'
#' - `value`: A numeric value to record.
#' - `attributes`: Additional attributes to add.
#' - `span_context`: Span context. If missing, the active context is used,
#'   if any.
#'
#' ### Value
#'
#' The histogram object itself, invisibly.
#'
#' @return Not applicable.
#' @examples
#' mp <- get_default_meter_provider()
#' mtr <- mp$get_meter()
#' hst <- mtr$create_histogram("response-time")
#' hst$record(1.123)
NULL

histogram_noop <- list(
  new = function(name = NULL, ...) {
    self <- structure(
      list(
        record = function(value, attributes = NULL, span_context = NULL, ...) {
          invisible(self)
        }
      ),
      class = c("otel_histogram_noop", "otel_histogram")
    )
    self
  }
)

#' OpenTelemetry Gauge Object
#'
#' @name otel_gauge
#' @family low level metrics API
#' @description
#' [otel_meter_provider] -> [otel_meter] -> [otel_counter],
#' [otel_up_down_counter], [otel_histogram], [otel_gauge]
#'
#' @details
#' Usually you do not need to deal with otel_gauge objects directly.
#' [gauge_record()] automatically sets up a meter and creates a
#' gauge instrument, as needed.
#'
#' A gauge object is created by calling the `create_gauge()` method
#' of an [otel_meter_provider()].
#'
#' You can use the `record()` method to record the current value.
#'
#' In R gauge values are represented by doubles.
#'
#' # Methods
#'
#' ## `gauge$record()`
#'
#' Update the statistics with the specified amount.
#'
#' ### Usage
#'
#' ```r
#' gauge$record(value, attributes = NULL, span_context = NULL, ...)
#' ```
#'
#' ### Arguments
#'
#' - `value`: A numeric value. The current absolute value.
#' - `attributes`: Additional attributes to add.
#' - `span_context`: Span context. If missing, the active context is used,
#'   if any.
#'
#' ### Value
#'
#' The gauge object itself, invisibly.
#'
#' @return Not applicable.
#' @examples
#' mp <- get_default_meter_provider()
#' mtr <- mp$get_meter()
#' gge <- mtr$create_gauge("response-time")
#' gge$record(1.123)
NULL

gauge_noop <- list(
  new = function(name = NULL, ...) {
    self <- structure(
      list(
        record = function(value, attributes = NULL, span_context = NULL, ...) {
          invisible(self)
        }
      ),
      class = c("otel_gauge_noop", "otel_gauge")
    )
  }
)
