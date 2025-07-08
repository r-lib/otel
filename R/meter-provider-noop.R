#' No-op logger provider
#' @keywords internal
#' @export

meter_provider_noop <- list(
  new = function() {
    self <- structure(
      list(
        get_meter = function(name = NULL, ...) {
          meter_noop$new(name, ...)
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

meter_noop <- list(
  new = function(name = NULL, ...) {
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
        }
      ),
      class = c("otel_meter_noop", "otel_meter")
    )
    self
  }
)

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
