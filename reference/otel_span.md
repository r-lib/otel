# OpenTelemetry Span Object

[otel_tracer_provider](https://otel.r-lib.org/reference/otel_tracer_provider.md)
-\> [otel_tracer](https://otel.r-lib.org/reference/otel_tracer.md) -\>
otel_span -\>
[otel_span_context](https://otel.r-lib.org/reference/otel_span_context.md)

## Value

Not applicable.

## Details

An otel_span object represents an OpenTelemetry span.

Use
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
or [`start_span()`](https://otel.r-lib.org/reference/start_span.md) to
create and start a span.

Call [`end_span()`](https://otel.r-lib.org/reference/end_span.md) to end
a span explicitly. (See
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
and
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
to end a span automatically.)

## Lifetime

The span starts when it is created in the
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
or [`start_span()`](https://otel.r-lib.org/reference/start_span.md)
call.

The span ends when
[`end_span()`](https://otel.r-lib.org/reference/end_span.md) is called
on it, explicitly or automatically via
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
or
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md).

## Activation

After a span is created it may be active or inactively, independently of
its lifetime. A live span (i.e. a span that hasn't ended yet) may be
inactive. While this is less common, a span that has ended may still be
active.

When otel creates a new span, it sets the parent span of the new span to
the active span by default.

### Automatic spans

[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
creates a new span, starts it and activates it for the caller frame. It
also automatically ends the span when the caller frame exits.

### Manual spans

[`start_span()`](https://otel.r-lib.org/reference/start_span.md) creates
a new span and starts it, but it does not activate it. You must activate
the span manually using
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md).
You must also end the span manually with an
[`end_span()`](https://otel.r-lib.org/reference/end_span.md) call. (Or
the `end_on_exit` argument of
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md).)

## Parent spans

OpenTelemetry spans form a hierarchy: a span can refer to a parent span.
A span without a parent span is called a root span. A trace is a set of
connected spans.

When otel creates a new span, it sets the parent span of the new span to
the active span by default.

Alternatively, you can set the parent span of the new span manually. You
can also make the new span be a root span, by setting `parent = NA` in
`options` to the
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
or [`start_span()`](https://otel.r-lib.org/reference/start_span.md)
call.

## Methods

### `span$add_event()`

Add a single event to the span.

#### Usage

    span$add_event(name, attributes = NULL, timestamp = NULL)

#### Arguments

- `name`: Event name.

- `attributes`: Attributes to add to the event. See
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  for supported R types. You may also use
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  to convert an R object to an OpenTelemetry attribute value.

- `timestamp`: A
  [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) object.
  If missing, the current time is used.

#### Value

The span object itself, invisibly.

### `span$end()`

End the span. Calling this method is equivalent to calling the
[`end_span()`](https://otel.r-lib.org/reference/end_span.md) function on
the span.

Spans created with
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
end automatically by default. You must end every other span manually, by
calling `end_span`, or using the `end_on_exit` argument of
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md).

Calling the `span$end()` method (or
[`end_span()`](https://otel.r-lib.org/reference/end_span.md)) on a span
multiple times is not an error, the first call ends the span, subsequent
calls do nothing.

#### Usage

    span$end(options = NULL, status_code = NULL)

#### Arguments

- `options`: Named list of options. Possible entry:

  - `end_steady_time`: A
    [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html) object
    that will be used as a steady timer.

- `status_code`: Span status code to set before ending the span, see the
  `span$set_status()` method for possible values.

#### Value

The span object itself, invisibly.

### `span$get_context()`

Get a span's span context. The span context is an
[otel_span_context](https://otel.r-lib.org/reference/otel_span_context.md)
object that can be serialized, copied to other processes, and it can be
used to create new child spans.

#### Usage

    span$get_context()

#### Value

An
[otel_span_context](https://otel.r-lib.org/reference/otel_span_context.md)
object.

### `span$is_recording()`

Checks whether a span is recorded. If tracing is off, or the span ended
already, or the sampler decided not to record the trace the span belongs
to.

#### Usage

    span$is_recording()

#### Value

A logical scalar, `TRUE` if the span is recorded.

### `span$record_exception()`

Record an exception (error, usually) event for a span.

If the span was created with
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md),
or it was ended automatically with
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md),
then otel records exceptions automatically, and you don't need to call
this function manually.

You can still use it to record exceptions that are not R errors.

#### Usage

    span$record_exception(error_condition, attributes, ...)

#### Arguments

- `error_condition`: An R error object to record.

- `attributes`: Additional attributes to add to the exception event.

- `...`: Passed to the `span$add_event()` method.

#### Value

The span object itself, invisibly.

### `span$set_attribute()`

Set a single attribute. It is better to set attributes at span creation,
instead of calling this method later, since samplers can only make
decisions based on attributes present at span creation.

#### Usage

    span$set_attribute(name, value)

#### Arguments

- `name`: Attribute name.

- `value`: Attribute value. See
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  for supported R types. You may also use
  [`as_attributes()`](https://otel.r-lib.org/reference/as_attributes.md)
  to convert an R object to an OpenTelemetry attribute value.

#### Value

The span object itself, invisibly.

### `span$set_status()`

Set the status of the span.

If the span was created with
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md),
or it was ended automatically with
[`local_active_span()`](https://otel.r-lib.org/reference/local_active_span.md)
or
[`with_active_span()`](https://otel.r-lib.org/reference/with_active_span.md),
then otel sets the status of the span automatically to `ok` or `error`,
depending on whether an error happened in the frame the span was
activated for.

Otherwise the default span status is `unset`, and you need to set it
manually.

#### Usage

    span$set_status(status_code, description = NULL)

#### Arguments

- `status_code`: Possible values: unset, ok, error.

- `description`: Optional description, a string.

#### Value

The span itself, invisibly.

### `span$update_name()`

Update the span's name. Overrides the name give in
[`start_local_active_span()`](https://otel.r-lib.org/reference/start_local_active_span.md)
or [`start_span()`](https://otel.r-lib.org/reference/start_span.md).

It is undefined whether a sampler will use the original or the new name.

#### Usage

    span$update_name(name)

#### Arguments

- `name`: String, the new span name.

#### Value

The span object itself, invisibly.

## See also

Other low level trace API:
[`get_default_tracer_provider()`](https://otel.r-lib.org/reference/get_default_tracer_provider.md),
[`get_tracer()`](https://otel.r-lib.org/reference/get_tracer.md),
[`otel_span_context`](https://otel.r-lib.org/reference/otel_span_context.md),
[`otel_tracer`](https://otel.r-lib.org/reference/otel_tracer.md),
[`otel_tracer_provider`](https://otel.r-lib.org/reference/otel_tracer_provider.md),
[`tracer_provider_noop`](https://otel.r-lib.org/reference/tracer_provider_noop.md)

## Examples

``` r
fn <- function() {
  trc <- otel::get_tracer("myapp")
  spn <- trc$start_span("fn")
  # ...
  spn$set_attribute("key", "value")
  # ...
  on.exit(spn$end(status_code = "error"), add = TRUE)
  # ...
  spn$end(status_code = "ok")
}
fn()
```
