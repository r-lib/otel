% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracer-provider-noop.R
\name{otel_span}
\alias{otel_span}
\title{OpenTelemetry Span Object}
\value{
Not applicable.
}
\description{
\link{otel_tracer_provider} -> \link{otel_tracer} -> \link{otel_span} -> \link{otel_span_context}
}
\details{
An otel_span object represents an OpenTelemetry span.

Use \code{\link[=start_local_active_span]{start_local_active_span()}} or \code{\link[=start_span]{start_span()}} to create and start
a span.

Call \code{\link[=end_span]{end_span()}} to end a span explicitly. (See
\code{\link[=start_local_active_span]{start_local_active_span()}} and \code{\link[=local_active_span]{local_active_span()}} to end a span
automatically.)
}
\section{Lifetime}{
The span starts when it is created in the \code{\link[=start_local_active_span]{start_local_active_span()}}
or \code{\link[=start_span]{start_span()}} call.

The span ends when \code{\link[=end_span]{end_span()}} is called on it, explicitly or
automatically via \code{\link[=start_local_active_span]{start_local_active_span()}} or \code{\link[=local_active_span]{local_active_span()}}.
}

\section{Activation}{
After a span is created it may be active or inactively, independently
of its lifetime. A live span (i.e. a span that hasn't ended yet) may
be inactive. While this is less common, a span that has ended may still
be active.

When otel creates a new span, it sets the parent span of the new span
to the active span by default.
\subsection{Automatic spans}{

\code{\link[=start_local_active_span]{start_local_active_span()}} creates a new span, starts it and activates
it for the caller frame. It also automatically ends the span when the
caller frame exits.
}

\subsection{Manual spans}{

\code{\link[=start_span]{start_span()}} creates a new span and starts it, but it does not
activate it. You must activate the span manually using
\code{\link[=local_active_span]{local_active_span()}} or \code{\link[=with_active_span]{with_active_span()}}. You must also end the
span manually with an \code{\link[=end_span]{end_span()}} call. (Or the \code{end_on_exit} argument
of \code{\link[=local_active_span]{local_active_span()}} or \code{\link[=with_active_span]{with_active_span()}}.)
}
}

\section{Parent spans}{
OpenTelemetry spans form a hierarchy: a span can refer to a parent span.
A span without a parent span is called a root span. A trace is a set of
connected spans.

When otel creates a new span, it sets the parent span of the new span
to the active span by default.

Alternatively, you can set the parent span of the new span manually.
You can also make the new span be a root span, by setting \code{parent = NA}
in \code{options} to the \code{\link[=start_local_active_span]{start_local_active_span()}} or \code{\link[=start_span]{start_span()}} call.
}

\section{Methods}{
\subsection{\code{span$add_event()}}{

Add a single event to the span.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$add_event(name, attributes = NULL, timestamp = NULL)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{name}: Event name.
\item \code{attributes}: Attributes to add to the event. See \code{\link[=as_attributes]{as_attributes()}}
for supported R types. You may also use \code{\link[=as_attributes]{as_attributes()}} to convert
an R object to an OpenTelemetry attribute value.
\item \code{timestamp}: A \link[base:DateTimeClasses]{base::POSIXct} object. If missing, the current time is
used.
}
}

\subsection{Value}{

The span object itself, invisibly.
}

}

\subsection{\code{span$end()}}{

End the span. Calling this method is equivalent to calling the
\code{\link[=end_span]{end_span()}} function on the span.

Spans created with \code{\link[=start_local_active_span]{start_local_active_span()}} end automatically by
default. You must end every other span manually, by calling \code{end_span},
or using the \code{end_on_exit} argument of \code{\link[=local_active_span]{local_active_span()}} or
\code{\link[=with_active_span]{with_active_span()}}.

Calling the \code{span$end()} method (or \code{end_span()}) on a span multiple
times is not an error, the first call ends the span, subsequent calls do
nothing.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$end(options = NULL, status_code = NULL)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{options}: Named list of options. Possible entry:
\itemize{
\item \code{end_steady_time}: A \link[base:DateTimeClasses]{base::POSIXct} object that will be used as
a steady timer.
}
\item \code{status_code}: Span status code to set before ending the span, see
the \code{span$set_status()} method for possible values.
}
}

\subsection{Value}{

The span object itself, invisibly.
}

}

\subsection{\code{span$get_context()}}{

Get a span's span context. The span context is an \link{otel_span_context}
object that can be serialized, copied to other processes, and it can be
used to create new child spans.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$get_context()
}\if{html}{\out{</div>}}
}

\subsection{Value}{

An \link{otel_span_context} object.
}

}

\subsection{\code{span$is_recording()}}{

Checks whether a span is recorded. If tracing is off, or the span ended
already, or the sampler decided not to record the trace the span belongs
to.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$is_recording()
}\if{html}{\out{</div>}}
}

\subsection{Value}{

A logical scalar, \code{TRUE} if the span is recorded.
}

}

\subsection{\code{span$record_exception()}}{

Record an exception (error, usually) event for a span.

If the span was created with \code{\link[=start_local_active_span]{start_local_active_span()}}, or it was
ended automatically with \code{\link[=local_active_span]{local_active_span()}} or \code{\link[=with_active_span]{with_active_span()}},
then otel records exceptions automatically, and you don't need to call
this function manually.

You can still use it to record exceptions that are not R errors.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$record_exception(error_condition, attributes, ...)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{error_condition}: An R error object to record.
\item \code{attributes}: Additional attributes to add to the exception event.
\item \code{...}: Passed to the \code{span$add_event()} method.
}
}

\subsection{Value}{

The span object itself, invisibly.
}

}

\subsection{\code{span$set_attribute()}}{

Set a single attribute. It is better to set attributes at span creation,
instead of calling this method later, since samplers can only make
decisions based on attributes present at span creation.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$set_attribute(name, value)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{name}: Attribute name.
\item \code{value}: Attribute value. See \code{\link[=as_attributes]{as_attributes()}} for supported R types.
You may also use \code{\link[=as_attributes]{as_attributes()}} to convert an R object to an
OpenTelemetry attribute value.
}
}

\subsection{Value}{

The span object itself, invisibly.
}

}

\subsection{\code{span$set_status()}}{

Set the status of the span.

If the span was created with \code{\link[=start_local_active_span]{start_local_active_span()}}, or it was
ended automatically with \code{\link[=local_active_span]{local_active_span()}} or \code{\link[=with_active_span]{with_active_span()}},
then otel sets the status of the span automatically to \code{ok} or \code{error},
depending on whether an error happened in the frame the span was
activated for.

Otherwise the default span status is \code{unset}, and you need to set it
manually.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$set_status(status_code, description = NULL)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{status_code}: Possible values:
unset, ok, error.
\item \code{description}: Optional description, a string.
}
}

\subsection{Value}{

The span itself, invisibly.
}

}

\subsection{\code{span$update_name()}}{

Update the span's name. Overrides the name give in
\code{\link[=start_local_active_span]{start_local_active_span()}} or \code{\link[=start_span]{start_span()}}.

It is undefined whether a sampler will use the original or the new name.
\subsection{Usage}{

\if{html}{\out{<div class="sourceCode r">}}\preformatted{span$update_name(name)
}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\itemize{
\item \code{name}: String, the new span name.
}
}

\subsection{Value}{

The span object itself, invisibly.
}

}
}

\examples{
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
}
\seealso{
Other low level trace API: 
\code{\link{get_default_tracer_provider}()},
\code{\link{get_tracer}()},
\code{\link{otel_span_context}},
\code{\link{otel_tracer}},
\code{\link{otel_tracer_provider}},
\code{\link{tracer_provider_noop}}
}
\concept{low level trace API}
