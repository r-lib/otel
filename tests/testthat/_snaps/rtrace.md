# trace_namespace

    Code
      trace_namespace("otel")
    Message
      i Instrumenting otel.

# trace_env

    Code
      print(env$f, useSource = FALSE)
    Output
      new("functionWithTrace", .Data = function () 
      {
          on.exit(.doTrace(try(.__span$end())))
          {
              .doTrace(.__span <- otel::get_tracer("org.r-lib.otel")$start_as_active_span("pkg::f", 
                  tracer_name = "org.r-lib.otel", scope = NULL))
              "dummy"
          }
      }, original = function () 
      "dummy", source = <environment>)
      <environment: 0x<address>>
      attr(,"original")
      function () 
      "dummy"
      <environment: 0x<address>>
      attr(,"source")
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "functionWithTrace"
      attr(,"class")attr(,"package")
      [1] "methods"
    Code
      env$obj
    Output
      [1] "not-a-function"

