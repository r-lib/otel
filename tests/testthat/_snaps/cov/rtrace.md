# trace_env

    Code
      print(env$f, useSource = FALSE)
    Output
      new("functionWithTrace", .Data = function () 
      {
          on.exit(.doTrace({
              try(.__span$deactivate(.__scope))
              try(.__span$end())
          }))
          {
              .doTrace({
                  .__span <- otel::start_span("pkg::f", tracer = "org.r-lib.otel")
                  .__scope <- .__span$activate(NULL)
              })
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

