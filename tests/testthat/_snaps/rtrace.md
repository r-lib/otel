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

# integration test

    Code
      callr::rscript(script, env = env_wo)
    Output
      <environment: namespace:otel>
      runme in
        child1 in
          grandchild1
          grandchild2
        child1 out
        notthis
        child2 in
          grandchild1
          grandchild2
        child2 out
      runme out

---

    Code
      callr::rscript(script, env = env)
    Output
      <environment: namespace:otel>
      runme in
        child1 in
          grandchild1
          grandchild2
        child1 out
        notthis
        child2 in
          grandchild1
          grandchild2
        child2 out
      runme out

---

    Code
      cli::tree(spnstree, root = root)
    Output
      oteltest::runme
      +-oteltest::child1
      | +-oteltest::grandchild1
      | \-oteltest::grandchild2
      \-oteltest::child2
        +-oteltest::grandchild1
        \-oteltest::grandchild2

