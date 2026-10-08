# trace_namespace

    Code
      trace_namespace("otel")
    Message
      i Instrumenting otel.

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

