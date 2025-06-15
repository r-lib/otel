# otel_save_cache, otel_restore_cache

    Code
      as.list(env)
    Output
      $instruments
      NULL
      
      $meter_provider
       [1]  1  2  3  4  5  6  7  8  9 10
      
      $tracer_provider
      [1] "bar"
      
      $logger_provider
      NULL
      
      $tracer_app
      NULL
      

---

    Code
      as.list(the)
    Output
      $instruments
      NULL
      
      $meter_provider
       [1]  1  2  3  4  5  6  7  8  9 10
      
      $tracer_provider
      [1] "bar"
      
      $logger_provider
      NULL
      
      $mode
      [1] "prod"
      
      $tracer_app
      NULL
      

# setup_r_trace

    Code
      res
    Output
      [[1]]
      [1] "ok"
      
      [[2]]
      NULL
      
      [[3]]
      NULL
      

---

    Code
      res
    Output
      [[1]]
      [1] "ok"
      
      [[2]]
      [1] "inc*"
      
      [[3]]
      [1] "exclude.*"
      

---

    Code
      res[[1]]
    Output
      [1] "UserHook::ok::onLoad"
    Code
      body(res[[2]])
    Output
      trace_namespace(pkg, inc, exc)

