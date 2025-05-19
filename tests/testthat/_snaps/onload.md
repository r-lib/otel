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

