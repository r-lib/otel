# start_shiny_app

    Code
      trc <- start_shiny_app()
    Message
      OpenTelemetry error: oops

# start_shiny_app_dev

    Code
      start_shiny_app_dev()
    Condition
      Error in `get_tracer()`:
      ! oops

# start_shiny_app_dev 2

    Code
      start_shiny_app_dev()

# start_shiny_session

    Code
      spn <- start_shiny_session(session = ssn, attributes = attr, options = opts)
    Message
      start session
      end session

---

    Code
      spn$attributes
    Output
      $PATH_INFO
      [1] "path-info"
      
      $HTTP_HOST
      [1] "http-host"
      
      $HTTP_ORIGIN
      [1] "http-origin"
      
      $QUERY_STRING
      [1] "query-string"
      
      $SERVER_PORT
      [1] 11122
      

---

    Code
      spn <- start_shiny_session()
    Message
      OpenTelemetry error: boo!

# start_shiny_session_dev

    Code
      start_shiny_session_dev()
    Condition
      Error in `get_tracer()`:
      ! oops

# start_shiny_session_dev 2

    Code
      spn <- start_shiny_session_dev(session = ssn, attributes = attr, options = opts)
    Message
      start session
      end session

---

    Code
      spn$attributes
    Output
      $PATH_INFO
      [1] "path-info"
      
      $HTTP_HOST
      [1] "http-host"
      
      $HTTP_ORIGIN
      [1] "http-origin"
      
      $QUERY_STRING
      [1] "query-string"
      
      $SERVER_PORT
      [1] 11122
      

