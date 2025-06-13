# errmsg

    Code
      errmsg("this is a ", "message instead of an error")
    Message
      this is a message instead of an error

# msg

    Code
      msg("just being busy")
    Message
      i just being busy

---

    Code
      msg("nothing to see here")

# glob_filter

    Code
      glob_filter(fns, include = "default_*")
    Output
      [1] "default_logs_exporter_envvar"   "default_logs_exporter_envvar_r"
    Code
      glob_filter(fns, include = c("default_*", "?auge"))
    Output
      [1] "default_logs_exporter_envvar"   "default_logs_exporter_envvar_r"
      [3] "gauge"                         
    Code
      glob_filter(fns, exclude = c("errmsg", "get_*"))
    Output
      [1] "default_logs_exporter_envvar"   "default_logs_exporter_envvar_r"
      [3] "friendly_type"                  "gauge"                         
    Code
      glob_filter(fns, include = c("default_*", "?auge"), exclude = "*_r")
    Output
      [1] "default_logs_exporter_envvar" "gauge"                       

# get_env_count

    Code
      get_env_count("FOO", -1L)
    Condition
      Error in `get_env_count()`:
      ! Invalid `default` in `get_env_count()`, must be a non-negative integer scalar.
    Code
      get_env_count("FOO", "bad-default")
    Condition
      Error in `get_env_count()`:
      ! Invalid `default` in `get_env_count()`, must be a non-negative integer scalar.

