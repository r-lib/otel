is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

as_string <- function(x, null = TRUE, call = NULL) {
  if (null & is.null(x)) {
    return(x)
  }
  if (is_string(x)) {
    return(x)
  }

  call <- call %||% match.call()
  stop(
    "Invalid argument: ",
    call[[2]],
    " must be a string scalar, but it is ",
    typename(x),
    "."
  )
}
