#' R objects as OpenTelemetry attributes
#'
#' Convert a list of R objects to a form that is suitable as OpenTelemetry
#' attributes.
#'
#' @param x A list of R objects, to be used as OpenTelemetry attributes.
#' @return A named list that can be used as the `attributes` argument to
#'   the `start_span()` method of [otel_tracer], the `log()` method of
#'   [otel_logger], etc.
#'
#' If `x` is not named, or some names are the empty string or `NA`, then
#' integer numbers as used for the missing or invalid names.
#'
#' If some elements in `x` are not of the natively supported R types in
#' OpenTelemetry (`r paste0(otel_attr_types, collapse = ", ")`), then
#' their printed form is captured using [utils::capture.output()].
#'
#' ## Limits
#'
#' The number of attributes can be limited with the
#' `r otel_attr_val_lth_limit_var` environment variable. The default is
#' `r otel_attr_cnt_limit_dflt`.
#'
#' The length of the each attribute (vector) can be limited with the
#' `r otel_attr_val_lth_limit_var` environment variable. The default is
#' ``r format(otel_attr_val_lth_limit_dflt)``. Note that this is applied to
#' the length of each attribute as an R vector. E.g. it does _not_
#' currently limit the number of characters in individual strings.
#'
#' @export
#' @examples
#' as_attributes(list(
#'   number = 1.0,
#'   vector = 1:10,
#'   string = "otel",
#'   string_vector = letters,
#'   object = mtcars
#' ))

as_attributes <- function(x) {
  if (!is.list(x)) {
    stop("Invalid argument: `x` must be a list in `as_attributes()`.")
  }
  len_limit <- get_env_count(otel_attr_cnt_limit_var, otel_attr_cnt_limit_dflt)
  val_len_limit <- get_env_count(
    otel_attr_val_lth_limit_var,
    otel_attr_val_lth_limit_dflt
  )

  if (length(x) > len_limit) {
    x <- x[seq_len(len_limit)]
  }

  # create unique names for the attributes
  nms <- as.character(names(x))
  length(nms) <- length(x)
  if (anyNA(nms) || any(nms == "")) {
    bad <- is.na(nms) | nms == ""
    nms[bad] <- as.character(seq_along(x))[bad]
  }
  nms <- make.unique(nms)

  for (i in seq_along(x)) {
    if (!typeof(x[[i]]) %in% otel_attr_types) {
      x[[i]] <- utils::capture.output(print(x[[i]]))
    }
    if (length(x[[i]]) > val_len_limit) {
      length(x[[i]]) <- val_len_limit
    }
    if (is.double(x[[i]]) && !all(is.finite(x[[i]]))) {
      # as.character keeps NAs, which is not what we want here
      x[[i]] <- paste(x[[i]])
    }
  }

  structure(x, names = nms)
}

otel_attr_cnt_limit_var <- "OTEL_ATTRIBUTE_COUNT_LIMIT"
otel_attr_cnt_limit_dflt <- 128L
otel_attr_val_lth_limit_var <- "OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT"
otel_attr_val_lth_limit_dflt <- Inf

otel_attr_types <- c(typeof(""), typeof(TRUE), typeof(1), typeof(1L))
