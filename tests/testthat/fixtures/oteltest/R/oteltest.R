#' @export

runme <- function() {
  message("runme in")
  child1()
  object[["notthis"]]()
  child2()
  message("runme out")
}

child1 <- function() {
  message("  child1 in")
  grandchild1()
  grandchild2()
  message("  child1 out")
}

child2 <- function() {
  message("  child2 in")
  grandchild1()
  grandchild2()
  message("  child2 out")
}

grandchild1 <- function() {
  message("    grandchild1")
}

grandchild2 <- function() {
  message("    grandchild2")
}

object <- list(
  1,
  2,
  3,
  notthis = function() {
    # this won't be instrumented, not a top-level package object
    message("  notthis")
  }
)
