#! /bin/sh

mfns <- c(
  "man/environmentvariables.Rd",
  "man/zci.Rd",
  "man/as_attributes.Rd"
)

for (fn in mfns) {
  lns <- readLines(fn)
  flt <- grep("^\\\\alias[{]OTEL_", lns, value = TRUE, invert = TRUE)
  writeLines(flt, fn)
}
