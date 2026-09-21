# Path to an example file the package installs under inst/examples/ (see
# inst/examples/README.md). Under devtools::test() pkgload resolves this to the
# source inst/examples/; under R CMD check, to the installed copy.
example_file <- function(...) {
  system.file("examples", ..., package = "hitop", mustWork = TRUE)
}
