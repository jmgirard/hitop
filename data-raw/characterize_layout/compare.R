# AC1: every recorded value identical() across the merge-base and branch runs.
base <- readRDS("base.rds")
head <- readRDS("head.rds")
cat("calls recorded: base", length(base), " head", length(head), "\n")
stopifnot(length(base) == length(head))
same <- vapply(seq_along(base), function(i) identical(base[[i]], head[[i]]), logical(1))
cat("identical:", sum(same), "of", length(same), "\n")
cat("by function:\n")
print(table(vapply(base, `[[`, "", "fn")))
cat("errors recorded:", sum(vapply(base, function(r) !is.null(r$error), logical(1))), "\n")
if (!all(same)) {
  cat("first differing call:", which(!same)[[1]], "\n")
  str(base[[which(!same)[[1]]]], max.level = 2)
  str(head[[which(!same)[[1]]]], max.level = 2)
  quit(status = 1)
}
cat("AC1: all identical\n")
