# Bind the package's datasets into its namespace.
#
# Functions in this package read their tables by bare name (`hitopsr_scales`,
# `pid_items`, ...). An installed package keeps its datasets in the namespace's
# separate "lazydata" store, which R puts on the search path only when the
# package is attached. So `hitop::hitop_module(...)` in a session that never
# called `library(hitop)` could not find `hitopsr_scales`. Each dataset is bound
# here as a promise that reads it from that store on first use, so no data is
# loaded until a function needs it. `devtools::load_all()` already places the
# datasets in the namespace, and a name already bound is left as it is.
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  lazydata <- tryCatch(
    getNamespaceInfo(ns, "lazydata"),
    error = function(cnd) NULL
  )
  if (!is.environment(lazydata)) {
    return(invisible())
  }
  for (name in ls(lazydata, all.names = TRUE)) {
    if (!exists(name, envir = ns, inherits = FALSE)) {
      bind_lazy_dataset(name, lazydata, ns)
    }
  }
  invisible()
}

# Internal Helper: bind one dataset in `ns` as a promise reading `store`.
#
# A function of its own so that each promise is evaluated in this call's
# frame, where `name` is fixed. A promise built in .onLoad()'s loop would read
# the loop variable, and every dataset would resolve to the last one bound.
bind_lazy_dataset <- function(name, store, ns) {
  force(name)
  force(store)
  delayedAssign(name, get(name, envir = store), assign.env = ns)
}
