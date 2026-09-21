# The latest hitop_artifacts row per file describes the currently distributed
# file. Shared by test-artifacts.R and test-json-export.R.
latest_manifest <- function() {
  m <- hitop_artifacts[order(hitop_artifacts$file, hitop_artifacts$build_date), ]
  m[!duplicated(m$file, fromLast = TRUE), ]
}
