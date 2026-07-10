# hitop 0.1.0

* Add initial HiTOP-HSUM functions
* Add data export functions
* Build out phase 1 website
* `score_pid5()` now returns the 5 personality-trait domain scores for the FULL
  and SF versions (APA scoring key Step 3), appended after the 25 facet scores
* Add the `pid_domains` dataset (the domain to primary-facet map used for FULL/SF
  domain scoring)
* `score_pid5()` gains an `apa_scoring` argument (default `TRUE`) that applies the
  published APA missing-data and proration rule: a facet (or BF domain) with more
  than 25% of its items unanswered is set to `NA`; otherwise the raw score is
  prorated to the full item count and rounded before averaging, and a FULL/SF
  domain is `NA` if any contributing facet is `NA`. **This changes the default
  scored output under missing data** (previously `rowMeans(na.rm = TRUE)` averaged
  whatever items were present). Pass `apa_scoring = FALSE` to restore the previous
  behavior. Under `apa_scoring = TRUE`, `na.rm` is ignored (with a warning if set
  to `FALSE`), and any standard error is `NA` wherever its scale score is `NA`
* Fix `validity_pid5()` erroring on single-row input for the FULL and SF forms
* Fix `score_pid5(calc_se = TRUE)` erroring on single-row input

# hitop 0.0.2

* Add initial HiTOP-SR and BR functions

# hitop 0.0.1

* Add initial PID-5 functions
