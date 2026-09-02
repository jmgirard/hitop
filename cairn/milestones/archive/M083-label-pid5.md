# M083: `label_pid5()`

**Status:** done (2026-09-02, PR #90 https://github.com/jmgirard/hitop/pull/90)

**Goal:** Ship `label_pid5()`, so PID-5 item and scale columns carry their questionnaire text and
display names as `label` attributes, as the two HiTOP instruments' data already does.

**Outcome:** `R/label_pid5.R` exports `label_pid5(data, target, version, prefix)`:
`target = "items"` attaches each item's `pid_items$Text` as that column's `label`;
`target = "scales"` the display name from `pid_scales[[version]]` plus, for FULL and SF,
`pid_domains`. `prefix = NULL` resolves to `pid5_`/`pid5sf_`/`pid5bf_` for items, `pid_` for
scales. A form-shaped item column not padded to its width goes unlabelled, reported by the
reused `unpadded_item_cols()`/`warn_unpadded_items()` (`R/util.R:590`) under class
`hitop_unpadded_items`. `test-label_pid5.R` covers all three forms at 530 expectations; docs, a
NEWS bullet, a `_pkgdown.yml` row and one mention per PID-5 scoring vignette carry it.

**Decisions:** none cross-cutting. Milestone-local: one function with `prefix = NULL` over
`label_pid5_items()`/`label_pid5_scales()`, which would break the `label_*(data, target, prefix)`
idiom; `validity_pid5()`'s columns left unlabelled, D-018 fixing those names to papers not on the
shelf (IP2); the no-columns-matched report left unclassed, matching both HiTOP siblings.

**Review:** three-lens fan-out; blame-history and prior-review returned nothing, the diff-bug lens
ten reproduced findings. All six criteria passed on fresh evidence. Two fixed on the branch
(`@return` silent on the untouched-frame path, `@param prefix` overstating the unpadded rule);
three rejected as shared with the siblings; four filed to the candidate row.
