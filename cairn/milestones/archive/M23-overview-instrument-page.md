# M23: Overview instrument page (SR/BR/HSUM link cards) (done 2026-07-17)

- **Goal:** a single beautified pkgdown page of at-a-glance summary link-cards for
  the three main HiTOP self-report measures (SR, BR, HSUM), embeddable by URL on
  hitop-system.org in place of the three separate embeds.
- **PR:** https://github.com/jmgirard/hitop/pull/25 (squash-merged 2026-07-17);
  planned + implemented + reviewed same day. Absorbed the "Overview instrument
  page" candidate (lineage M21).
- **Shipped:** new `vignettes/articles/overview.Rmd` — three summary cards (icon,
  name, item/scale summary, description, "View & download →" link to each download
  page), a static `{=html}` row reusing the M21 `.hitop-downloads` card/theme
  classes (no new helper, no inline `<style>`, full-width + light/dark via unchanged
  `pkgdown/extra.css`). Added as the **first** "Instruments" navbar entry (with a
  `---` separator, mirroring M22's Tutorials pattern). New `test-overview.R` locks
  page presence + exactly the three instrument-page links via `expect_setequal`.
  NEWS entry. Scope: three main HiTOP instruments only (PID-5 excluded); downloads
  stay on the per-instrument pages; external iframe markup out of scope.
- **Review:** AC1–AC6 fresh-evidenced (built HTML + light/dark screenshots +
  navbar-order parse + test); gate clean (`cairn_validate` 0, `document()` no-diff,
  `check_pkgdown()` clean, `check()` 0/0/0, `test()` 0 fail/9663 pass); CI 7/7.
  3-lens: 1 finding scored 85, fixed (HSUM "3 staged modules" → "3 assessment
  stages"); blame-history + prior-PR clean. No IP/GP changed (worked under GP3).
- **Note:** stale on-disk `vignettes/.quarto/_freeze/` dirs deleted locally during
  planning; never git-tracked, so the "purge git-tracked freeze" candidate was
  dropped as moot.
