# fastly2026limits: the URL length the online form's host accepts

**Provenance.** Ingested 2026-10-01 at M153 from `https://docs.fastly.com/en/guides/resource-limits`, read with WebFetch that day, and from a first-hand `curl` measurement of GitHub Pages the same day. No file is on the shelf.
Pagination: none. The page is one HTML page, cited by its table row.
Extraction: the Fastly row below was copied from the page text on 2026-10-01, and the measurement was run on 2026-10-01 (observed 2026-10-01). A re-check reads the page again and runs the same `curl` loop again.

**Citation.** Fastly (2026). *Network services resource limits*, "Request and response limits" table. https://docs.fastly.com/en/guides/resource-limits. And GitHub Pages responses to `https://jmgirard.github.io/hitop-form/?c=…`, observed 2026-10-01. No document.

**Role.** GitHub Pages hosts hitop-form's online form, and Fastly serves GitHub Pages. hitop-form's Study Link Builder refuses a link whose path and query are longer than 8,192 characters (`HOST_PATH_MAX` in `link.html`). This page records the documented limit and the measurement behind that number.

## Extracted values

- Fastly, "Request and response limits": "URL size | 8KB | Exceeding the limit results in a `414 URI Too Long` error."
- A response from `https://jmgirard.github.io/hitop-form/` carries `x-fastly-request-id`, `via: 1.1 varnish` and an `x-served-by: cache-…` header, and `server: GitHub.com`. So Fastly serves the page.
- Measurement: `curl -s -o /dev/null -w '%{http_code}'` of `https://jmgirard.github.io/hitop-form/?c=` followed by a run of `A` characters. The path and query (from `/hitop-form/` to the end) were 8,185, 8,191 and 8,192 characters long, and each got 200. At 8,193, 8,195, 8,200, 8,205 and 8,215 characters, each got 414.
- The Fastly page does not say whether 8 KB means 8,000 or 8,192 bytes, or which part of the request it counts. The measurement shows 8,192 characters of path and query.

## Traces to

- `cairn/milestones/M153-question-limit-link-length.md` AC6.
- hitop-form `link.html`: `HOST_PATH_MAX` and the refusal of a link longer than the online form's host accepts.
- hitop-form `README.md` and hitop's `vignettes/articles/online-collection.Rmd`: the host's link limit.

## Open questions

- Whether a recruiting site or mail program adds characters to a link before a participant opens it, beyond Prolific's IDs. Nothing here was measured (observed 2026-10-01).
