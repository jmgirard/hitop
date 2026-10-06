# fastly2026limits: the URL length the online form's host accepts

**Provenance.** Ingested 2026-10-01 at M153 from `https://docs.fastly.com/en/guides/resource-limits`, read with WebFetch that day, and from a first-hand `curl` measurement of GitHub Pages the same day. No file is on the shelf.
Pagination: none. The page is one HTML page, cited by its table row.
Extraction: the Fastly row below was copied from the page text on 2026-10-01, and the measurement was run on 2026-10-01 (observed 2026-10-01). Re-checked 2026-10-06 at M168: the Fastly row reads the same, and the re-measurement below replaces the limit (observed 2026-10-06). A re-check reads the page again and repeats the re-measurement on cache hits and on cache misses. The weekly deployed-page run of hitop-form's tests repeats the miss requests (`tests/host-limit.spec.js`).

**Citation.** Fastly (2026). *Network services resource limits*, "Request and response limits" table. https://docs.fastly.com/en/guides/resource-limits. And GitHub Pages responses to `https://jmgirard.github.io/hitop-form/?c=…` and variant paths, observed 2026-10-01 and 2026-10-06. No document.

**Role.** GitHub Pages hosts hitop-form's online form, and Fastly serves GitHub Pages from a cache. On a request the cache does not answer, GitHub's server refuses a path and query longer than 8,177 characters. hitop-form's Study Link Builder refuses a link whose count is longer than that (`HOST_PATH_MAX` in `link.html`, 8,177 since M168). This page records the documented limit and the measurements behind that number.

## Extracted values

- Fastly, "Request and response limits": "URL size | 8KB | Exceeding the limit results in a `414 URI Too Long` error."
- A response from `https://jmgirard.github.io/hitop-form/` carries `x-fastly-request-id`, `via: 1.1 varnish` and an `x-served-by: cache-…` header, and `server: GitHub.com`. So Fastly serves the page.
- Measurement, 2026-10-01 (M153): `curl -s -o /dev/null -w '%{http_code}'` of `https://jmgirard.github.io/hitop-form/?c=` followed by a run of `A` characters. The path and query (from `/hitop-form/` to the end) were 8,185, 8,191 and 8,192 characters long, and each got 200. At 8,193, 8,195, 8,200, 8,205 and 8,215 characters, each got 414. The cache status was not recorded.
- The Fastly page does not say whether 8 KB means 8,000 or 8,192 bytes, or which part of the request it counts.

## Re-measurement, 2026-10-06 (M168)

Each request is `curl` of `https://jmgirard.github.io` plus a path, `?c=` and a run of `A` characters, made 20:54 to 21:00 UTC. The length is the path and query, from the path's first `/` to the end. The `x-cache` column is that response header. "Absent" marks a response that had none. A cached response does not depend on the query, because the cache's key leaves the query out. Each `x-cache: HIT` response carried the same `age` as its neighbours whatever its query. A 400 is not cached, so a refused miss leaves the next request a miss too.

Paths: R is `/hitop-form/`, the path of every study link. V*k* is `/hitop-form/` followed by *k* more slashes and `index.html`. GitHub's server answers it with the same page, under a cache key no visitor uses. I0 is `/hitop-form/index.html`, I2 is `/hitop-form//index.html` and I. is `/hitop-form/./index.html`, each sent with `--path-as-is`.

| Path | Length | Status | x-cache |
|---|---|---|---|
| I0 | 8,185 | 400 | MISS |
| I0 | 8,185 | 400 | MISS |
| I2 | 8,185 | 400 | MISS |
| I2 | 8,185 | 400 | MISS |
| I. | 8,185 | 400 | MISS |
| I. | 8,185 | 400 | MISS |
| V1 | 6,092 | 200 | MISS |
| V2 | 7,138 | 200 | MISS |
| V3 | 7,661 | 200 | MISS |
| V4 | 7,923 | 200 | MISS |
| V5 | 8,054 | 200 | MISS |
| V6 | 8,119 | 200 | MISS |
| V7 | 8,152 | 200 | MISS |
| V8 | 8,168 | 200 | MISS |
| V9 | 8,176 | 200 | MISS |
| V10 | 8,180 | 400 | MISS |
| V10 | 8,178 | 400 | MISS |
| V10 | 8,177 | 200 | MISS |
| V21 | 8,178 | 400 | MISS |
| V21 | 8,177 | 200 | MISS |
| V22 | 8,178 | 400 | MISS |
| V22 | 8,177 | 200 | MISS |
| V23 | 8,178 | 400 | MISS |
| V23 | 8,178 | 400 | MISS |
| R (query of `B`) | 8,000 | 200 | HIT |
| R (query of `B`) | 8,100 | 200 | HIT |
| R (query of `B`) | 8,150 | 200 | HIT |
| R (query of `B`) | 8,192 | 200 | HIT |
| R (query of `C`) | 8,000 | 200 | HIT |
| R (query of `C`) | 8,100 | 200 | HIT |
| R (query of `C`) | 8,150 | 200 | HIT |
| R (query of `C`) | 8,192 | 200 | HIT |
| R (query of `D`) | 8,000 | 200 | HIT |
| R (query of `D`) | 8,100 | 200 | HIT |
| R (query of `D`) | 8,150 | 200 | HIT |
| R (query of `D`) | 8,192 | 200 | HIT |
| R | 8,178 | 400 | MISS |
| R | 8,177 | 200 | MISS |
| R | 8,178 | 200 | HIT |
| R | 8,192 | 200 | HIT |
| R | 8,193 | 414 | absent (`server: Varnish`) |
| V1039 | 8,178 | 400 | MISS |
| V1039 | 8,177 | 200 | MISS |
| V517 | 8,178 | 400 | MISS |
| V517 | 8,177 | 200 | MISS |
| V41 | 8,178 | 400 | MISS |
| V41 | 8,177 | 200 | MISS |

On a miss, every length up to 8,177 got 200 and every longer length got 400, on R and on the V paths. On a hit, 8,192 got 200 and 8,193 got Fastly's 414. So a link of 8,178 to 8,192 characters opens only while the page is cached, which lasts 600 seconds after a request (`cache-control: max-age=600`). The 15 characters between the two limits equal the length of `GET ` and ` HTTP/1.1` around a path (13 characters) and the line's closing CR LF (2). The V41, V517 and V1039 rows, made at M168's review, cover the range of slashes the weekly H1 test draws from. So GitHub's server possibly counts its request line to 8,192. That is an observation, not a measured fact.

Other requests that day recorded no `x-cache`, so they are not part of this re-measurement. Among them were two loops on R that sent every length from 8,185 to 8,215. The first got 400 at 8,185 to 8,192 and 414 above. The second, a few minutes later, got 200 at 8,185 to 8,192 and 414 above. Both fit the table: the first met a cold cache, the second a cached page.

## Traces to

- `cairn/milestones/M153-question-limit-link-length.md` AC6.
- `cairn/milestones/M168-link-length-gaps.md` AC4 and T5: the re-measurement, the move of `HOST_PATH_MAX` to 8,177, and the weekly host test.
- hitop-form `link.html`: `HOST_PATH_MAX` and the refusal of a link longer than the online form's host accepts.
- hitop-form `README.md` and hitop's `vignettes/articles/online-collection.Rmd`: the host's link limit.

## Open questions

- Whether a recruiting site or mail program adds characters to a link before a participant opens it, beyond Prolific's IDs and SONA's code. Nothing here was measured (observed 2026-10-01). Whether CloudResearch Connect appends parameters of its own has no source (observed 2026-10-06).
- Whether GitHub's server limit depends on anything other than the length of the path and query, such as request headers. Every request here was a plain `curl` (observed 2026-10-06).
