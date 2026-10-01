# github2026rawheaders: the response headers GitHub sends with a raw file

**Provenance.** First-hand record, made 2026-10-01 at M152 with `curl -sI`. The two files read were `https://raw.githubusercontent.com/jmgirard/hitop-form/main/README.md` and `https://raw.githubusercontent.com/jmgirard/hitop/main/.claude/launch.json`. The README was read with and without an `Origin: https://jmgirard.github.io` request header. No file is on the shelf.
Pagination: none.
Extraction: the header values below are copied from the responses as received on 2026-10-01 (observed 2026-10-01). GitHub documents none of them, so a re-check means running the same `curl -sI` again.

**Citation.** GitHub (2026). Response headers of `raw.githubusercontent.com`, observed 2026-10-01. No document.

**Role.** hitop-form's online form fetches a study's setup file from another site, and the Study Link Builder fetches it before it makes a link. This page records that a GitHub raw-file address lets other sites read the file. It also records how long GitHub can keep serving an old copy after the file is edited. The hitop-form README section on hosted setup files depends on it.

## Extracted values

- Other sites can read the file. Both responses carry `access-control-allow-origin: *`, with and without an `Origin` request header.
- The cache time. Both responses carry `cache-control: max-age=300` and an `expires` time 5 minutes after their `date`. The responses also carry `via: 1.1 varnish` and `x-cache`, so a cache in front of GitHub serves them.
- The file type. The `.json` file and the `.md` file both come back as `content-type: text/plain; charset=utf-8`. The online form reads the bytes and does not look at this header.

## Traces to

- `cairn/milestones/M152-study-link-setup-file.md` AC6: the README's statement that GitHub can serve the old copy of an edited file for the time its cache header states.
- hitop-form `README.md`, the section on hosted setup files.

## Open questions

- Whether the cache in front of GitHub keeps an old copy for longer than `max-age` after an edit. The online form asks for the file with `cache: 'no-store'`, which skips the browser's own cache only. Nothing here was measured across an edit (observed 2026-10-01).
