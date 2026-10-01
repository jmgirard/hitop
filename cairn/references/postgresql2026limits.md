# postgresql2026limits: PostgreSQL's limits on a table's columns and a row's size

**Provenance.** Ingested 2026-10-01 at M153 from `https://www.postgresql.org/docs/current/limits.html`, which served the PostgreSQL 18 documentation. No file is on the shelf.
Pagination: none. The page is one HTML page, cited by its appendix and table.
Extraction: the values below were copied from the page text on 2026-10-01 (observed 2026-10-01). A re-check reads the same page again.

**Citation.** The PostgreSQL Global Development Group (2026). *PostgreSQL 18 Documentation*, Appendix K, "PostgreSQL Limits", Table K.1. https://www.postgresql.org/docs/current/limits.html

**Role.** Supabase stores data in PostgreSQL. hitop-form's Study Link Builder writes the SQL for a Supabase table with one column per lead field, item and question. Since M153 a link holds any number of questions that fit in 100,000 bytes, so the table's size limits now bind first. This page records those limits.

## Extracted values

- Columns per table: 1,600 (Table K.1). The table's comment says this is further limited by the row fitting on a single page.
- A row must fit in one 8,192-byte heap page (the note under Table K.1). Its example: 1,600 `int` columns take 6,400 bytes and fit, and 1,600 `bigint` columns take 12,800 bytes and do not.
- A `text` value large enough can be stored out of line in the table's TOAST table, and 18 bytes stay in the row. A shorter `text` value is stored in the row with a 1-byte or 4-byte header (the same note).
- Dropped columns still count toward the 1,600 (the same note).
- Identifier length: 63 bytes (Table K.1).

## Traces to

- `cairn/milestones/M153-question-limit-link-length.md` AC2 and AC4.
- hitop-form `form.js` `POSTGRES_COLUMNS_MAX` and `storeColumns()`, and `link.html`'s refusal of a Supabase table over 1,600 columns.
- hitop-form `README.md` and hitop's `vignettes/articles/online-collection.Rmd`: the column limit and the row-size limit.

## Open questions

- How many long text answers a row with many question columns can hold before an insert fails. The 8,192-byte page and the 18-byte out-of-line pointer bound it, and nothing here was measured on Supabase (observed 2026-10-01).
