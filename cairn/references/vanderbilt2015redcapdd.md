# vanderbilt2015redcapdd — REDCap uses a dictionary's field names as the data export's column names

**Provenance.** Ingested 2026-09-22 by M108 from
`cairn/references/sources/vanderbilt2015redcapdd.pdf` (gitignored), fetched from
https://cri.uchicago.edu/wp-content/uploads/2015/12/REDCap-Data-Dictionary.pdf by the
M108 session. The file is a University of Chicago Center for Research Informatics copy
of a Vanderbilt guide: its first page reads "© 2015 Vanderbilt University", and its page
footers read "University of Chicago | Center for Research Informatics | REDCap Data
Dictionary". REDCap's own help is inside each installation behind a login. This guide
is the public vendor text the repo reached.
Pagination: PDF pages. Each footer prints its page number.
Extraction: verified 2026-09-22 against the source, each quotation below read from the pdftotext output of the shelf copy — observed 2026-09-22.

**Citation.** Vanderbilt University (2015). *Creating a Data Dictionary in REDCap*
[guide, copy hosted by the University of Chicago Center for Research Informatics,
created 2015-12-08]. No author or version line is printed.

**Role.** Settles that the `Variable / Field Name` values of a REDCap data dictionary
are the column names of that project's data export. M108 relies on this for the
descriptor's `columns` field: the names `generate_redcap_hitopsr()` writes into its
dictionary are the names `score_hitopsr()` reads from the export when `items` is
omitted.

## Extracted values

- Column A, Variable/Field Name: "Variable/Field names specify the variable name that
  will be used in reporting, data export, and data analysis." p. 1, item 1.
- What breaks the link: "Remember that if you change a variable name, you will lose all
  data entered under the old variable name." p. 1, item 10. A field renamed in REDCap
  after import therefore exports under its new name, and the descriptor still records
  the old one.
- Radio fields are one of the field types the dictionary defines: "radio buttons - radio
  buttons for each choice." p. 1, Column D, item e. The guide does not describe how an
  export splits a checkbox field into columns. The HiTOP-SR dictionary writes no
  checkbox fields, so M108 does not rely on that.

## Traces to

- `R/generate_redcap.R`, the `descriptor` argument of `generate_redcap_hitopsr()`: the
  claim that the descriptor's `columns` are the export's column names.
- `R/module_file.R`, `?write_module` and `?read_module`: the `columns` field.
- `tests/testthat/test-generator-descriptor.R`: the test compares `columns` to the
  dictionary's field names. This page carries the step from those names to the
  export's columns.

## Open questions

- The guide is from 2015. It names no export option that changes the column names, for
  example a label header chosen at export time. Nobody checked a current REDCap release
  in a live project — observed 2026-09-22.
