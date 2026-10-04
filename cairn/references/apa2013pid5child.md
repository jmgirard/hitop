# apa2013pid5child: the APA PID-5 child forms (ages 11 to 17), full and brief

**Provenance.** Ingested 2026-10-04 by M161 from two gitignored shelf copies.
`cairn/references/sources/apa2013pid5child.pdf` (sha256
`6015387653f59fc357ac95a389577b414da4eb1aed03b83b8967359589d4a773`) is the full
form. PDF metadata: title "The personality inventory for DSM-5 - Child age
11-17", created 2022-03-09, modified 2022-07-14, 9 pages.
`cairn/references/sources/apa2013pid5bfchild.pdf` (sha256
`1aa54248a6d60edc2a19609427c35ccb940eed71801ea3e9043de2e8377af2db`) is the brief
form. PDF metadata: title "The personality inventory for DSM-5 - Brief dorm -
Child age 11-17" (sic), created 2022-03-09, modified 2022-07-14, 3 pages.
Pagination: PDF pages. In both files, page 1 is the APA rights page. The full
form's pages 2 to 7 are the items, which the form numbers "Page 1" to "Page 6".
Page 8 is the scoring key and page 9 the instructions to clinicians. The brief
form's page 2 is the items, and page 3 is the domain key and the instructions to
clinicians.
Extraction: verified 2026-10-04 by `data-raw/check_pid_child_text.R` against both files, all 220 + 25 item texts and the full form's key — observed 2026-10-04.

**Citation.** Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., &
Skodol, A. E. (2013). *The Personality Inventory for DSM-5 (PID-5)—Child Age
11–17* and *The Personality Inventory for DSM-5—Brief Form (PID-5-BF)—Child Age
11–17*. American Psychiatric Association. The authors are the ones the form
footers name.

**Role.** The source for the child instructions in `R/sysdata.rda` and for the
claim that the existing `FULL` and `BF` keying scores the child forms.

## Comparison with the package's adult tables

`data-raw/check_pid_child_text.R` ran on 2026-10-04 and found no difference. It
reads both files with `pdftotext -raw` and checks:

1. The 220 full-form and 25 brief-form item texts, in order, against
   `pid_items$Text` by `FULL` and `BF` number.
2. The full form's Step 1 reverse list and its Facet Table R marks against
   `pid_items$Reverse`.
3. Its Facet Table item lists against `pid_scales$FULL`.
4. Its Domain Table primary facets against `pid_domains`.
5. The brief form's Domain Scoring table against `pid_scales$BF`.
6. Each form's stored first-page paragraph, footer notice and response labels
   (`pid_child_instructions` in `R/sysdata.rda`) against its PDF text. This
   check was added on 2026-10-04, with three planted defects that went red.

Texts must match exactly after whitespace and typographic quotes are
normalized. The script also drops a final period from the PDF text, because
`pid_items$Text` stores none for any form. The script exits non-zero on any
difference. On 2026-10-04, a planted defect of each of seven kinds went red.
The kinds were a changed text, two swapped full-form items, two swapped
brief-form items and a flipped reverse flag. The others were a changed facet
item, a changed BF domain item and a reordered domain facet list.

Differences found: none.

## Extracted values

### Full form (pp. 2–9)

The page-2 instructions read: "Instructions to the child receiving care: This is
a list of things different people might say about themselves. We are interested
in how you would describe yourself. There are no “right” or “wrong” answers. So
you can describe yourself as honestly as possible, we will keep your responses
confidential. We’d like you to take your time and read each statement carefully,
selecting the response that best describes you."

Pages 3 to 7 begin "Instructions to child receiving care: Please continue to
complete the questionnaire. Remember, this is a list of things different people
might say about themselves." The rest of that paragraph repeats the page-2 text
from "We are interested".

The response options are 0 "Very False or Often False", 1 "Sometimes or Somewhat
False", 2 "Sometimes or Somewhat True" and 3 "Very True or Often True", the
adult labels in `pid_instructions$options`. The page-2 header asks for
"Name/ID", "Age" and "Date". Later headers ask for "Name/ID (child receiving
care)". Each item page's footer reads "Krueger RF, Derringer J, Markon KE,
Watson D, Skodol AE. Copyright © 2013 American Psychiatric Association. All
rights reserved. This material can be reproduced without permission by
researchers and by clinicians for use with their patients."

Step 1 of the key (p. 8) reverses 16 items: 7, 30, 35, 58, 87, 90, 96, 97, 98,
131, 142, 155, 164, 177, 210 and 215. The Facet Table marks the same 16 with R.
The page-9 scoring rule matches the adult key's rule, and it says "round to the
nearest whole number". The page-9 instructions describe a "220 item self-rated
personality trait assessment scale for children ages 11 to 17".

### Brief form (pp. 2–3)

The page-2 instructions read: "Instructions: This is a list of things different
people might say about themselves. We are interested in how you would describe
yourself. There are no right or wrong answers. So you can describe yourself as
honestly as possible, we will keep your responses confidential. We’d like you to
take your time and read each statement carefully, selecting the response that
best describes you."

The response options are the full form's four labels. The header asks for
"Name", "Age" and "Date". The footer reads "Krueger RF, Derringer J, Markon KE,
Watson D, Skodol AE. Copyright © 2013 American Psychiatric Association. All
Rights Reserved. This material can be reproduced without permission by
researchers and by clinicians for use with their patients." The page-3 key lists
the 5 items of each domain and marks no item R.

### Difference from the adult instructions

`pid_instructions$start` holds the adult text. The brief child form's paragraph
equals it once the apostrophe is normalized. The full child form adds the label
"Instructions to the child receiving care:" and puts "right" and "wrong" in
quotation marks.

## Traces to

- `data-raw/check_pid_child_text.R` (the comparison against these files).
- The child instruction entries in `data-raw/sysdata.R` (`R/sysdata.rda`).

## Open questions

None.
