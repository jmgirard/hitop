# hitop

The goal of the **hitop** package is to provide an open-source toolkit
of functions and resources tailored for the [Hierarchical Taxonomy of
Psychopathology (HiTOP)](https://www.hitop-system.org/) community. While
the package is currently optimized to support researchers in managing
large-scale assessment data, future development will expand its features
to support clinical workflows and individual practitioner needs.

### Key Features

- Scoring functions and reliability estimates for the instruments marked
  in the table below, and helpers that rename and label item columns.
- Word forms, and import files for Qualtrics, REDCap and the online
  form, ready to download from the [package
  website](https://jmgirard.github.io/hitop/).
- Item text, scoring keys and scale definitions as data tables, for
  documentation and reproducible work.

## Installation

You can install the development version of hitop from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("jmgirard/hitop")
```

## What the package covers

The PID-5 is the Personality Inventory for DSM-5. HiTOP-SR, HiTOP-BR and
HiTOP-HSUM are the HiTOP Self-Report, the HiTOP Brief Report and the
HiTOP Harmful Substance Use Module. The two child forms are for ages 11
to 17.

Items is the number of items. Scoring and Reliability mark the
instruments that the `score_*()` and `reliability_*()` functions take.
Tutorial links each scoring tutorial that shows the scoring call for the
instrument. The child forms use the call of their adult form. Forms
lists the download formats: Word forms, Qualtrics and REDCap import
files, and JSON files for the online form.

| Instrument | Items | Scoring | Reliability | Tutorial | Forms |
|----|----|----|----|----|----|
| HiTOP-SR | 405 | Yes | Yes | [Scoring HiTOP-SR](https://jmgirard.github.io/hitop/articles/hitopsr_scoring.html) | Word, Qualtrics, REDCap, JSON |
| HiTOP-BR | 45 | Yes | Yes | [Scoring HiTOP-BR](https://jmgirard.github.io/hitop/articles/hitopbr_scoring.html) | Word, Qualtrics, REDCap, JSON |
| HiTOP-HSUM | 650 |  |  |  | Word, Qualtrics, REDCap |
| PID-5 | 220 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html) | Word, Qualtrics, REDCap, JSON |
| PID-5-SF | 100 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html), [Scoring PID-5-SF](https://jmgirard.github.io/hitop/articles/pid5sf_scoring.html) | Word, Qualtrics, REDCap, JSON |
| PID-5-BF | 25 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html), [Scoring PID-5-BF](https://jmgirard.github.io/hitop/articles/pid5bf_scoring.html) | Word, Qualtrics, REDCap, JSON |
| PID5BF+M | 36 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html) | Word, Qualtrics, REDCap |
| PID-5-IRF | 218 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html) | Word, Qualtrics, REDCap |
| PID-5 Child | 220 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html) | Word, Qualtrics, REDCap |
| PID-5-BF Child | 25 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html), [Scoring PID-5-BF](https://jmgirard.github.io/hitop/articles/pid5bf_scoring.html) | Word, Qualtrics, REDCap |
| PID-5-FFBF | 100 | Yes | Yes | [Scoring PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.html) |  |

For PID-5 validity scales and norms, see
[`?validity_pid5`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
and
[`?norm_pid5`](https://jmgirard.github.io/hitop/reference/norm_pid5.md).
