# hitop

The goal of the **hitop** package is to provide an open-source toolkit
of functions and resources tailored for the [Hierarchical Taxonomy of
Psychopathology (HiTOP)](https://www.hitop-system.org/) community. While
the package is currently optimized to support researchers in managing
large-scale assessment data, future development will expand its features
to support clinical workflows and individual practitioner needs.

### Key Features

- **Scoring & Utilities:** Automated scoring algorithms and
  data-cleaning functions for multiple instrument variants, including
  the Personality Inventory for DSM-5 (PID-5, PID-5-SF, PID-5-BF), the
  HiTOP Self-Report (HiTOP-SR), the HiTOP Brief Report (HiTOP-BR), and
  the HiTOP Harmful Substance Use Module (HiTOP-HSUM).
- **Instrument Downloads:** Direct access to downloadable assessment
  resources, including standard paper forms, as well as ready-to-import
  configuration files for popular data collection platforms like
  Qualtrics and REDCap.
- **Comprehensive Metadata:** Embedded psychometric metadata, including
  explicit scale definitions, scoring keys, and item-level text to
  streamline documentation and reproducible workflows.

## Installation

You can install the development version of hitop from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pak("jmgirard/hitop")
```

## Development Progress

### Phase 1

Add Instrument Data

HiTOP-SR (405)

HiTOP-BR (45)

HiTOP-HSUM (650)

PID-5 (220)

PID-5-SF (100)

PID-5-BF (25)

PID-5-BFP (36) - *todo*

Add Scoring Functions

HiTOP-SR (405)

HiTOP-BR (45)

PID-5 (220)

PID-5-SF (100)

PID-5-BF (25)

HiTOP-HSUM (650) - *waiting for feedback*

PID-5-BFP (36) - *todo*

Add Validity Functions

PID-5 (220)

PID-5-SF (100)

Add Reliability Functions

HiTOP-SR (405)

HiTOP-BR (45)

PID-5 (220)

PID-5-SF (100)

PID-5-BF (25)

HiTOP-HSUM (650) - *waiting for feedback*

PID-5-BFP (36) - *todo*

Add Scoring Tutorials

HiTOP-SR (405)

HiTOP-BR (45)

PID-5 (220)

PID-5-SF (100)

PID-5-BF (25)

HiTOP-HSUM (650) - *waiting for feedback*

PID-5-BFP (36) - *todo*

Add Instrument Export Functions

HiTOP-SR (405)

HiTOP-BR (45)

HiTOP-HSUM (650)

PID-5 (220)

PID-5-SF (100)

PID-5-BF (25)

PID-5-BFP (36) - *todo*

### Phase 2

Add Normative Data (for PID-5) - *todo*

Add Norming Functions (for PID-5) - *todo*

Add Visualization Functions - *todo*

Add Package Unit Testing

### Phase 3

Add Normative Data (for HiTOP) - *waiting for data*

Add Norming Functions (for HiTOP) - *waiting for data*

Add Individual Report Generation

Add Bass-Ackwards Functions

Add Extended Bass-Ackwards Functions

Update Package Unit Testing

Submit to CRAN

Write Package Paper
