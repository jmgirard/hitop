
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hitop <a href="https://jmgirard.github.io/hitop/"><img src="man/figures/logo.png" align="right" height="138" alt="hitop website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jmgirard/hitop/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jmgirard/hitop/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jmgirard/hitop/graph/badge.svg)](https://app.codecov.io/gh/jmgirard/hitop)
<!-- badges: end -->

The goal of the **hitop** package is to provide an open-source toolkit
of functions and resources tailored for the
<a href="https://www.hitop-system.org/" target="_blank">Hierarchical
Taxonomy of Psychopathology (HiTOP)</a> community. While the package is
currently optimized to support researchers in managing large-scale
assessment data, future development will expand its features to support
clinical workflows and individual practitioner needs.

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

- [ ] Add Instrument Data
  - [x] HiTOP-SR (405)
  - [x] HiTOP-BR (45)
  - [x] HiTOP-HSUM (650)
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
  - [x] PID-5-BF (25)
  - [ ] PID-5-BFP (36) - *todo*
- [ ] Add Scoring Functions
  - [x] HiTOP-SR (405)
  - [x] HiTOP-BR (45)
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
  - [x] PID-5-BF (25)
  - [ ] HiTOP-HSUM (650) - *waiting for feedback*
  - [ ] PID-5-BFP (36) - *todo*
- [x] Add Validity Functions
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
- [ ] Add Reliability Functions
  - [x] HiTOP-SR (405)
  - [x] HiTOP-BR (45)
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
  - [x] PID-5-BF (25)
  - [ ] HiTOP-HSUM (650) - *waiting for feedback*
  - [ ] PID-5-BFP (36) - *todo*
- [ ] Add Scoring Tutorials
  - [x] HiTOP-SR (405)
  - [x] HiTOP-BR (45)
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
  - [x] PID-5-BF (25)
  - [ ] HiTOP-HSUM (650) - *waiting for feedback*
  - [ ] PID-5-BFP (36) - *todo*
- [ ] Add Instrument Export Functions
  - [x] HiTOP-SR (405)
  - [x] HiTOP-BR (45)
  - [x] HiTOP-HSUM (650)
  - [x] PID-5 (220)
  - [x] PID-5-SF (100)
  - [x] PID-5-BF (25)
  - [ ] PID-5-BFP (36) - *todo*

### Phase 2

- [ ] Add Normative Data (for PID-5) - *todo*
- [ ] Add Norming Functions (for PID-5) - *todo*
- [ ] Add Visualization Functions - *todo*
- [x] Add Package Unit Testing

### Phase 3

- [ ] Add Normative Data (for HiTOP) - *waiting for data*
- [ ] Add Norming Functions (for HiTOP) - *waiting for data*
- [ ] Add Individual Report Generation
- [ ] Update Package Unit Testing
- [ ] Submit to CRAN
- [ ] Write Package Paper
