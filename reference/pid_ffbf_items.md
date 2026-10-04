# PID-5 Forensic Faceted Brief Form Item Data

The 100 items of the PID-5 Forensic Faceted Brief Form (PID-5-FFBF), an
adaptation of the PID-5 faceted short form for people in prison, with
self- and informant-report versions in English and German. Most items
are rewritten for the prison setting, so they have their own table
rather than columns of
[pid_items](https://jmgirard.github.io/hitop/reference/pid_items.md).
The form was validated in German. Its English text is the authors'
English version, from Table S3, and the study validated only the German
version (Niemeyer et al., 2022, p. 40). Items they did not adapt keep
the APA PID-5 wording. Scored by `score_pid5(version = "FFBF")`.

## Usage

``` r
pid_ffbf_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 100
rows and 7 columns:

- FFBF:

  Item number on the PID-5-FFBF (integer)

- Facet:

  Name of the PID-5 facet, spelled as in `pid_items$Facet`. Each facet
  has 4 items: facet k, in alphabetical order, holds items k, k + 25,
  k + 50, and k + 75

- Reverse:

  Whether the item needs to be reverse scored (items 12 and 26)

- Text:

  Self-report item text, English

- TextIRF:

  Informant-report item text, English, without a subject (Table S3
  prints each with a leading ellipsis)

- TextDE:

  Self-report item text, German

- TextIRFDE:

  Informant-report item text, German, without a subject

## Source

Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D. (2022).
Assessing maladaptive personality in the forensic context: Development
and validation of the Personality Inventory for DSM-5 Forensic Faceted
Brief Form (PID-5-FFBF). *Journal of Personality Assessment, 104*(1),
30-43.
[doi:10.1080/00223891.2021.1923522](https://doi.org/10.1080/00223891.2021.1923522)
. Item text from its supplement, Table S3 (<https://osf.io/fzyvr/>):
Copyright 2013 American Psychiatric Association, German version 2015,
developed with permission from Hogrefe and the APA.

## Details

The texts are those of Table S3 of the form's supplement, without its
source notes, its reverse marks, the stray markers E14, E18 and E77, a
leading ellipsis, or the final period, and with straight quotes and
apostrophes (including an acute accent used as an apostrophe in item
50). Hyphens inside German words are removed, and English words split at
a hyphen across a line are joined with the hyphen kept. The printed
wording is otherwise kept, typos included.

## Examples

``` r
pid_ffbf_items
#> # A tibble: 100 × 7
#>     FFBF Facet              Reverse Text                TextIRF TextDE TextIRFDE
#>    <int> <chr>              <lgl>   <chr>               <chr>   <chr>  <chr>    
#>  1     1 Anhedonia          FALSE   I'm not really int… is not… Ich h… zeigt an…
#>  2     2 Anxiousness        FALSE   I get very nervous… is ver… Ich w… ist sehr…
#>  3     3 Attention Seeking  FALSE   I do things to mak… does t… Ich t… tut Ding…
#>  4     4 Callousness        FALSE   I really don't car… really… Es is… kümmert …
#>  5     5 Deceitfulness      FALSE   I sometimes make u… someti… Um zu… erfindet…
#>  6     6 Depressivity       FALSE   Sometimes I think … someti… Manch… scheint …
#>  7     7 Distractibility    FALSE   I have trouble con… has tr… Es fä… hat Schw…
#>  8     8 Eccentricity       FALSE   I have been told i… thinks… Man h… hat eine…
#>  9     9 Emotional Lability FALSE   I get upset quickl… gets u… Ich g… gerät sc…
#> 10    10 Grandiosity        FALSE   To be honest: I am… thinks… Um eh… denkt, e…
#> # ℹ 90 more rows
```
