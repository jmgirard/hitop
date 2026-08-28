# HiTOP-SR Definitions

Brief clinician and client-facing definitions of each scale and subscale
in the HiTOP-SR

## Usage

``` r
hitopsr_definitions
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 93
rows and 5 columns:

- Scale:

  The name of the scale

- Subscale:

  The name of the subscale (or NA if not a subscale)

- Brief:

  The brief clinician-facing definition (10-20 words)

- Client:

  The client-facing definition with examples (30-40 words)

- camelCase:

  The camel case name of whatever the row defines: the subscale where
  there is one, otherwise the scale. Matches
  [hitopsr_scales](https://jmgirard.github.io/hitop/reference/hitopsr_scales.md)\$camelCase
  on the scale rows and
  [hitopsr_subscales](https://jmgirard.github.io/hitop/reference/hitopsr_subscales.md)\$camelCase
  on the subscale rows.

## Examples

``` r
hitopsr_definitions
#> # A tibble: 93 × 5
#>    Scale                Subscale Brief                          Client camelCase
#>    <chr>                <chr>    <chr>                          <chr>  <chr>    
#>  1 Agoraphobia          NA       Fear and avoidance of situati… Being… agorapho…
#>  2 Antisocial Behavior  NA       Behavior that goes against so… Doing… antisoci…
#>  3 Appearance Focus     NA       Excessive focus on appearance… Stron… appearan…
#>  4 Appetite Loss        NA       Decreased appetite and food i… Reduc… appetite…
#>  5 Binge Eating         NA       Eating unusually large amount… Episo… bingeEat…
#>  6 Bodily Distress      NA       Physical symptoms like pain, … Physi… bodilyDi…
#>  7 Body Dissatisfaction NA       Persistent dissatisfaction wi… Ongoi… bodyDiss…
#>  8 Callousness          NA       Disregard for others' feeling… A gen… callousn…
#>  9 Checking             NA       Repetitive urges to check tas… Havin… checking 
#> 10 Cleaning             NA       Fear of dirt and germs, leadi… Fear … cleaning 
#> # ℹ 83 more rows
```
