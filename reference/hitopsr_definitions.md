# HiTOP-SR Definitions

Brief clinician and client-facing definitions of each scale and subscale
in the HiTOP-SR

## Usage

``` r
hitopsr_definitions
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 93
rows and 4 columns:

- Scale:

  The name of the scale

- Subscale:

  The name of the subscale (or NA if not a subscale)

- Brief:

  The brief clinician-facing definition (10-20 words)

- Client:

  The client-facing definition with examples (30-40 words)

## Examples

``` r
hitopsr_definitions
#> # A tibble: 93 × 4
#>    Scale                Subscale Brief                                    Client
#>    <chr>                <chr>    <chr>                                    <chr> 
#>  1 Agoraphobia          NA       Fear and avoidance of situations where … Being…
#>  2 Antisocial Behavior  NA       Behavior that goes against social norms… Doing…
#>  3 Appetite Loss        NA       Decreased appetite and food interest th… Reduc…
#>  4 Binge Eating         NA       Eating unusually large amounts of food … Episo…
#>  5 Bodily Distress      NA       Physical symptoms like pain, weakness, … Physi…
#>  6 Body Dissatisfaction NA       Persistent dissatisfaction with one's b… Ongoi…
#>  7 Body Focus           NA       Excessive focus on appearance, with fre… Stron…
#>  8 Callousness          NA       Disregard for others' feelings and need… A gen…
#>  9 Checking             NA       Repetitive urges to check tasks, driven… Havin…
#> 10 Cleaning             NA       Fear of dirt and germs, leading to exce… Fear …
#> # ℹ 83 more rows
```
