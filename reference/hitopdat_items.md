# HiTOP-DAT Item Data

The items of the HiTOP-DAT (HiTOP Digital Assessment and Tracker), a
battery of seven measures: the WHODAS (12 items), the IDAS-II (99), the
AUDIT (10), the DUDIT (11), the positive items of the CAPE (20), the
CAT-PD static form (216) and the PHQ-15 (14). The battery numbers run
from 1 to 382 in the order the battery gives the measures, which is the
order listed here.

## Usage

``` r
hitopdat_items
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with 382
rows and 5 columns:

- Item:

  The item's number in the battery, 1 to 382 (integer)

- Measure:

  The measure the item belongs to

- MeasureItem:

  The item's number in its own measure (integer)

- Text:

  Item text

- Choice_Set:

  Name of the item's answer set (see
  [hitopdat_choices](https://jmgirard.github.io/hitop/reference/hitopdat_choices.md))

## Details

The item text is taken from the battery's Qualtrics file, with its
markup removed and line breaks within an item made spaces. CAT-PD item
194, cut short in the file, is completed from the IPIP key. The file
moves the PHQ-15's item 4 (menstrual problems) out of the battery, so
the PHQ-15 has 14 items here and its own numbers skip 4. The battery
gives only the CAPE's positive items, and they keep their CAPE numbers
(2 to 42, with gaps). The battery has no scoring function in this
package yet.

## See also

[hitopdat_choices](https://jmgirard.github.io/hitop/reference/hitopdat_choices.md),
[hitopdat_scales](https://jmgirard.github.io/hitop/reference/hitopdat_scales.md)

## Examples

``` r
hitopdat_items
#> # A tibble: 382 × 5
#>     Item Measure MeasureItem Text                                     Choice_Set
#>    <int> <chr>         <int> <chr>                                    <chr>     
#>  1     1 WHODAS            1 Standing for long periods such as 30 mi… whodas    
#>  2     2 WHODAS            2 Taking care of your household responsib… whodas    
#>  3     3 WHODAS            3 Learning a new task, for example, learn… whodas    
#>  4     4 WHODAS            4 How much of a problem did you have join… whodas    
#>  5     5 WHODAS            5 How much have you been emotionally affe… whodas    
#>  6     6 WHODAS            6 Concentrating on doing something for te… whodas    
#>  7     7 WHODAS            7 Walking a long distance, such as half a… whodas    
#>  8     8 WHODAS            8 Washing your whole body.                 whodas    
#>  9     9 WHODAS            9 Getting dressed.                         whodas    
#> 10    10 WHODAS           10 Dealing with people you do not know.     whodas    
#> # ℹ 372 more rows
```
