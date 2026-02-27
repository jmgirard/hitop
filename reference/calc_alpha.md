# Compute Cronbach's Coefficient Alpha

`calc_alpha()` computes Cronbach's coefficient alpha for a set of items
from the **unstandardized covariance** matrix. Items are coerced to
numeric before calculation. Missing data can be handled via **pairwise**
deletion.

## Usage

``` r
calc_alpha(df)
```

## Arguments

- df:

  A data frame or matrix with one row per observation and one column per
  item.

## Value

A numeric scalar giving Cronbach's alpha (typically in \[0, 1\], but can
be outside this range for problematic data).

## Details

Let \\k\\ be the number of items and let \\X_1,\ldots,X_k\\ denote the
item scores. Define the total score \\T = \sum\_{i=1}^k X_i\\.
Cronbach's alpha can be written without matrix notation as \$\$\alpha
\\=\\ \frac{k}{k-1}\left(1 - \frac{\sum\_{i=1}^k
\mathrm{Var}(X_i)}{\mathrm{Var}(T)}\right).\$\$ In words: take the sum
of the item variances and divide by the variance of the total score;
subtract this ratio from 1 and multiply by \\k/(k-1)\\.

Both \\\mathrm{Var}(X_i)\\ and \\\mathrm{Var}(T)\\ are computed using
pairwise-complete observations.

## Coercion

Each column is coerced via
[`as.numeric()`](https://rdrr.io/r/base/numeric.html). This means:

- factors are converted to **integer level codes** (not labels);

- characters that are not parsable as numbers become `NA`;

- dates/times become their underlying numeric representations. Ensure
  your columns are truly numeric item responses or pre-process as
  needed.

## References

- Cronbach, L. J. (1951). Coefficient alpha and the internal structure
  of tests. *Psychometrika, 16*(3), 297–334.

- Sijtsma, K. (2009). On the use, the misuse, and the very limited
  usefulness of Cronbach's alpha. *Psychometrika, 74*(1), 107–120.
