
<!-- README.md is generated from README.Rmd. Please edit that file -->

# miceafter

The `miceafter` package provides functions to apply statistical and
pooled analyses after multiple imputation. Therefore the name
‘miceafter’. The main functions of the package are the `df2milist`,
`list2milist`, `mids2milist` and the `with.milist` functions. The
`df2milist` function turns a data frame with multiply imputed datasets
into an object of class `milist`, the `list2milist` does this for a list
with multiply imputed datasets and the `mids2milist` for objects of
class `mids`. These `milist` object can than be used with the
`with.milist` function to apply repeated statistical analyses across the
multiply imputed datasets. Subsequently, pooling functions are available
in the form of separate `pool` functions, like the `pool_levenetest`
function to pool Levene’s tests across multiply imputed datasets or the
`pool_propdiff_nw function` to pool the difference between proportions
according to method Newcombe-Wilson. The package also contains a
function `pool_glm` to pool and select linear and logistic regression
functions.

More and more statistical analyses and pooling functions will be added
over time.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mwheymans/miceafter")
```

## Citation

Cite the package as:

``` r
Martijn W Heymans (2021). miceafter: Data Analysis and Pooling after Multiple Imputation. 
R package version 0.1.0. https://mwheymans.github.io/miceafter/
```

## Examples

## Pooling Levene’s Test

This example shows you how to pool the Levene test across 5 multiply
imputed datasets. The pooling method that is used is method D1.

``` r
library(miceafter)

# Step 1: Turn data frame with multiply imputed datasets into object of 'milist'
imp_dat <- df2milist(lbpmilr, impvar="Impnr")

# Step 2: Do repeated analyses across multiply imputed datasets
ra <- with(imp_dat, expr=levene_test(Pain ~ factor(Carrying)))

# Step 3: Pool repeated test results
res <- pool_levenetest(ra, method="D1")
res
#>       F_value df1      df2    P(>F)       RIV
#> [1,] 1.586703   2 115.3418 0.209032 0.1809493
#> attr(,"class")
#> [1] "mipool"
```

## Pooling Difference between Proportions according to Newcombe-Wilson

``` r
library(miceafter)

# Step 1: Turn data frame with multiply imputed datasets into object of 'milist'
imp_dat <- df2milist(lbpmilr, impvar="Impnr")

# Step 2: Do repeated analyses across multiply imputed datasets
ra <- with(imp_dat, 
           expr=propdiff_wald(Chronic ~ Radiation, strata = TRUE))

# Step 3: Pool repeated test results
res <- pool_propdiff_nw(ra)
res
#>      Prop diff CI L NW CI U NW
#> [1,]    0.2786  0.1199   0.419
#> attr(,"class")
#> [1] "mipool"
```

More examples can be found on the [package
website](https://mwheymans.github.io/miceafter/)
