
# mntaxa

<!-- badges: start -->
[![R-CMD-check](https://github.com/MN-DNR-MBS/mntaxa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MN-DNR-MBS/mntaxa/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of mntaxa is to easily load and format MNTaxa tables for analysis. Users connected to the MNTaxa database can access the most updated tables while all other users can access a snapshot of tables.

## Installation

You can install the development version of mntaxa from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("MN-DNR-MBS/mntaxa")
```

## Examples

This example shows how to retrieve a look-up table of all accepted names for taxa in MNTaxa.

``` r
library(mntaxa)
lookup <- lookup_mntaxa()
```

This example shows how to retrieve a look-up table formatted for analyzing releves with analysis groups.

```r
library(mntaxa)
lookup <- lookup_mntaxa(taxonomy_levels = F,
                        sources = F,
                        releve = T,
                        phys = F,
                        strata = T,
                        origin = F,
                        common = F,
                        cvals = F,
                        exclude = F,
                        replace_sub_var = T,
                        replace_family = T,
                        replace_genus = T,
                        drop_higher = T,
                        higher_include = c("Belonia",
                                           "Chara",
                                           "Lychnothamnus",
                                           "Nitella",
                                           "Nitellopsis",
                                           "Spirogyra",
                                           "Tolypella"),
                        excluded_duplicates = T,
                        clean_duplicates = F,
                        group_accepted = T,
                        group_analysis = T)
```
