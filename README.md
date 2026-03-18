
# mntaxa

<!-- badges: start -->
[![R-CMD-check](https://github.com/MN-DNR-MBS/mntaxa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MN-DNR-MBS/mntaxa/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

MNTaxa is the Minnesota Department of Natural Resource's list of vascular plant species that have been documented in Minnesota. It contains a list of accepted names, synonyms, and attributes. The goal of the mntaxa package is to easily load and format MNTaxa tables for analysis. Users connected to the MNTaxa database can access the most updated tables while all other users can access a snapshot of tables.  

### DNR Users  

If you would like to pull "live" MNTaxa tables, please configure the MNTaxa ODBC DSN on your computer before using this package.  

### External Users  

The MNTaxa tables included in this version of mntaxa (1.0.0) were saved on February 26, 2026. Data objects such as taxa_raw contain date attributes that can be used to further investigate the status of the tables if needed.  

### Attributes  

Attributes that can included in MNTaxa tables include rank, taxonomic parents, authorities, publications, physiognomy (including a revised version used for vegetation classification), origins, common names, and C-values.  

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

## Acknowledgements  

The MNTaxa database is managed by Derek Anderson and Jared Cruz, who both contributed to the development of this package. Alaina Berger, Daniel Wovcha, Dustin Graham, and Nathan Dahlberg also helped develop the datasets used in this package. For more information on MNTaxa, please visit [https://www.dnr.state.mn.us/eco/mbs/plant-lists.html](https://www.dnr.state.mn.us/eco/mbs/plant-lists.html).
