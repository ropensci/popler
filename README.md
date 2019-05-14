
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/AldoCompagnoni/popler.svg?branch=master)](https://travis-ci.org/AldoCompagnoni/popler) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AldoCompagnoni/popler?branch=master&svg=true)](https://ci.appveyor.com/project/AldoCompagnoni/popler) [![Coverage status](https://codecov.io/gh/AldoCompagnoni/popler/branch/master/graph/badge.svg)](https://codecov.io/github/AldoCompagnoni/popler?branch=master)

Popler
======

`popler` is the R package to browse and query the `popler` data base. `popler` is a PostgreSQL data base that contains population-level datasets from the US long term ecological research (LTER) network. This package is currently only available on GitHub, but our ultimate goal is to submit it to CRAN. A more detailed explanation on the structure of the `popler` online database is contained in the [dratf of the manuscript presenting popler](https://github.com/texmiller/popler-ms/blob/master/popler_ms.pdf) and in the [dedicated vignette](https://github.com/AldoCompagnoni/popler/blob/master/vignettes/popler-database-structure.Rmd).

The package `popler` aims to facilitate finding, retrieving, and using time-series data of population abundance associated with the US LTER network. To *find datasets*, the functions in `popler` aid in understanding and browsing the metadata information referring to each dataset. To *retrieve* data, the function `pplr_get_data()` downloads single datasets or groups of datasets. These downloads share the same data structure. To *use* downloaded data, the package provides ancillary functions to consult and cite the original data sources, examine the temporal replication of each data set, and methods for a couple of `dplyr` verbs to assist with data manipulation.

### Installation

------------------------------------------------------------------------

``` r
# Install stable version once on CRAN (hopefully soon!)
install.packages('popler')


# Install development version now
if(!require(devtools, quietly = TRUE)) {
  install.packages(devtools)
}

devtools::install_github('AldoCompagnoni/popler')
```

Finding datasets
================

------------------------------------------------------------------------

##### Dictionary of variables

All exported functions in the `popler` R package use the `pplr_` prefix and lazy and/or tidy evaluation, meaning you do not need to manually quote most inputs. We suggest to start exploring the variables in the data base using `pplr_dictionary()`. This function explores the metadata variables used to describe the datasets contained in `popler`. `pplr_dictionary()` works with and without inputs. Without inputs, the function returns a data frame showing a description of each metadata variable. The input of `pplr_dictionary()` is supposed to be the name of one of the metadata variables. When `pplr_dictionary()` is provided with the name of a metadata variable, it returns the possible unique values of the variable. Additionally, the `pplr_report_dictionary()` function generates an `Rmd` file and renders it into html. This html contains both the meaning of variables, and their unique values.

``` r
library(popler)

pplr_dictionary()
```

##### Browsing `popler`

Once you are familiar with the meaning and content of `popler`'s metadata variables, `pplr_browse()` provides the metadata of the studies contained in `popler`. `pplr_browse()` also works with and without an input. Without input, the function produces a data frame including the metadata variables describing every study currently contained in the `popler` database. Note that this data frame is a `tbl` that inherits from the `browse` class. Inputs to `pplr_browse()` allow users to subset this data frame (e.g. `duration_years > 5`). When subsetting, the unique values provided by `pplr_dictionary()` are particularly useful. For more nuanced subsetting of available datasets, the `keyword` argument allows to subset variables using partial matching. Note that `keyword` will act primarily on information contained in the title of studies.

``` r
all_studies <- pplr_browse()

# do not quote logical expressions
long_studies <- pplr_browse(duration_years > 20) 

# keyword is quoted
parasite_studies <- pplr_browse(keyword = 'parasite') 
```

The default settings of both `pplr_browse()` and `pplr_dictionary()` report a subset of the metadata variables contained in popler. To report all variables, set `full_tbl = TRUE`.

``` r
#  vars are quoted
interesting_studies <- pplr_browse(vars = c('duration_years', 'lterid')) 

# Use full_tbl = TRUE to get a table with all possible variables
all_studies_and_vars <- pplr_browse(full_tbl = TRUE)
```

##### Reporting metadata

You can generate a human-readable report on metadata variables of the projects you subset using `pplr_browse` by providing the function with the argument `report = TRUE` . This argument uses `rmarkdown` to render the metadata into an html file, and opens it into your default browser. Alternatively, you can perform the same action described above by providing the `browse` object produced calling `pplr_browse` to the function `pplr_report_metadata()`.

``` r
# generate metadata report for all studies
pplr_browse(report=TRUE)
# alternatively
all_studies <- pplr_browse()
pplr_report_metadata(all_studies)

# generate metadata report for parasite studies
pplr_browse(keyword = 'parasite', report = TRUE)
parasite_studies <- pplr_browse(keyword = 'parasite') 
# alternatively
pplr_report_metadata(parasite_studies)
```

Retrieving data
===============

------------------------------------------------------------------------

Once you explored the metadata and decided which projects interest you, it's time to actually download the data! `pplr_get_data()` connects to the data base via an API, and downloads the raw data based on the criteria supplied. Alternatively, if you're happy with the projects represented in the `browse` object you created earlier, you can simply pass that object to `pplr_get_data()`. Note that if your `browse` object contains 5 rows, `pplr_get_data()` will download 5 separate datasets. All objects created with `pplr_get_data()` inherit from `get_data` and `data.frame` classes.

``` r
# create a browse object and use it to get data

penguins <- pplr_browse(lterid == 'PAL')

# unpack covariates as well

penguin_raw_data <- pplr_get_data(penguins, cov_unpack = TRUE)

# A very specific query

more_raw_data <- pplr_get_data((proj_metadata_key == 43 | 
                                proj_metadata_key == 25) & 
                                year < 1995 )
```

Using data
==========

------------------------------------------------------------------------

We provide three ancillary functions to facilitate the use of the objects downloaded through `pplr_get_data()`.

First, `pplr_metadata_url()` opens up a webpage containing study details. Before doing scientific analyses, we urge the users to review the peculiarities of each dataset by vetting their online documentation. Importantly, `pplr_metadata_url()` also works on objects produced by `pplr_browse`.

Second, `pplr_cov_unpack()` transforms the data contained in the `covariates` column of each downloaded dataset into separate columns. This can or cannot be useful depending on the objectives of the user. Note: you can also transform covariates into a data frame directly through `pplr_get_data()` by providing the function with argument `cov_unpack = TRUE`.

Third, `pplr_citation()` produces a citation for each downloaded dataset.

##### Spatio-temporal replication

The datasets contained in `popler` present many heterogeneities, especially in terms of their spatio-temporal replication. Most studies present at least a few spatial replicates which were not sampled every year. Note that most datasets in `popler` present at least one additional replication level. These spatial replicates are denoted with numbered variables of the form `spatial_replication_level_X`, where `X` refers to the replication level which can go fro 1 to 5. The names of these replication levels (e.g. plot, subplot, etc.) are contained in variable `spatial_replication_level_x_label`.

Once you download a dataset, you can examine the temporal replication of the largest spatial replicate (the site, or `spatial_replication_level_1`) using function `pplr_site_rep_plot()`. This function produces a plot showing whether or not a given site was sampled in a year.

Additionally, `pplr_site_rep()` produces either a logical vector for subsetting an existing `get_data` object or a summary table of temporal replication for a given spatial resolution. You can control the minimum frequency of sampling and the minimum duration of sampling using the `freq` and `duration` arguments, respectively. Additionally, you can choose the level of spatial replication to filter providing an integer between 1 and 5 to the `rep_level` argument. `return_logical` allows you to control what is returned by the function. `TRUE` returns a logical vector corresponding to rows of the `get_data` that correspond to spatial replicates that meet the criteria of replication specified in the function. `FALSE` returns a summary table describing the number of samples per year at the selected spatial resolution.

``` r
# Example with piping and subsetting w/ the logical vector output

library(dplyr)

SEV_studies <- pplr_get_data( lterid == 'SEV' datatype == 'invidual' )

long_SEV_studies <- SEV_studies %>%
  .[pplr_site_rep(input = .,
                  duration = 12,
                  rep_level = 3), ] %>%
  pplr_site_rep_plot()

# Or, create the summary table

SEV_summary <- SEV_studies %>% 
  pplr_site_rep(duration = 13,
                rep_level = 1,
                return_logical = FALSE)


# Modify the site_rep_plot() by hand using ggplot2 syntax
library(ggplot2)

pplr_site_rep_plot(long_SEV_studies, return_plot = TRUE) +
  ggtitle('Sevilleta LTER Temporal Replication')
```

##### Data manipulation

`popler` supplies methods for a couple of `dplyr` verbs to assist with data manipulation. `filter` and `mutate` methods are available for objects of `browse` and `get_data` classes. Other `dplyr` verbs change the structure of the object too much for those classes to retain their meaning so they are not included in the package, but one can still use them for their own purposes.

``` r
penguins_98 <- filter(penguin_raw_data, year == 1998)

class(penguins_98) # classes are not stripped from objects

penguins_98_true <- mutate(penguins_98, penguins_are = 'Awesome')

class(penguins_98_true)
```

Further information
===================

------------------------------------------------------------------------

Additional information on `popler` is contained in a manuscript, and in the vignettes associated with the R package.

The [manuscript, currently in draft form](https://github.com/texmiller/popler-ms/blob/master/popler_ms.pdf), presents the `popler` database, the R package, and our recommendations on how to use them.

The R package contains three vignettes: one vignette [illustrates the structure of the popler database](https://github.com/AldoCompagnoni/popler/blob/master/vignettes/popler-database-structure.Rmd), and two vignettes provide [an introduction](https://github.com/AldoCompagnoni/popler/blob/master/vignettes/introduction-to-popler.Rmd) and [a more detailed look](https://github.com/AldoCompagnoni/popler/blob/master/vignettes/vetting-popler.Rmd) at the intended workflow of the popler package.

In case these vignettes do not cover your particular use case, you still have questions, or you discover a bug, please don't hesitate to create an [issue](https://github.com/AldoCompagnoni/popler/issues).
