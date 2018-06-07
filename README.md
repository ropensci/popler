
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/AldoCompagnoni/popler.svg?branch=master)](https://travis-ci.org/AldoCompagnoni/popler) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AldoCompagnoni/popler?branch=master&svg=true)](https://ci.appveyor.com/project/AldoCompagnoni/popler) [![Coverage status](https://codecov.io/gh/AldoCompagnoni/popler/branch/master/graph/badge.svg)](https://codecov.io/github/AldoCompagnoni/popler?branch=master)

Popler
------

Popler is an R package for querying the *Popler* data base. It connects to an SQL data base that contains information on long term population dynamics studies from the US LTER network. Currently, it is only available on GitHub, but will hopefully be on CRAN soon.

``` r
# Install stable version once on CRAN (hopefully soon!)
install.packages('popler')


# Install development version now
if(!require(devtools, quietly = TRUE)) {
  install.packages(devtools)
}

devtools::install_github('AldoCompagnoni/popler')
```

All exported functions use the `pplr_` prefix and tidy evaluation, meaning you do not need to manually quote inputs. Once installed, you can explore the variables in the data base using the `pplr_dictionary()` function. This will give you a better idea of what each variable means and assist in refining queries for the next step. Additionally, there is the `report_dictionary()` function which generates an .Rmd file and renders it into html.

``` r
pplr_dictionary()
```

More to come later...
