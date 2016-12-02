## ---- message = FALSE, warning = FALSE-----------------------------------
devtools::install_github("AldoCompagnoni/popler",ref = "0.0.9001", build_vignettes = TRUE)
library(popler)

## ------------------------------------------------------------------------
dictionary()

## ------------------------------------------------------------------------
dictionary("phylum")
dictionary(vars = c("lterid", "duration_years") )

