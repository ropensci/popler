## ---- message = FALSE, warning = FALSE-----------------------------------
#devtools::install_github("AldoCompagnoni/popler",ref = "0.0.9001", #build_vignettes = TRUE)
library(popler)

## ------------------------------------------------------------------------
dictionary()

## ------------------------------------------------------------------------
dictionary("phylum")
dictionary(vars = c("lterid", "duration_years") )

## ------------------------------------------------------------------------
browse()

## ------------------------------------------------------------------------
browse(lterid == "SEV")
poa_metadata   <- browse(genus == "Poa")
poa_metadata

## ------------------------------------------------------------------------
as.tbl(get_data(poa_metadata))

## ------------------------------------------------------------------------
as.tbl( get_data(poa_metadata, year > 2008) )

## ------------------------------------------------------------------------
as.tbl( get_data(subset = proj_metadata_key == 25) )

