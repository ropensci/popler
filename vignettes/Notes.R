## ---- warning = FALSE, message = FALSE-----------------------------------
library(popler)
dictionary(full_tbl = T)
browse(full_tbl = T)

## ---- warning = FALSE, message = FALSE-----------------------------------
left_out <- setdiff(dictionary(full_tbl = T)$variable, dictionary()$variable)
keep     <- which( dictionary(full_tbl = T)$variable %in% left_out )
dictionary(full_tbl = T)[keep,]

