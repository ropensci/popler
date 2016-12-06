## ---- warning = FALSE, message = FALSE-----------------------------------
library(popler)
browse(duration_years > 10)

## ---- warning = FALSE, message = FALSE-----------------------------------
dictionary("samplefreq")
browse(samplefreq == "monthly")

## ---- warning = FALSE, message = FALSE-----------------------------------
dictionary("class")
browse(class == "Insecta")

## ---- warning = FALSE, message = FALSE-----------------------------------
dictionary("community")
browse(community == "no") # 8 single-species studies

dictionary("treatment_type_1")
nrow( browse(treatment_type_1 == "grazing") ) # 2 grazing studies

dictionary("studytype")
nrow( browse(studytype == "obs") ) # 28 observational studies

## ---- warning = FALSE, message = FALSE-----------------------------------
dictionary( c("lat_lter","lng_lter") )
browse(lat_lter > 40 & lng_lter < -100) # single-species studies

## ---- warning = FALSE, message = FALSE-----------------------------------
browse(kingdom == "Plantae" & n_spat_levs == 4 & duration_years > 10)

