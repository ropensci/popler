setwd("C:/Users/ac79/MEGA/Projects/LTER/popler_dev/taxa_clean")

# read files, format then 
file_list  <- list.files(pattern = ".csv")
taxa_files <- lapply(file_list,read.csv)
taxa_files <- setNames(taxa_files, unlist(lapply(".csv",gsub,"",file_list)) )

# write files
setwd("C:/Users/ac79/Documents/CODE/popler")

attach(taxa_files)
devtools::use_data(clss_clean, internal = T, overwrite = T)
devtools::use_data(fam_clean, internal = T, overwrite = T)
devtools::use_data(king_clean, internal = T, overwrite = T)
devtools::use_data(ordr_clean, internal = T, overwrite = T)
devtools::use_data(phyl_clean, internal = T, overwrite = T)
detach(taxa_files)
