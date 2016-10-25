# NCO Metacommunity LTER Working Group
# Test run of the popler package

#devtools::install_github("AldoCompagnoni/popler",
#                         ref="workingGroupNCEAS")


# Build R package on your machine==============================================
# Three options:

# 1. Build package directly on your machine------------------------------------

# Install devtools if you don't have it
#install.packages("devtools")
# Install roxygen if you don't have it
#devtools::install_github("klutometis/roxygen")


# Install browse_poplerr dependencies
install.packages("dplyr")
install.packages("RPostgreSQL")

# hit the "build and repload" button in Rstudio

#2. Install package from binary (.zip) file-----------------------------------
#Path of the directory where the .zip file is stored.
dirName="C:/Users/ac79/MEGA/Projects/RICE/LTER/"
install.packages(paste0(dirName,"browse_popler_0.0.0.9000.zip"),
                 repos=NULL,type="binary")
remove.packages("browse_popler")
detach("package:browse_popler")


#3. Install package from source (.tar.gz) file-----------------------------------
install.packages(paste0(dirName,"browse_popler_0.0.0.9000.tar.gz"),
                 repos=NULL,type="source")



# Using the browse_popler() function to browse database=================================

# no arguments returns all the possible argument entries
browse_popler()
#Note, the above is equal to:
browse_popler(group_factors = NULL, tally_by = NULL, criteria = NULL, trim =T )


# Using one argument at a time --------------------------------------------------

# group_factors only returns unique values for each entry
unique_species=browse_popler(group_factor="species") # these are the species present

# tally_by returns tallies for each entry
species_count=browse_popler(tally_by="species")

# criteria subsets the data. Synthax is the same as subset{base}
poa_studies=browse_popler(criteria=genus=="Poa")


# Combining arguments------------------------------------------------------------

# Tally by group
# n. of species by study
species_x_study=browse_popler(group_factors="study",tally_by="species")
species_x_study=browse_popler(group_factors=c("study","metarecordid"),tally_by="species")
# n. of sites per study
sites_x_study=browse_popler(group_factors=c("study"),tally_by="study_site") #n. of sites by study

# Tally species by study, only for community studies-----------------------------
# Are they all 1?
spp_x_study=browse_popler(group_factors=c("study","metarecordid"),
                     tally_by="species",
                     criteria=community=="yes")

spp_x_study=browse_popler(group_factors=c("lterid","study"),tally_by="study_site",
                          criteria=community=="yes" & duration_years > 10 & studytype=="obs")

browseURL(browse_popler(group_factors = "metalink",criteria=metarecordid==21))

# Tally species by study, subsetting by latitude------------------------------------------
spp_x_study_lat=browse_popler(group_factors=c("study","metarecordid"),
                       tally_by="species",
                       criteria=lat_site > 35)

# Tally by study, subsetting by study duration--------------------------------------------
spp_x_study_duration=browse_popler(group_factors=c("study","metarecordid","duration_years"),
                            tally_by="species",
                            criteria=duration_years > 10 & duration_years <20)

# How many community studies involving animals?--------------------------------------------
animal_studies=browse_popler(tally_by="study",
                      criteria=kingdom=="Animalia" & community=="yes" & studytype=="exp")

animal_studies=browse_popler(tally_by="study",criteria=kingdom=="Animalia")


# Querying Database=======================================================================

# get data from study number 21
dat21=tunnel_query(metarecordid==21)

# get data from multiple studies
dat21_22=tunnel_query(metarecordid==21 | metarecordid==22)

# get all data from a particular genus
dat_poa=tunnel_query(genus=="Poa")
