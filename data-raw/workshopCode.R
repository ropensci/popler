# NCO Metacommunity LTER Working Group
# Test run of the poplerr package

#Install packages
install.packages("devtools")
devtools::install_github("klutometis/roxygen")

install.packages(dplyr)
install.packages(RPostgreSQL)


# Using the popler() function to browse database-------------------

# no arguments shows possible group factors
popler()

# group_factors only returns unique values for each entry
popler(group_factor="species") # these are the species present

# tally_by returns tallies for each entry
popler(tally_by="species")

# criteria subsets the
popler(criteria=genus=="Abietinaria")

# Tally by group
popler(group_factors="title",tally_by="species") # n. of species by study

# Tally by group, with a subset of data
popler(group_factors=c("title","metarecordid"),
       tally_by="species",
       criteria=community=="yes")

# Using the tunnel_query() function to query database-------------------

# get data from the database
data1=tunnel_query(metarecordid==21)

#
popler(group_factor="title",tally_by="species",
       criteria=metarecordid==54)
