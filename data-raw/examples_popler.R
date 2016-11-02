
# Ways to install popler
install.packages("C:/Users/ac79/Documents/CODE/popler_0.0.0.9000.zip",repos = NULL )
install.packages("C:/Users/ac79/Documents/CODE/popler_0.0.0.9000.tar.gz",repos = NULL )
devtools::install_github("AldoCompagnoni/popler",ref = "v3")

# browse_popler function examples ----------------------------------------------
browse_popler(group_factors="species")

browse_popler(group_factors=c("title", "species"))

browse_popler(group_factors="species",tally_by="title")
 
browse_popler(group_factors="genus",tally_by="species")

browse_popler(group_factors=c("proj_metadata_key","title"),
              criteria=genus=="Poa")

browse_popler(group_factors=c("spatial_replication_level_1_number_of_unique_reps"),
              tally_by = "title")

# Tunnel query examples ----------------------------------------------

# Select grasshopper data set 
grasshop <- tunnel_query(proj_metadata_key == 21)

# Play with grasshopper columns
grasshop <- tunnel_query(proj_metadata_key == 21,
                         add_columns = c("lat_study_site","lng_study_site","clss"),
                         subtract_columns = c("day"),
                         metadata = F)

# Select data by species
artemisia <- tunnel_query(genus == "Artemisia",metadata = T)
poa       <- tunnel_query(genus == "Poa")
carex     <- tunnel_query(genus == "Carex")

# download all insect studies
insect_studies <- tunnel_query(clss == "Insecta")
