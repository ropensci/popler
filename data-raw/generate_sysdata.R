#Data used to generate the main table
setwd("C:/Users/ac79/Documents/CODE/popler")

#library(RPostgreSQL)

# generate main data table--------------------------------------------

conn <- src_postgres(
  dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

table_search <- tbl(conn, sql(
  paste("SELECT *",
        "FROM project_table",
        "JOIN site_in_project_table ON project_table.proj_metadata_key =",
        "site_in_project_table.project_table_fkey",
        "JOIN taxa_table ON site_in_project_table.site_in_project_key =",
        "taxa_table.site_in_project_taxa_key",
        "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
        "study_site_table.study_site_key",
        "JOIN lter_table ON study_site_table.lter_table_fkey =",
        "lter_table.lterid")))

dataPoplerFunction=as.data.frame(table_search)

# store main data table--------------------------------------------
devtools::use_data(dataPoplerFunction, internal = T, overwrite = T)
