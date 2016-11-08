#Data used to generate the main table
setwd("C:/Users/ac79/Documents/CODE/popler")

library(RPostgreSQL)
library(dplyr)

# generate main data table--------------------------------------------

conn <- src_postgres(
  dbname="popler_3", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

#list columns
proj_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                              table_name = 'project_table'")))[,1]
lter_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                              table_name = 'lter_table'")))[,1]
taxa_cols     <- as.data.frame(tbl(conn, sql( "SELECT column_name FROM information_schema.columns WHERE
                                              table_name = 'taxa_table'")))[,1]
seach_cols    <- paste( c(proj_cols,lter_cols,taxa_cols), collapse = ", ")


table_search <- tbl(conn, sql(
  paste("SELECT", seach_cols, 
        "FROM project_table",
        "JOIN site_in_project_table ON project_table.proj_metadata_key =",
        "site_in_project_table.project_table_fkey",
        "JOIN taxa_table ON site_in_project_table.site_in_project_key =",
        "taxa_table.site_in_project_taxa_key",
        "JOIN study_site_table ON site_in_project_table.study_site_table_fkey =",
        "study_site_table.study_site_key",
        "JOIN lter_table ON study_site_table.lter_table_fkey =",
        "lter_table.lterid")))

out                 <- as.data.frame(table_search, n=-1)
out$duration_years  <- out$studyendyr - out$studystartyr
dataPoplerFunction  <- out

# store main data table--------------------------------------------
devtools::use_data(dataPoplerFunction, internal = T, overwrite = T)
