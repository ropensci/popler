#Data used to generate the main table
setwd("C:/Users/Aldo/Documents/popler")

# generate main data table--------------------------------------------

conn <- src_postgres(
  dbname="popler", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

table_search <- tbl(conn, sql(
  paste("SELECT * ",
        "FROM taxa_table",
        "JOIN study_table ON taxa_table.taxa_lter_proj_site =",
        "study_table.lter_proj_site",
        "JOIN site_in_study_table ON study_table.study_site = site_in_study_table.study_site_pk")))

dataPoplerFunction=as.data.frame(table_search)

# store main data table--------------------------------------------
devtools::use_data(dataPoplerFunction, internal = T)
