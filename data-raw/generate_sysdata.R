#Data used to generate the main table
setwd("C:/Users/ac79/MEGA/Projects/RICE/LTER/poplerr/data-raw")

# generate main data table--------------------------------------------

conn <- src_postgres(
  dbname="popler", host="www.how-imodel-it.com", port=5432, user="lter", password="bigdata")

table_search <- tbl(conn, sql(
  paste("SELECT * ",
        "FROM taxa_table",
        "JOIN main_table ON taxa_table.taxa_lter_proj_site =",
        "main_table.lter_proj_site",
        "JOIN site_table ON main_table.main_siteid = site_table.siteid")))

out=as.data.frame(table_search)

write.csv(out,"mainTable.csv",row.names=F)

# store main data table--------------------------------------------
