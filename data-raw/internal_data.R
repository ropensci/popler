
# explanations ---------------------------------------------------------

#' @noRd
# data frame of variable explanations
explanations = data.frame(variable = c('proj_metadata_key',
                                       'lter_project_fkey',
                                       'title','samplingunits',
                                       'datatype',
                                       'structured_data',
                                       'structured_type_1',
                                       'structured_type_1_units',
                                       'structured_type_2',
                                       'structured_type_2_units',
                                       'structured_type_3',
                                       'structured_type_3_units',
                                       'studystartyr','studyendyr',
                                       'samplefreq','studytype','community',
                                       'spatial_replication_level_1_extent',
                                       'spatial_replication_level_1_extent_units',
                                       'spatial_replication_level_1_label',
                                       'spatial_replication_level_1_number_of_unique_reps',
                                       'spatial_replication_level_2_extent','spatial_replication_level_2_extent_units',
                                       'spatial_replication_level_2_label',
                                       'spatial_replication_level_2_number_of_unique_reps',
                                       'spatial_replication_level_3_extent',
                                       'spatial_replication_level_3_extent_units',
                                       'spatial_replication_level_3_label',
                                       'spatial_replication_level_3_number_of_unique_reps',
                                       'spatial_replication_level_4_extent',
                                       'spatial_replication_level_4_extent_units',
                                       'spatial_replication_level_4_label',
                                       'spatial_replication_level_4_number_of_unique_reps',
                                       'spatial_replication_level_5_extent',
                                       'spatial_replication_level_5_extent_units',
                                       'spatial_replication_level_5_label',
                                       'spatial_replication_level_5_number_of_unique_reps',
                                       'tot_spat_rep','n_spat_levs',
                                       'treatment_type_1','treatment_type_2',
                                       'treatment_type_3','control_group',
                                       'derived','authors','authors_contact',
                                       'metalink','knbid','duration_years',
                                       'lterid','lter_name','lat_lter','lng_lter',
                                       'currently_funded',
                                       'current_principle_investigator',
                                       'current_contact_email','alt_contact_email',
                                       'homepage',
                                       'sppcode','kingdom','subkingdom',
                                       'infrakingdom','superdivision',
                                       'division','subdivision','superphylum',
                                       'phylum','subphylum','class','subclass',
                                       'order','family','genus','species',
                                       'common_name','authority','metadata_taxa_key'), 
                          
                          description=c("project_id","foreign key of lter project",
                                        "title of project",
                                        "unit of measure for abundance",
                                        "type of abundance data (e.g. count,biomass)",
                                        "are abundance observations grouped (e.g. based on age)?",
                                        "1st type of indidivual structure (if any)",
                                        "unit of measure of 1st type of individual structure",
                                        "2nd type of indidivual structure (if any)",
                                        "unit of measure of 2nd type of individual structure",
                                        "3rd type of indidivual structure (if any)",
                                        "unit of measure of 3rd type of individual structure",
                                        "year of first observation",
                                        "year of last observation",
                                        "frequency of sampling (e.g. yearly, seasonal, monthly)",
                                        "experimental or observational study?",
                                        "does data set contain multiple taxa?",
                                        
                                        "extent (in area or volume) of replication level 1 (highest rep. level: site)",
                                        "unit of measure by which extent of the replication level 1 was measured",
                                        "label by which rep. level 1 is identified in the original data set",
                                        "number of replicates (level 1)",
                                        "extent (in area or volume) of replication level 2 (highest rep. level: site)",
                                        "unit of measure by which extent of the replication level 2 was measured",
                                        "label by which rep. level 2 is identified in the original data set",
                                        "number of replicates (level 2)",
                                        "extent (in area or volume) of replication level 3 (highest rep. level: site)",
                                        "unit of measure by which extent of the replication level 3 was measured",
                                        "label by which rep. level 3 is identified in the original data set",
                                        "number of replicates (level 3)",
                                        "extent (in area or volume) of replication level 4 (highest rep. level: site)",
                                        "unit of measure by which extent of the replication level 4 was measured",
                                        "label by which rep. level 4 is identified in the original data set",
                                        "number of replicates (level 4)",
                                        "extent (in area or volume) of replication level 5 (highest rep. level: site)",
                                        "unit of measure by which extent of the replication level 5 was measured",
                                        "label by which rep. level 5 is identified in the original data set",
                                        "number of replicates (level 5)",
                                        "total number of spatial replicates",
                                        "number of nested spatial levels",
                                        
                                        "type of 1st treatment (if any)",
                                        "type of 2nd treatment (if any)",
                                        "type of 3rd treatment (if any)",
                                        "label of control treatment/group",
                                        "is abundance a derived quantity?", # make 100% sure this is correct
                                        "authors of the study",
                                        "email contact of study's authors",
                                        "web address of study's metadata",
                                        "identifier of the knb Catalog System", 
                                        
                                        "duration of project in years",
                                        "LTER site identification code",
                                        "full name of LTER site",
                                        "LTER site latitude", "LTER site longitude",
                                        "is LTER site currently_funded? (1 is funded,
                                        0 is not funded)",
                                        "current principlal investigator of LTER",
                                        "email of LTER's current principlal investigator",
                                        "alternative email of LTER's current principlal investigator",
                                        "LTER homepage web address",
                                        
                                        "species code (generally refers to a binomial name (genus, species)",
                                        "kingdom","subkingdom","infrakingdom",
                                        "superdivision","division", 
                                        "subdivision","superphylum",
                                        "phylum","subphylum","class",
                                        "subclass","order","family","genus",
                                        "specific epithet", "common name of species", 
                                        "scholar who first published the species name",
                                        "foreign key of taxa table"),
                          stringsAsFactors = F)

explain_short = data.frame( variable = c('title','proj_metadata_key',
                                         'lterid','datatype',
                                         'structured_data',
                                         'studytype','duration_years',
                                         'community',
                                         'structure','treatment',
                                         'lat_lter','lng_lter',
                                         'species','kingdom','phylum',
                                         'class','order','family','genus'),
                            
                            description=c("title of project","unique project id",
                                          "lter name",
                                          "type of abundance data (e.g. count,biomass)",
                                          "are abundance observations grouped (e.g. based on age)?",
                                          "experimental or observational study?",
                                          "duration of project in years",
                                          "does data set contain multiple taxa?",
                                          "types of indidivual structure",
                                          "types of treatment",
                                          "lter site latitude", "lter site longitude",
                                          "specific epithet of a taxonomic unit",
                                          "kingdom","phylum","class","order",
                                          "family","genus"),
                            stringsAsFactors = F)


# data for tests -------------------------------------------------------

 
# format data
download_popler_test_data <- function( proj_id ){

  # all service functions

  # connect to the database
  popler_connector <- function(dbname=NULL, host=NULL, port=NULL, user=NULL, password=NULL, silent=TRUE) {

    if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
      stop("RPostgreSQL package required to connect to postgres db", call. = FALSE)
    }
    user <- if(is.null(user)){
      if(identical(Sys.getenv("TRAVIS"), "true")){"postgres"} else {""}
      } else user
    con <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                     host     = if(is.null(host))     "" else host,
                     dbname   = if(is.null(dbname))   "" else dbname,
                     user     = user,
                     password = if(is.null(password)) "" else password,
                     port     = if(is.null(port))     "" else port)
  }

  # database open function
  db_open <- function(){
    popler_connector(dbname="" ,
                      host="" ,
                      port= ,
                      user="" ,
                      password="" ,
                      silent=TRUE)
  }

  # get query
  query_get <- function(connection, query){
    # accepts a connection and a string query input
    # outputs a dataframe
    return(dplyr::tbl(connection, dbplyr::sql(query)) %>% data.frame())
  }

  # open connection
  conn      <- db_open()

  # produce efficient query (uses WITH clause)
  efficienty_query <- function( proj_id ){

    paste0('
    WITH project_taxa_table AS (
         select authors, authors_contact,   -- 126
                datatype,
                spatial_replication_level_1_label, spatial_replication_level_2_label, spatial_replication_level_3_label, spatial_replication_level_4_label, spatial_replication_level_5_label, proj_metadata_key, structured_type_1, structured_type_2, structured_type_3, structured_type_4,
                sppcode, genus, species, taxa_table.taxa_table_key
         FROM   taxa_table
           JOIN site_in_project_table ON taxa_table.site_in_project_taxa_key =site_in_project_table.site_in_project_key
           JOIN project_table         ON site_in_project_table.project_table_fkey = project_table.proj_metadata_key
           JOIN study_site_table      ON site_in_project_table.study_site_table_fkey =study_site_table.study_site_key
           JOIN lter_table            ON study_site_table.lter_table_fkey = lter_table.lterid
    WHERE proj_metadata_key = ',proj_id,' )

    SELECT authors, authors_contact, year, day, month, sppcode, genus, species, datatype, spatial_replication_level_1_label, spatial_replication_level_1, spatial_replication_level_2_label, spatial_replication_level_2, spatial_replication_level_3_label, spatial_replication_level_3, spatial_replication_level_4_label, spatial_replication_level_4, spatial_replication_level_5_label, spatial_replication_level_5, proj_metadata_key, structure_type_1, structure_type_2, structure_type_3, structure_type_4, count_table.treatment_type_1, count_table.treatment_type_2, count_table.treatment_type_3, covariates , count_observation
    FROM project_taxa_table
      JOIN count_table ON count_table.taxa_count_fkey = project_taxa_table.taxa_table_key
    UNION ALL

    SELECT authors, authors_contact, year, day, month, sppcode, genus, species, datatype, spatial_replication_level_1_label, spatial_replication_level_1, spatial_replication_level_2_label, spatial_replication_level_2, spatial_replication_level_3_label, spatial_replication_level_3, spatial_replication_level_4_label, spatial_replication_level_4, spatial_replication_level_5_label, spatial_replication_level_5, proj_metadata_key, structure_type_1, structure_type_2, structure_type_3, structure_type_4, biomass_table.treatment_type_1, biomass_table.treatment_type_2, biomass_table.treatment_type_3, covariates , biomass_observation
    FROM project_taxa_table
      JOIN biomass_table ON biomass_table.taxa_biomass_fkey = project_taxa_table.taxa_table_key
    UNION ALL

    SELECT authors, authors_contact, year, day, month, sppcode, genus, species, datatype, spatial_replication_level_1_label, spatial_replication_level_1, spatial_replication_level_2_label, spatial_replication_level_2, spatial_replication_level_3_label, spatial_replication_level_3, spatial_replication_level_4_label, spatial_replication_level_4, spatial_replication_level_5_label, spatial_replication_level_5, proj_metadata_key, structure_type_1, structure_type_2, structure_type_3, structure_type_4, percent_cover_table.treatment_type_1, percent_cover_table.treatment_type_2, percent_cover_table.treatment_type_3, covariates , percent_cover_observation
    FROM project_taxa_table
      JOIN percent_cover_table ON percent_cover_table.taxa_percent_cover_fkey = project_taxa_table.taxa_table_key
    UNION ALL

    SELECT authors, authors_contact, year, day, month, sppcode, genus, species, datatype, spatial_replication_level_1_label, spatial_replication_level_1, spatial_replication_level_2_label, spatial_replication_level_2, spatial_replication_level_3_label, spatial_replication_level_3, spatial_replication_level_4_label, spatial_replication_level_4, spatial_replication_level_5_label, spatial_replication_level_5, proj_metadata_key, structure_type_1, structure_type_2, structure_type_3, structure_type_4, individual_table.treatment_type_1, individual_table.treatment_type_2, individual_table.treatment_type_3, covariates , individual_observation
    FROM project_taxa_table
      JOIN individual_table ON individual_table.taxa_individual_fkey = project_taxa_table.taxa_table_key
    UNION ALL

    SELECT authors, authors_contact, year, day, month, sppcode, genus, species, datatype, spatial_replication_level_1_label, spatial_replication_level_1, spatial_replication_level_2_label, spatial_replication_level_2, spatial_replication_level_3_label, spatial_replication_level_3, spatial_replication_level_4_label, spatial_replication_level_4, spatial_replication_level_5_label, spatial_replication_level_5, proj_metadata_key, structure_type_1, structure_type_2, structure_type_3, structure_type_4, density_table.treatment_type_1, density_table.treatment_type_2, density_table.treatment_type_3, covariates , density_observation
    FROM project_taxa_table
      JOIN density_table ON density_table.taxa_density_fkey = project_taxa_table.taxa_table_key')

  }

  # service function
  colname_change <- function(from, to, x){
    names(x) <- gsub(from, to, names(x))
    return(x)
  }

  # download raw data
  output_data <- query_get(conn, efficienty_query( proj_id ))

  return(output_data)

  # # set to numeric DATE information
  # output_data <- output_data %>%
  #                   mutate( year  = as.numeric(year),
  #                           month = as.numeric(month),
  #                           day   = as.numeric(day) )
  #
  # # set to numeric the observation variable
  # obs_id      <- grep('observation', names(output_data) )
  # output_data[,obs_id] <- output_data[,obs_id] %>% as.numeric
  #
  # # replace -99999, but only for numeric variables
  # replace_99              <- function(x) replace(x, x == -99999, NA)
  #
  # # substitute
  # num_repl                <- sapply(output_data,
  #                                   is.numeric) %>% as.vector()
  # output_data[,num_repl]  <- plyr::colwise(replace_99)(as.data.frame(output_data[,num_repl]))
  #
  # # remove variables that whose content is just "NA"
  # output_data <- base::Filter(function(x) !all(x == "NA"), output_data)
  #
  # # Change "ordr" and "clss" to "order" and "class"
  # output_data <- colname_change("clss", "class", output_data)
  # output_data <- colname_change("ordr", "order", output_data)
  # output_data <- colname_change("count_observation", "abundance_observation",
  #                               output_data)
  #
  # # assign class
  # data_obj <- structure(output_data,
  #               unique_projects = unique(output_data$proj_metadata_key),
  #               unique_authors  = unique(output_data[ ,c("proj_metadata_key",
  #                                                        "authors",
  #                                                        "authors_contact")]),
  #               class = c("get_data", class(output_data))
  #               )

}

# download datasets used for tests
data_1   <- download_popler_test_data( 1 )
data_221 <- download_popler_test_data( 221 )
data_6   <- download_popler_test_data( 6 )


# store internal data --------------------------------------------------

int.data <- list(explanations  = explanations,
                 explain_short = explain_short,
                 data_1        = data_1,
                 data_221      = data_221,
                 data_6        = data_6 )

devtools::use_data(int.data, internal = T, overwrite = T)
