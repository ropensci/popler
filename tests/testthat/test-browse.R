# test the browse_popler function 
library(testthat)

summary_table <- pplr_summary_table_import()

# Main utility functions ---------------------------------------------------------------------
context("browse(): Informational messages")

# Do informational messages provide the expected output?
test_that("Informational messages", {
  
  # Initial group_factors
  possiblevars <- tolower(c("lterid", "proj_metadata_key", "title",
                            "metalink", "studytype", "community",
                            "duration_years", "study_site_key", "datatype",
                            "structured", "species", "taxonomy",
                            "kingdom", "phylum", "clss",
                            "ordr", "family", "genus"))
  
  # errors table
  expect_error(vars_spell("pi",
                          c(names(summary_table), 'taxonomy'),
                          possiblevars))
  expect_error(vars_spell("proj_metadata_fkey",
                          c(names(summary_table), 'taxonomy'), 
                          possiblevars))
  expect_error(vars_spell("rep_level_3",
                          c(names(summary_table), 'taxonomy'),
                          possiblevars))
  expect_error(vars_spell("treatment",
                          c(names(summary_table), 'taxonomy'), 
                          possiblevars))
  
})

context("browse(): Select by criteria")

# Select By Criteria function
test_that("Select by criteria", {
  
  # Data
  x <- summary_table
  
  # Test 1 
  subset_data <- popler:::select_by_criteria(x, substitute(kingdom == "Plantae"))
  expect_equal( unique(subset_data$kingdom), "Plantae")
  # Test 2
  subset_data <- popler:::select_by_criteria(x, substitute(lterid == "SEV"))
  expect_equal(unique(subset_data$lterid), "SEV")
  # Test 3
  subset_data <- popler:::select_by_criteria(x,substitute(genus == "Poa"))
  expect_equal(unique(subset_data$genus), "Poa")
  
  rm(list = c("x","subset_data"))
  
})


context("browse(): taxa_nest")

# Select By Criteria function
test_that("taxa_nest", {
  possible_vars <- popler:::default_vars()
  
  # limit yourself to one/two studies
  # 1 full tab = TRUE, vars = NULL
  shrinked_tab <- popler:::taxa_nest(summary_table, full_tbl = TRUE)
  # test 1a: taxas variables
  expect_equal(ncol(shrinked_tab$taxas[[1]]), 18)
  # test 1b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(summary_table$proj_metadata_key)))
  
  # 2 full tab = TRUE, vars = NULL
  shrinked_tab <- popler:::taxa_nest(summary_table[ , possible_vars], full_tbl = FALSE)
  # test 2a: taxas variables
  expect_equal(ncol(shrinked_tab$taxas[[1]]), 8)
  # test 2b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(summary_table$proj_metadata_key)))
  
  # 4. vars = "proj_metadata_key" (not a taxonomic variable)
  shrinked_tab <- popler:::taxa_nest(summary_table[ , popler:::vars_check("proj_metadata_key"),
                                           drop = FALSE],
                            full_tbl = FALSE)
  # no taxa list
  expect_true(is.null(shrinked_tab$taxas))
  
  # preserves data.frame structure instead of vector
  expect_true(inherits(shrinked_tab, 'data.frame'))
  expect_equal(ncol(shrinked_tab), 1)
  
  # 5. vars = c("proj_metadata_key","lterid") (no taxonomic variables)
  shrinked_tab <- popler:::taxa_nest(summary_table[ , popler:::vars_check(c("proj_metadata_key", 
                                                          'lterid')),
                                           drop = FALSE],
                            full_tbl = FALSE)  # no taxa list
  expect_true(is.null(shrinked_tab$taxas))
  expect_equal(ncol(shrinked_tab), 2)
  
  # 5. vars = "genus" (only one taxonomic variable)
  shrinked_tab <- popler:::taxa_nest(summary_table[ ,popler:::vars_check("genus")],
                            full_tbl = FALSE)
  # two columns: "proj_metadata_key", and "genus"
  expect_equal(ncol(shrinked_tab), 2)
  # name of list column is "genus"
  expect_equal(names(shrinked_tab)[2], "genus")
  
  # 6. vars = c("genus","species") (two taxonomic variables)
  shrinked_tab <- popler:::taxa_nest(summary_table[ , popler:::vars_check(c("genus","species"))],
                            full_tbl = FALSE)
  # two columns: "proj_metadata_key", and "taxas"
  expect_equal(ncol(shrinked_tab), 2)
  # name of list column is now "taxas"
  expect_equal(names(shrinked_tab)[2], "taxas")
  
  
  rm("shrinked_tab")
  
})


# error functions ---------------------------------------------------------------------
context("pplr_browse(): error functions")

# does this function actually return character columns? 
test_that("Errors ", {
  
  # Initial group_factors
  possible_arg  <- tolower(c("lterid", "proj_metadata_key", "title",
                            "metalink", "studytype", "community",
                            "duration_years", "study_site_key", "datatype",
                            "structured", "species", "taxonomy",
                            "kingdom", "phylum", "clss",
                            "ordr", "family", "genus"))
  
  main_t        <- summary_table
  
  
  # vars_spell errors
  expect_error(vars_spell( "lterids", names(main_t), possible_arg ))
  expect_error(vars_spell( "gensus", names(main_t), possible_arg ))
  expect_error(vars_spell( "tilte", names(main_t), possible_arg ))
  expect_error(vars_spell( "specis", names(main_t), possible_arg ))
  
  # select criteria
  expect_error( select_by_criteria(main_t, substitute( lterid == "SEVa" )) )
  expect_error( select_by_criteria(main_t, substitute( lterids == "SEV" )) ) 
  expect_error( select_by_criteria(main_t, substitute( lterids == "SEVa" )) ) 
  
})


# browse function itself --------------------------------------------------
context("browse() function")

test_that("browse() function ", {
  
 
  # No search criteria
  expect_equal(ncol(pplr_browse() ), 19)
  expect_equal(ncol(pplr_browse(full_tbl = TRUE)), 60 )
  
  # functioning of "vars"
  expect_equal(names(pplr_browse(vars="lterid")), 
               c("proj_metadata_key", "lterid"))
  
  expect_equal(names(pplr_browse(vars = "lng_lter")), 
               c("proj_metadata_key", "lng_lter"))
  
  # functioning of ...
  SBC <- pplr_browse(lterid == 'SBC')
  
  expect_equal(ncol(SBC), 19)
  expect_true(all(popler:::default_vars()[1:18] %in% names(SBC)))
  
  a_few <- pplr_browse(lterid == 'SEV' | lterid == 'MVC',
                       full_tbl = TRUE)
  expect_equal(ncol(a_few), 60)
  
  
  # functioning of keywords
  parasites <- pplr_browse(keyword = 'parasite')
  
  expect_true(inherits(parasites,
                       'tbl_df'))

  expect_equal(dim(parasites)[2], 19) # 18 columns of data + 1 for taxonomic
  
  
  rodents <- pplr_browse(keyword = 'rodent', full_tbl = TRUE)
  expect_equal(ncol(rodents), 60) # all info
  
  #functioning error messages
  expect_error(pplr_browse(structured_type_3 == "stages" & 
                             datatype == "individual"))
  expect_error(pplr_browse(lterid == "SEVa"))
  
  expect_error(pplr_browse(lter == "SEV"))
  
})

