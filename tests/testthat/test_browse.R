# test the browse_popler function 
library(testthat)

# Main utility functions ---------------------------------------------------------------------
context("browse(): Informational messages")

# Do informational messages provide the expected output?
test_that("Informational messages", {
  
  # Initial group_factors
  possiblevars <- tolower(c("lterid","proj_metadata_key","title","metalink","studytype","community","duration_years",
                            "study_site_key","datatype", "structured",
                            "species","taxonomy","kingdom","phylum","clss","ordr","family","genus"))
  
  # errors table
  expect_error(vars_spell("pi",possiblevars))
  expect_error(vars_spell("proj_metadata_fkey",possiblevars))
  expect_error(vars_spell("rep_level_3",possiblevars))
  expect_error(vars_spell("treatment",possiblevars))
  
})

context("browse(): Select by criteria")

# Select By Criteria function
test_that("Select by criteria", {
  
  # Data
  x <- summary_table
  
  # Test 1 
  subset_data <- select_by_criteria(x,substitute(kingdom == "Plantae"))
  expect_equal( unique(subset_data$kingdom), "Plantae")
  # Test 2
  subset_data <- select_by_criteria(x,substitute(lterid == "SEV"))
  expect_equal( unique(subset_data$lterid), "SEV")
  # Test 3
  subset_data <- select_by_criteria(x,substitute(genus == "Poa"))
  expect_equal( unique(subset_data$genus), "Poa")
  
  rm(list=c("x","subset_data"))
  
})


context("browse(): taxa_nest")

# Select By Criteria function
test_that("taxa_nest", {
  
  # Data
  main_t <- summary_table
  
  # limit yourself to one/two studies
  # 1 full tab = TRUE
  shrinked_tab <- taxa_nest(main_t, full_tbl = TRUE)
  # test 1a: taxas variables
  expect_equal(ncol(shrinked_tab$taxas[[1]]), 18)
  # test 1b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(main_t$proj_metadata_key)))
  
  # 2 full tab = TRUE
  shrinked_tab <- taxa_nest(main_t, full_tbl = TRUE)
  # test 2a: taxas variables
  expect_equal(ncol(shrinked_tab$taxas[[1]]), 18)
  # test 2b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(main_t$proj_metadata_key)))
  
  rm(list=c("x","main_t","shrinked_tab"))
  
})


# error functions ---------------------------------------------------------------------
context("browse(): error functions")

# does this function actually return character columns? 
test_that("Errors ", {
  
  main_t        <- summary_table
  possible_arg  <- possiblevars
  
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
  
 
  # n of columns
  expect_equal(ncol( browse() ), 19 )
  expect_equal(ncol( browse(full_tbl = T) ), 60 )
  
  # functioning of "vars"
  expect_equal(names( browse(vars="lterid") ), c("proj_metadata_key", "lterid", "taxas") )
  expect_equal(names( browse(vars="lng_lter") ), c("proj_metadata_key", "lng_lter", "taxas") )
  
  #functioning error messages
  expect_error( browse(structured_type_3 == "stages" & datatype == "individual") )
  expect_error( browse(lterid == "SEVa") )
  expect_error( browse(lter == "SEV") )
  
})


# dictionary function --------------------------------------------------
context("dictionary() function")

test_that("dictionary() function ", {
  
  
  # n of variables in "informational message"
  expect_equal(nrow( dictionary() ), 18 )
  expect_equal(nrow( dictionary(full_tbl = T) ), 76 )
  
  # n of list elements
  expect_equal( length(dictionary(species)), 1)
  expect_equal( length(dictionary(lterid,lng_lter)), 2)
  
  # class of list elements
  expect_equal( class(dictionary(species)[[1]]), "data.frame")
  expect_equal( class(dictionary(lterid)[[1]]), "character")
  
})

