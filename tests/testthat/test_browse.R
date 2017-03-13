# test the browse_popler function 
library(testthat)

# Main utility functions ---------------------------------------------------------------------
context("browse(): Format Main Table()")

# does this function actually return character columns? 
test_that("Format Main Table", {
  
  expect_equal(is.character(factor_to_character(main_popler)[,2]),T)
  expect_equal(is.character(factor_to_character(main_popler)[,3]),T)
  expect_equal(is.character(factor_to_character(main_popler)[,4]),T)
  expect_equal(is.character(factor_to_character(main_popler)[,8]),T)
  expect_equal(is.character(factor_to_character(main_popler)[,9]),T)
  
})

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
  x <- factor_to_character(main_popler)
  
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


context("browse(): nest_taxa")

# Select By Criteria function
test_that("nest_taxa", {
  
  # Data
  x <- factor_to_character(main_popler)
  # Case insensitive matching ("lower" everything)
  names(main_t) <- tolower( names(main_t) )
  # convert columsn "ordr" to "order" and "clss" to "class"
  main_t        <- class_order_names(main_t)
  
  # limit yourself to one/two studies
  # 1 full tab = TRUE
  shrinked_tab <- nest_taxa(main_t, full_tbl = T)
  # test 1a: taxonomy variables
  expect_equal(ncol(shrinked_tab$taxonomy[[1]]), 18)
  # test 1b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(main_t$proj_metadata_key)))
  
  # 2 full tab = FALSE
  shrinked_tab <- nest_taxa(main_t, full_tbl = TRUE)
  # test 2a: taxonomy variables
  expect_equal(ncol(shrinked_tab$taxonomy[[1]]), 18)
  # test 2b: number of projects
  expect_equal(nrow(shrinked_tab), length(unique(main_t$proj_metadata_key)))
  
  rm(list=c("x","main_t","shrinked_tab"))
  
})


# error functions ---------------------------------------------------------------------
context("browse(): error functions")

# does this function actually return character columns? 
test_that("Errors ", {
  
  main_t        <- factor_to_character(main_popler)
  names(main_t) <- tolower( names(main_t) )
  main_t        <- class_order_names(main_t)
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
  expect_equal(ncol( browse() ), 16 )
  expect_equal(ncol( browse(full_tbl = T) ), 61 )
  
  # functioning of "vars"
  expect_equal(names( browse(vars="lterid") ), c("proj_metadata_key", "lterid", "taxonomy") )
  expect_equal(names( browse(vars="lng_lter") ), c("proj_metadata_key", "lng_lter", "taxonomy") )
  
  #functioning error messages
  expect_error( browse(structured_type_3 == "stage" & datatype == "individual") )
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



# summary_popler() function --------------------------------------------------
context("summary_popler() function")

test_that("summary_popler() function ", {
  
  
  # n of variables produced (this neeed to work)
  expect_equal(dim( summary_popler() ), c(1,1) )
  expect_equal(ncol( summary_popler(group_vars = "lterid",
                                    count_vars = "species") ), 
               2 )
  expect_equal(ncol( summary_popler(group_vars = "lterid",
                                    count_vars = c("species","order")) ), 
               3 )
  
  # column names produced
  expect_equal(names( summary_popler() ), "title_count" )
  expect_equal(names( summary_popler(count_vars = "lterid") ), "lterid_count" )
  expect_equal(names( summary_popler(count_vars = "class") ), "class_count" )


})

