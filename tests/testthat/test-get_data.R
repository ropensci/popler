# test the get_data function, and associated utility functions
library(testthat)
context("get_data() utility functions")


# Does query_cols() return the expected number of columns?
test_that("vars_query", {
  
  conn <- db_open()

  # possible columns 
  potential_vars  <- vars_query(conn)
  # tests
  expect_equal( length(potential_vars$all_vars), 111)
  expect_equal( length(potential_vars$default_vars), 23)

})


# all conditinos in which concatenate queries should work  
test_that("concatenate_queries", {

  # set up test variables
  
  # a browse object
  poa_d <- browse(genus == "Poa")
  # a non-popler object 
  obj   <- c(1,2)
  
  # expected outputs --------------------------------------------------
  
  # expected output - a call
  expect_equal( class( concatenate_queries( poa_d )), "call")
  expect_equal( class( concatenate_queries(browse(genus == "Poa")) ), "call")
  expect_equal( class( concatenate_queries(poa_d, year == 2000) ), "call")
  expect_equal( class( concatenate_queries(browse(genus == "Poa"), year == 2000) ), "call")
  
  # expected output - at least one "&" in call if two elements are provided
  call_conc <- concatenate_queries(browse(genus == "Poa"), year == 2000) %>%
                deparse()
  expect_true( grepl("&",call_conc))
  
  
  #expected errors ------------------------------------------------------
  
  #1. More than 2 arguments
  expect_error( concatenate_queries(poa_d, poa_d, poa_d) )
  expect_error( concatenate_queries(obj, obj, obj) )
  expect_error( concatenate_queries(poa_d, poa_d, obj, obj) )
  expect_error( concatenate_queries(year == 2000, year == 2000, year == 2000) )
  #2. two logical statements
  expect_error( concatenate_queries(year == 2000, year == 2000) )
  #3. two popler objects
  expect_error( concatenate_queries(poa_d, poa_d) )
  #4. any object that is not a popler object
  expect_error( concatenate_queries(poa_d, poa_d) )
  
  rm( list = c("poa_d", "obj") )
  
})


