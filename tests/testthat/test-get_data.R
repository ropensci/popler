# test the get_data function, and associated utility functions
context("get_data() utility functions")

# set up test variables

# a browse object
browseObj <- popler::pplr_browse(genus == "Poa")

# a non-popler object 
obj   <- c(1,2)

# Does query_cols() return the expected number of columns?
test_that("vars_query", {
  
  skip_on_cran()
  
  conn <- popler:::db_open()

  # possible columns 
  potential_vars  <- popler:::vars_query(conn)
  # tests
  expect_equal( length(potential_vars$all_vars), 111)
  expect_equal( length(potential_vars$default_vars), 28)

})


# Need to fix this stuff. Not sure what is going on, but browse_object is not
# begin found in test env
# # all conditinos in which concatenate queries should work  
# test_that("concatenate_queries", {
#   skip_on_cran()
#   # expected outputs 
#   
#   # expected output - a call
#   expect_true(inherits(popler:::concatenate_queries(browseObj), 'call'))
#   expect_true(inherits(popler:::concatenate_queries(pplr_browse(genus == "Poa")), 
#                         "call"))
#   expect_true(inherits(popler:::concatenate_queries(browseObj, year == 2000),
#                        'call'))
#   expect_true(inherits(popler:::concatenate_queries(pplr_browse(genus == "Poa"), 
#                                                     year == 2000), 'call'))
#   
#   # expected output - at least one "&" in call if two elements are provided
#   call_conc <- popler:::concatenate_queries(pplr_browse(genus == "Poa"), 
#                                             year == 2000) %>%
#                 deparse()
#   expect_true(grepl("&", call_conc))
#   
#   
#   #expected errors 
#   
#   #1. More than 2 arguments
#   expect_error( popler:::concatenate_queries(browseObj, browseObj, browseObj) )
#   expect_error( popler:::concatenate_queries(browseObj, duration_year == 5, obj))
#   expect_error( popler:::concatenate_queries(year == 2000, 
#                                              year == 1999, 
#                                              year == 2001) )
#   #2. two logical statements
#   expect_error( popler:::concatenate_queries(year == 2000, year == 2000) )
#   #3. two popler objects
#   expect_error( popler:::concatenate_queries(browseObj, browseObj) )
#   #4. any object that is not a popler object
#   
#   expect_error( popler:::concatenate_queries(browseObj, obj) )
#   
# 
# })


