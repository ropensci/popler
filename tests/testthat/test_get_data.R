# test the get_data function, and associated utility functions
library(testthat)
context("get_data() utility functions")


# Does query_cols() return the expected number of columns?
test_that("query_cols", {
  
  # possible columns 
  potential_vars  <- popler:::query_cols()
  # tests
  expect_equal( length(potential_vars$all_cols), 107)
  expect_equal( length(potential_vars$default_cols), 19)

})


# Does inherit_search() accurately identify the variables mentioned in a "browse" logical statement?   
test_that("inherit_search", {
  
  # possible columns 
  potential_vars  <- popler:::query_cols()
  
  # testing objects
  sev       <- as.character( attributes(browse(lterid == "SEV"))$search_argument )
  author    <- as.character( attributes(browse(authors == "Scott Collins"))$search_argument )
  fund      <- as.character( attributes(browse(currently_funded == "0"))$search_argument )
  sev_cla   <- as.character( attributes(browse(lterid == "SEV" & class == "Insecta"))$search_argument )
  sev_ord   <- as.character( attributes(browse(lterid == "SEV" & order == "Diptera"))$search_argument )
  
  # "ordr/clss" to "order/class"
  sev_ord   <- gsub("order", "ordr", sev_ord)
  sev_cla   <- gsub("class", "clss", sev_cla)
  
  # tests
  expect_equal(popler:::inherit_search(potential_vars$all_cols, sev), "lterid")
  expect_equal(popler:::inherit_search(potential_vars$all_cols, author), "authors")
  expect_equal(popler:::inherit_search(potential_vars$all_cols, fund), "currently_funded")
  expect_equal(popler:::inherit_search(potential_vars$all_cols, sev_cla), c("lterid","clss") )
  expect_equal(popler:::inherit_search(potential_vars$all_cols, sev_ord), c("lterid","ordr") )
  
  rm( list = c("sev", "author", "fund", "sev_cla", "sev_ord") )
  
})


# Does inherit_variables() accurately identify the variables mentioned in MULTIPLE logical statement?   
test_that("inherit_variables", {
  
  # possible columns 
  potential_vars  <- popler:::query_cols()
  
  # testing objects
  aut_lter      <- popler:::inherit_variables(browse(authors == "Scott Collins"),
                                              substitute(lterid == "SEV"), 
                                                         potential_vars$all_cols)
  aut_clss      <- popler:::inherit_variables(browse(authors == "Scott Collins"),
                                              substitute(class == "Insecta"), 
                                                         potential_vars$all_cols)
  aut_ordr      <- popler:::inherit_variables(browse(authors == "Scott Collins"),
                                              substitute(order == "Carnivora"), 
                                                         potential_vars$all_cols)
  fun_aut_lter  <- popler:::inherit_variables(browse(currently_funded == "1"),
                                              substitute(authors == "Scott Collins" & lterid == "SEV"), 
                                                         potential_vars$all_cols)
  
  # tests
  expect_equal(aut_lter, c("authors", "lterid") )
  expect_equal(aut_clss, c("authors", "clss") )
  expect_equal(aut_ordr, c("authors", "ordr") )
  expect_equal(fun_aut_lter, c("currently_funded", "authors", "lterid") )
  
  rm( list = c("aut_lter", "aut_clss", "aut_ordr", "fun_aut_lter") )
  
})


# combine subset statements and translate them into sql 
test_that("inherit_variables", {
  
  aut       <- browse(authors == "Scott Collins")
  current   <- browse(currently_funded == "1" | lterid == "SEV")   
  
  # only one argument
  expect_equal(popler:::subset_arguments(aut),"authors = 'Scott Collins'")
  expect_equal(popler:::subset_arguments(subset = substitute(lterid == "SEV") ),
               "lterid = 'SEV'")
  # multiple arguments
  expect_equal(popler:::subset_arguments(aut, substitute(lterid == "SEV")),
               "authors = 'Scott Collins' AND lterid = 'SEV'")
  # multiple statements and multiple arguments
  expect_equal(popler:::subset_arguments(current, substitute(class == "Insecta")),
               "currently_funded = '1' OR lterid = 'SEV' AND clss = 'Insecta'")
  expect_equal(popler:::subset_arguments(current, substitute(class == "Insecta" | order == "Carnivora")),
               "currently_funded = '1' OR lterid = 'SEV' AND clss = 'Insecta' OR ordr = 'Carnivora'")
  
  rm(list=c("aut", "current"))
  
})

