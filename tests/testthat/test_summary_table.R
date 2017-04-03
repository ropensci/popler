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

