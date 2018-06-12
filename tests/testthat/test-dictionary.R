# dictionary function --------------------------------------------------
context("dictionary() function")

test_that("dictionary() function ", {
  skip_on_cran()
  
  # n of variables in "informational message"
  expect_equal(nrow(pplr_dictionary()), 18)
  expect_equal(nrow(pplr_dictionary(full_tbl = TRUE)), 76)
  
  # n of list elements
  expect_equal(length(pplr_dictionary(species)), 1)
  expect_equal(length(pplr_dictionary(lterid, lng_lter)), 2)
  
  # class of list elements
  expect_equal(class(pplr_dictionary(species)[[1]]), "data.frame")
  expect_equal(class(pplr_dictionary(lterid)[[1]]), "character")
  
})
