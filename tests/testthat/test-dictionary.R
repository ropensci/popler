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
  expect_equal(length(pplr_dictionary(lterid, lng_lter, lat_lter,
                                      treatment, structure, species)), 6)
  
  # class of list elements
  
  # first, exceptions to general rules
  expect_true(inherits(pplr_dictionary(species)[[1]], "character"))
  expect_true(inherits(pplr_dictionary(structure)[[1]], 'character'))
  expect_true(inherits(pplr_dictionary(treatment)[[1]], 'character'))
  expect_true(inherits(pplr_dictionary(proj_metadata_key)[[1]], 'integer'))
  
  # make sure others are correct
  expect_true(inherits(pplr_dictionary(lterid)[[1]], "character"))
  expect_true(inherits(pplr_dictionary(duration_years)[[1]], 'table'))
  
  
  # Fails correctly
  expect_error(pplr_dictionary(lter))
  
})
