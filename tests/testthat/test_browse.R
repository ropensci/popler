# test the browse_popler function 
library(testthat)
context("browse() function")

# does this function actually return character columns? 
test_that("Format Main Table", {
  
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,2]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,3]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,4]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,8]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,9]),T)
  
})

# Do informational messages provide the expected output?
test_that("Informational messages", {
  
  # Initial group_factors
  possibleargs <- tolower(c("lterid","proj_metadata_key","title","metalink","studytype","community","duration_years",
                            "study_site_key","datatype", "structured",
                            "species","taxonomy","kingdom","phylum","clss","ordr","family","genus"))
  
  # errors standard table
  expect_error(popler:::err_standard_tab("lter_id",possibleargs))
  expect_error(popler:::err_standard_tab("titles_study",possibleargs))
  expect_error(popler:::err_standard_tab("metadatalink",possibleargs))
  
  # errors table
  expect_error(popler:::err_full_tab("pi",possibleargs))
  expect_error(popler:::err_full_tab("proj_metadata_fkey",possibleargs))
  expect_error(popler:::err_full_tab("rep_level_3",possibleargs))
  expect_error(popler:::err_full_tab("treatment",possibleargs))
  
})
