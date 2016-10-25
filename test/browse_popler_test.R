# test the browse_popler function 
context("browse_popler() function")

# does this function actually return character columns? 
test_that("Informational messages", {
  
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,2]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,3]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,4]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,8]),T)
  expect_equal(is.character(popler:::formatMainTable(popler:::dataPoplerFunction)[,9]),T)
  
}

# Do informational messages provide the expected output?
test_that("Informational messages", {
  
  # Initial group_factors
  possibleargs <- tolower(c("lterid","proj_metadata_key","title","metalink","studytype","community","duration_years",
                            "study_site_key","datatype", "structured",
                            "species","taxonomy","kingdom","phylum","clss","ordr","family","genus"))
  
  # errors in group_factors
  expect_error(popler:::errorsGroup("lter_id",possibleargs))
  expect_error(popler:::errorsGroup("proj_metadata_fkey",possibleargs))
  expect_error(popler:::errorsGroup("titles_study",possibleargs))
  expect_error(popler:::errorsGroup("metadatalink",possibleargs))
  
  # errors in tally_by
  expect_error(popler:::errorsTally("lter_id",possibleargs))
  expect_error(popler:::errorsTally("proj_metadata_fkey",possibleargs))
  expect_error(popler:::errorsTally("titles_study",possibleargs))
  expect_error(popler:::errorsTally("metadatalink",possibleargs))
  
  # errors linkes to group_factors
  expect_output(popler:::infoMessage(group_factors=NULL,tally_by=NULL,
                                         criteria=NULL,possibleargs))
  expect_equal(nrow(popler:::infoMessage(group_factors=NULL,tally_by=NULL,
                                         criteria=NULL,possibleargs)),18)
  expect_equal(popler:::infoMessage(group_factors="character",tally_by=NULL,
                                    criteria=NULL,possibleargs),NULL)
  expect_equal(popler:::infoMessage(group_factors=NULL,tally_by="character",
                                    criteria=NULL,possibleargs),NULL)
  expect_equal(popler:::infoMessage(group_factors=NULL,tally_by=NULL,
                                    criteria="character",possibleargs),NULL)
  
})


# multiple columns
test_that("Multiple columns", {
  
  expect_equal(popler:::multipleColumns("taxonomy")[1],"kingdom")
  expect_equal(popler:::multipleColumns("taxonomy")[2],"phylum")
  expect_equal(popler:::multipleColumns("taxonomy")[3],"clss")
  expect_equal(popler:::multipleColumns("taxonomy")[4],"ordr")
  expect_equal(popler:::multipleColumns("taxonomy")[5],"family")
  expect_equal(popler:::multipleColumns("taxonomy")[6],"genus")
  expect_equal(popler:::multipleColumns("taxonomy")[7],"species")
  
  expect_equal(popler:::multipleColumns("species")[1],"genus")
  expect_equal(popler:::multipleColumns("species")[2],"species")
  
})



# multiple columns
test_that("Multiple columns", {
  
  expect_equal(popler:::multipleColumns("taxonomy")[1],"kingdom")
  expect_equal(popler:::multipleColumns("taxonomy")[2],"phylum")
  expect_equal(popler:::multipleColumns("taxonomy")[3],"clss")
  expect_equal(popler:::multipleColumns("taxonomy")[4],"ordr")
  expect_equal(popler:::multipleColumns("taxonomy")[5],"family")
  expect_equal(popler:::multipleColumns("taxonomy")[6],"genus")
  expect_equal(popler:::multipleColumns("taxonomy")[7],"species")
  
  expect_equal(popler:::multipleColumns("species")[1],"genus")
  expect_equal(popler:::multipleColumns("species")[2],"species")
  
})


# Select By Criteria function
test_that("Select by criteria", {
  
  # Data
  x <- popler:::formatMainTable(popler:::dataPoplerFunction)

  # Test 1 
  subsetDat <- popler:::selectByCriteria(x,substitute(kingdom == "Plantae"))
  expect_equal( unique(subsetDat$kingdom), "Plantae")
  # Test 2
  subsetDat <- popler:::selectByCriteria(x,substitute(lter_table_fkey == "SEV"))
  expect_equal( unique(subsetDat$lter_table_fkey), "SEV")
  # Test 3
  subsetDat <- popler:::selectByCriteria(x,substitute(proj_metadata_key == 1))
  expect_equal( unique(subsetDat$proj_metadata_key), 1)
  
  rm(list=c("x","subsetDat"))
  
})


# Tally function
test_that("Tally function", {
  
  # Data frame
  x         <- popler:::formatMainTable(popler:::dataPoplerFunction)
  subsetDat <- popler:::selectByCriteria(x,substitute(kingdom == "Plantae"))
  
  # Tests ()
  expect_equal( names(popler:::tall(subsetDat,"species",NULL,T)), "species_tally" )
  expect_equal( names(popler:::tall(subsetDat,"clss",NULL,T)), "clss_tally" )
  expect_equal( names(popler:::tall(subsetDat,"title",NULL,T)), "title_tally" )
  
  rm(list=c("x","subsetDat"))
  
})


# Uniquevalues function
uniqueValues(subsetDat,columnNames,trim)

test_that("Uniquevalues function", {
  
  # Data frame
  x         <- popler:::formatMainTable(popler:::dataPoplerFunction)
  subsetDat <- popler:::selectByCriteria(x,substitute(kingdom == "Plantae"))
  
  # Tests  
  expect_equal( nrow(popler:::uniqueValues(subsetDat,"species",T)), 
                length(unique(subsetDat$species)) )
  expect_equal( nrow(popler:::uniqueValues(subsetDat,"lterid",T)), 
                length(unique(subsetDat$lterid)) )
  expect_equal( nrow(popler:::uniqueValues(subsetDat,"common_name",T)), 
                length(unique(subsetDat$common_name)) )
  
  rm(list=c("x","subsetDat"))
  
})
