# test the browse_popler function 
context("tunnel_query() functions")


# does this function actually return character columns? 
test_that("parse_to_sql_search()", {
  
  # translating" "class" into "clss" and "order" into "ordr"
  expect_equal(as.character(parse_to_sql_search(class == "Mammalia")), "clss = 'Mammalia'")
  expect_equal(as.character(parse_to_sql_search(order == "Primates")), "ordr = 'Primates'")
  
  #lazy vs. nonlazy evaluation
  expect_equal( parse_to_sql_search(proj_metadata_key == 21 | proj_metadata_key == 9),
                parse_to_sql_search("proj_metadata_key == 21 | proj_metadata_key == 9", lazy = F)
  )
  expect_equal( parse_to_sql_search(kingdom == "Animalia"),
                parse_to_sql_search("kingdom == 'Animalia'", lazy = F)
  )
  expect_equal( parse_to_sql_search(kingdom == "Animalia" & proj_metadata_key > 5),
                parse_to_sql_search("kingdom == 'Animalia' & proj_metadata_key > 5", lazy = F)
  )
  
}

# does this function actually return character columns? 
test_that("tunnel_query function", {
  
  # does it fetch the correct data set?
  expect_equal(nrow(tunnel_query(proj_metadata_key == 21)), 21623)
  expect_equal(nrow(tunnel_query(proj_metadata_key == 9)), 30250)
  
  # compare tunnel_query to tunnel_query_
  expect_equal(tunnel_query(proj_metadata_key == 9), 
               tunnel_query_("proj_metadata_key == 9")
               )

}


