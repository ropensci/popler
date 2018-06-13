context('popler_citation')

test_that('output class is correct', {
  expect_is(pplr_citation(pplr_browse(proj_metadata_key == 17)), 'list')
  expect_is(pplr_citation(pplr_browse(proj_metadata_key == 17))$Bibtex,
            'Bibtex')
  expect_is(pplr_citation(pplr_browse(proj_metadata_key == 17))$acknowledgement,
            'character')
  
  expect_equal(length(pplr_citation(pplr_browse(proj_metadata_key %in% c(17, 
                                                                         317)))),
               3)
  
  expect_error(pplr_citation(pplr_browse(proj_metadata_key %in% c(40000))))
})