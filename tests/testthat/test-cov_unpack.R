context('cov_unpack') 

data_obj <- pplr_get_data( proj_metadata_key == 11 )

test_that('cov_unpack', {
  
  skip_on_cran()
  
  covs <- pplr_cov_unpack(data_obj)
  
  classes <- lapply(covs, function(x) inherits(x, 'factor'))
  expect_true(all(unlist(classes)))
  
})
