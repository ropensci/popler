context('cov_unpack') 


summary_table <- pplr_summary_table_import()

exp_data <- pplr_get_data(lterid == 'MCR')


test_that('cov_unpack', {
  
  skip_on_cran()
  
  covs <- pplr_cov_unpack(exp_data)
  
  classes <- lapply(covs, function(x) inherits(x, 'factor'))
  expect_true(all(unlist(classes)))
  
})
