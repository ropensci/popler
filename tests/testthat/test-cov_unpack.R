context('cov_unpack') 


# "mock" download of data object using a stub ---------------------------

# Invoke the stub function with the function that needs to be mocked
pplr_query_stub <- stubthat::stub( popler:::pplr_query )

# Define the behavior of the stub
pplr_query_stub$withArgs( proj_id = 1 )$returns( int.data$data_1 )
pplr_query_stub$withArgs( proj_id = 6 )$returns( int.data$data_6 )
pplr_query_stub$withArgs( proj_id = 221 )$returns( int.data$data_221 )

# download datasets within mockr 
downloader <- function(...){
  mockr::with_mock(pplr_query = pplr_query_stub$f,
                   pplr_get_data(...),
                   .env=environment( pplr_get_data ) )
}

# use the stub to produce data object
data_obj   <- downloader( proj_metadata_key == 1 )


# run tests -----------------------------------------------------
test_that('cov_unpack', {
  
  skip_on_cran()
  
  covs <- pplr_cov_unpack(data_obj)
  
  classes <- lapply(covs, function(x) inherits(x, 'factor'))
  expect_true(all(unlist(classes)))
  
})
