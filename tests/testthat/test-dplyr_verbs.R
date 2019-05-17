context('dplyr verbs')

# download browse object
browse_obj <- pplr_browse()


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
data_obj   <- downloader( proj_metadata_key == 221 )


# run tests ------------------------------------------------------
test_that('verbs return correct classes', {
  
  skip_on_cran()
  
  # filter
  expect_is(filter(browse_obj, lterid == 'PAL'),
            'browse')
  expect_is(filter(data_obj,
                   species == 'sedges'),
            'get_data')
  
  # mutate
  expect_is(mutate(browse_obj,
                   new_col = 1,
                   new_col2 = 42),
            'browse')
  
  expect_is(mutate(data_obj,
                   new_col = 1,
                   new_col2 = 42), 
            'get_data')
  
})

library(magrittr)
test_that('piping works', {
  
  skip_on_cran()
  
  expect_is(browse_obj %>%
              filter(lterid == 'SEV') %>% 
              pplr_maps(return_plot = TRUE),
            'list')
  
  expect_true(any(browse_obj %>%
                    filter(lterid == 'SEV') %>%
                    class() %in% 'browse'))
  
  expect_true(any(browse_obj %>%
                    mutate(new_col = 22) %>% 
                    class() %in% 'browse'))
  
  expect_true(any(data_obj %>%
                    filter(species == 'sedges') %>%
                    class() %in% 'get_data'))
  
  expect_true(any(data_obj %>%
                    mutate(new_col = 42) %>%
                    class() %in% 'get_data'))
  
  
})