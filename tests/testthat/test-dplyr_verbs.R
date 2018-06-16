context('dplyr verbs')


test_that('verbs return correct classes', {
  
  skip_on_cran()
  
  # filter
  expect_is(filter(pplr_browse(), lterid == 'PAL'),
            'browse')
  expect_is(filter(pplr_get_data(proj_metadata_key == 221),
                   species == 'sedges'),
            'get_data')
  
  # mutate
  expect_is(mutate(pplr_browse(lterid == 'PAL'),
                   new_col = 1,
                   new_col2 = 42),
            'browse')
  
  expect_is(mutate(pplr_get_data(proj_metadata_key == 221),
                   new_col = 1,
                   new_col2 = 42), 
            'get_data')
  
})

library(magrittr)
test_that('piping works', {
  
  skip_on_cran()
  
  expect_is(pplr_browse() %>%
              filter(lterid == 'SEV') %>% 
              pplr_maps(return_plot = TRUE),
            'list')
  
  expect_true(any(pplr_browse() %>%
                    filter(lterid == 'SEV') %>%
                    class() %in% 'browse'))
  
  expect_true(any(pplr_browse() %>%
                    mutate(new_col = 22) %>% 
                    class() %in% 'browse'))
  
  expect_true(any(pplr_get_data(proj_metadata_key == 221) %>%
                    filter(species == 'sedges') %>%
                    class() %in% 'get_data'))
  
  expect_true(any(pplr_get_data(proj_metadata_key == 221) %>%
                    mutate(new_col = 42) %>%
                    class() %in% 'get_data'))
  
  
})