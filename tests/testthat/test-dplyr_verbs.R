context('dplyr verbs')


browse_obj <- pplr_browse()
data_obj <- pplr_get_data(proj_metadata_key == 221)

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