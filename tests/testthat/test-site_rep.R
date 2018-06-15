context('pplr_site_rep')

library(ggplot2)
test_that('returns correct values', {
  skip_on_cran
  
  data <- pplr_get_data(lterid == 'PAL')
  
  expect_is(pplr_site_rep(data, return_plot = TRUE),
            'ggplot')
  
  y <- pplr_site_rep(data, return_plot = FALSE)
  
  expect_true(identical(data, y))
  
}) 

test_that('fails correctly', {
  
  data <- pplr_get_data(lterid == 'PAL')
  data$spatial_replication_level_4 <- NA
  
  wrong <- pplr_browse(lterid == 'PAL')
  
  expect_error(pplr_site_rep(wrong))
  
  expect_error(pplr_site_rep(data, rep_level = 4))
  
})