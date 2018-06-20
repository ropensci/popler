context('pplr_site_rep')


test_that('pplr_site_rep returns correct types', {
  skip_on_cran()
  
  data <- pplr_get_data(proj_metadata_key == 12)
  
  ids <- pplr_site_rep(data, 
                       freq = 2, 
                       duration = 12,
                       rep_level = 2, 
                       return_logical = TRUE)
  
  expect_true(is.logical(ids))
  
  expect_true(length(ids) == dim(data)[1])
  
  summary_tab <- pplr_site_rep(data,
                               freq = 2,
                               duration = 12,
                               rep_level = 2,
                               return_logical = FALSE)
  
  # two spatial columns, 1 year, 1 sample n
  expect_true(dim(summary_tab)[2] == 4)
  
  expect_true(all(names(summary_tab)[1:2] %in% names(data)))
  
  
  # test lower frequencies
  
  ids2 <- pplr_site_rep(data,
                        freq = 0.5,
                        duration = 12,
                        rep_level = 2,
                        return_logical = TRUE)
  
  expect_true(is.logical(ids2))
  expect_true(sum(ids2) > sum(ids))
  
  summary_tab_2 <- pplr_site_rep(data,
                                 freq = 0.5,
                                 duration = 12,
                                 rep_level = 2,
                                 return_logical = FALSE)
  
  expect_true(dim(summary_tab_2)[1] > dim(summary_tab)[1])
  expect_true(dim(summary_tab_2)[2] == dim(summary_tab)[2])
  
})




library(ggplot2)
test_that('pplr_site_rep_plot() returns correct values', {
  skip_on_cran()
  
  data <- pplr_get_data(lterid == 'PAL')
  
  expect_is(pplr_site_rep_plot(data, return_plot = TRUE),
            'ggplot')
  
  y <- pplr_site_rep_plot(data, return_plot = FALSE)
  
  expect_true(identical(data, y))
  
}) 

test_that('fails correctly', {
  
  skip_on_cran()
  data <- pplr_get_data(lterid == 'PAL')
  
  expect_error(pplr_site_rep(data, rep_level = 5))
  expect_error(pplr_site_rep(data, duration = 55))
  expect_error(pplr_site_rep(data, freq = 7))
  
  # Pal only has 2 levels of spatial replication
  expect_error(pplr_site_rep(data, rep_level = 3))
  
  wrong <- pplr_browse(lterid == 'PAL')
  expect_error(pplr_site_rep(wrong))
  
  expect_error(pplr_site_rep_plot(wrong))
  

})
