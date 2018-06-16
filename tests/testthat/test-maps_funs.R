context('lter_map functions') 

library(maps)
library(mapproj)

test_that('produce proper output', {
  skip_on_cran()
  
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'SBC')),
            'list')
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'SBC'))$data, 
            'data.frame')
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'SBC'))$breaks,
            'numeric')

  # map_data <- popler:::prep_map_data(pplr_browse(lterid == 'MCR'))
  # 
  # counts <- map_data$data
  # sizes <- map_data$breaks
  
  expect_is(popler:::ak_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'ARC'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count),
                            size_breaks = popler:::prep_map_data(pplr_browse(lterid == 'ARC'))$breaks),
            'ggplot')
  
  expect_is(popler:::us_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'SBC'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count),
                            size_breaks = popler:::prep_map_data(pplr_browse(lterid == 'SBC'))$breaks),
            'ggplot')
  
  expect_is(popler:::an_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'PAL'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count),
                            size_breaks = popler:::prep_map_data(pplr_browse(lterid == 'PAL'))$breaks),
            'ggplot')
  
  expect_is(popler:::mc_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count),
                            size_breaks = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$breaks),
            'ggplot')
  
  expect_is(pplr_maps(pplr_browse(lterid == 'SEV'),
                      return_plot = TRUE),
            'list')
  
  expect_equal(length(pplr_maps(pplr_browse(lterid == 'SEV'),
                                return_plot = TRUE)),
               1)
  expect_equal(length(pplr_maps(pplr_browse(lterid == 'SEV' | 
                                            lterid == 'MCR'),
                                return_plot = TRUE)),
               2)
  expect_equal(length(pplr_maps(pplr_browse(lterid == 'SEV' | 
                                            lterid == 'MCR' |
                                            lterid == 'PAL'),
                                return_plot = TRUE)),
               3)
  expect_equal(length(pplr_maps(pplr_browse(), 
                                return_plot = TRUE)),
               4)
})