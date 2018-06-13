context('lter_map functions') 

test_that('produce proper output', {
  skip('passes interactively and in devtools::test, but not in devtools::check()')
  
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'MCR')),
            'list')
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$data, 
            'data.frame')
  expect_is(popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$breaks,
            'numeric')

  # map_data <- popler:::prep_map_data(pplr_browse(lterid == 'MCR'))
  # 
  # counts <- map_data$data
  # sizes <- map_data$breaks
  
  expect_is(popler:::ak_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count)),
            'ggplot')
  
  expect_is(popler:::us_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count),
                            size_breaks = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$breaks),
            'ggplot')
  
  expect_is(popler:::an_map(count_data = popler:::prep_map_data(pplr_browse(lterid == 'MCR'))$data,
                            x = rlang::quo(long),
                            y = rlang::quo(lat),
                            polygon_group = rlang::quo(group),
                            count_group = rlang::quo(count)),
            'ggplot')
})