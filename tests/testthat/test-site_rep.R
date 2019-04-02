context('pplr_site_rep')


# "mock" download of data object using a stub ---------------------------

# Invoke the stub function with the function that needs to be mocked
pplr_query_stub <- stubthat::stub( popler::pplr_query )

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
data_obj   <- downloader( proj_metadata_key == 6 )


# run tests -----------------------------------------------------
test_that('pplr_site_rep returns correct types', {
  skip_on_cran()
  
  ids <- pplr_site_rep(data_obj, 
                       freq = 2, 
                       duration = 6,
                       rep_level = 2, 
                       return_logical = TRUE)
  
  expect_true(is.logical(ids))
  
  expect_true(length(ids) == dim(data_obj)[1])
  
  summary_tab <- pplr_site_rep(data_obj,
                               freq = 2,
                               duration = 6,
                               rep_level = 2,
                               return_logical = FALSE)
  
  # two spatial columns, 1 year, 1 sample n
  expect_true(dim(summary_tab)[2] == 4)
  
  expect_true(all(names(summary_tab)[1:2] %in% names(data_obj)))
  
  
  # test lower frequencies
  
  ids2 <- pplr_site_rep(data_obj,
                        freq = 0.5,
                        duration = 4,
                        rep_level = 2,
                        return_logical = TRUE)
  
  expect_true(is.logical(ids2))
  expect_true(sum(ids2) > sum(ids))
  
  summary_tab_2 <- pplr_site_rep(data_obj,
                                 freq = 0.5,
                                 duration = 4,
                                 rep_level = 2,
                                 return_logical = FALSE)
  
  expect_true(dim(summary_tab_2)[1] > dim(summary_tab)[1])
  expect_true(dim(summary_tab_2)[2] == dim(summary_tab)[2])
  
})


library(ggplot2)
test_that('pplr_site_rep_plot() returns correct values', {
  skip_on_cran()

  expect_is(pplr_site_rep_plot(data_obj, return_plot = TRUE),
            'ggplot')
  
  y <- pplr_site_rep_plot(data_obj, return_plot = FALSE)
  
  expect_true(identical(data_obj, y))
  
}) 

test_that('fails correctly', {
  
  skip_on_cran()
  
  expect_error(pplr_site_rep(data_obj, rep_level = 6))
  expect_error(pplr_site_rep(data_obj, duration = 55))
  expect_error(pplr_site_rep(data_obj, freq = 7))
  
  # project 6 only has 4 levels of spatial replication
  expect_error(pplr_site_rep(data_obj, rep_level = 5))
  
  wrong <- pplr_browse(lterid == 'PAL')
  expect_error(pplr_site_rep(wrong))
  
  expect_error(pplr_site_rep_plot(wrong))
  

})
