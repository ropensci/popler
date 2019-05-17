context('test report_dictionary')

library(fs)
library(ggplot2)

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
test_that('Output file types are correct', {
  skip_on_cran()
  
  rmd_file <- tempfile(fileext = '.Rmd')
  html_file <- tempfile(fileext = '.html')
  
  # short table
  pplr_report_dictionary(md_file = rmd_file,
                         html_file = html_file)
  
  expect_equal(sum(grepl('*[.]Rmd|*[.]html', fs::dir_ls(tempdir()))), 2)
  
  # Cleanup for next test
  paths <- fs::dir_ls(tempdir())[grepl('*[.]Rmd|*[.]html|*[_]files',
                                       fs::dir_ls(tempdir()))]
  
  fs::file_delete(paths)
  
  # long table
  pplr_report_dictionary(full_tbl = TRUE, 
                         md_file = rmd_file,
                         html_file = html_file)
  
  expect_equal(sum(grepl('*[.]Rmd|*[.]html', fs::dir_ls(tempdir()))), 2)
  
  # Cleanup for next test
  paths <- fs::dir_ls(tempdir())[grepl('*[.]Rmd|*[.]html|*[_]files',
                                       fs::dir_ls(tempdir()))]
  
  fs::file_delete(paths)

  
})

test_that('report_metadata can use all types of input', {

  skip_on_cran()
  
  # browse
  pplr_report_metadata(pplr_browse(lterid == 'MCR'), 
                       md_file = tempfile(fileext = '.Rmd'),
                       html_file = tempfile(fileext = '.html'))
  
  expect_equal(sum(grepl('*[.]Rmd|*[.]html', fs::dir_ls(tempdir()))), 2)
  
  # Cleanup for next test
  paths <- fs::dir_ls(tempdir())[grepl('*[.]Rmd|*[.]html|*[_]files',
                                       fs::dir_ls(tempdir()))]
  
  fs::file_delete(paths)
  
  
  # Get_data
  
  pplr_report_metadata(data_obj,
                       md_file = tempfile(fileext = '.Rmd'),
                       html_file = tempfile(fileext = '.html'))
  
  expect_equal(sum(grepl('*[.]Rmd|*[.]html', fs::dir_ls(tempdir()))), 2)
  
  paths <- fs::dir_ls(tempdir())[grepl('*[.]Rmd|*[.]html|*[_]files',
                                       fs::dir_ls(tempdir()))]
  
  fs::file_delete(paths)
  
})
