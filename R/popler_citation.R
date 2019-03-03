#' Provide citations for a popler browse or get_data object.
#'
#' Returns a bibliography, Bibtex citations, and acknowledgement template.
#' @param input An object  of class \code{browse} or \code{get_data}.
#' @param bibtex_path Specify the filename and location for 
#' the generated markdown file (optional)
#' 
#' @return A list of references from \code{input}.
#' 
#' @importFrom utils person toBibtex bibentry
#' @export
#' 
#' @examples
#' \dontrun{
#' # make a browse object
#' metadata <- pplr_browse(proj_metadata_key %in% c(17, 317, 494))
#'
#' # cite the projects
#' cite <- pplr_citation(metadata)
#' 
#' # cite$bibliography          # the bibliography
#' # cite$Bibtex                # Bibtex entries for each dataset
#' # cite$acknowledgement       # acknowledgement template
#' }
#' 
pplr_citation <- function(input, bibtex_path = NULL){
  input <- rebrowse(input)
  
  # initialize a bibentry object
  bib <- bibentry()
  
  # loop over each project_metadata_key
  for(i in seq_len(nrow(input))){
    
    # DOI not present: citation based on URL
    if( input$doi_citation[i] == 'NA'){
    
      # remove periods
      input$authors[i] <- gsub("[.]", "", input$authors[i])
      
      # set a flag for et al.
      etal <- FALSE
      
      # if et al is in the authors, trigger the flag
      if(length(grep("et al", input$authors[i])) > 0){ 
        etal <- TRUE 
      }
      
      # remove et al if it occurs
      input$authors[i] <- gsub("et al", "", input$authors[i])
      
      # nake a vector of author names
      tmp <- unlist(strsplit(trimws(input$authors[i], "both"), ","))
      
      # create a "person" object for each author
      new_aut <- format_author(tmp[1])
      if(length(tmp) > 1) {
        for(j in 2:length(tmp)){
          new_aut <- c(new_aut, format_author(tmp[j]) )
        }
      }

      # if the et al flag was triggered, add an "et al" person
      if(etal){
        new_aut <- c(new_aut, person(given = "et al")) 
      }
      
      # use DOI url if available 
      if( input$doi[i] == 'NA' ){
        link_out <- input$metalink[i] 
      } else{
        link_out <- input$doi[i] 
      }
        
      # create a bibentry for each proj_metadata_key
      bib <- c(bib, bibentry(
        bibtype = "Misc",
        title   = input$title[i],
        author  = new_aut,
        year    = input$studyendyr[i],
        note    = input$lter_name[i],
        url     = link_out))
      
    } else {
    # DOI-based citation
    
    
      # author, title, and year affiliated with DOI citation
      authors_l_temp  <- strsplit(input$doi_citation[i], '[0-9]{4}')[[1]][1]
      
      authors_l <- gsub("\\.$",'', authors_l_temp) %>% 
                      strsplit(',')  %>% 
                      lapply(trimws, 'both') %>% 
                      unlist
      
      author_doi_temp  <- lapply(authors_l, format_doi_author) %>% 
                        unlist %>% 
                        paste0(collapse = ', ')
                        # Accommodate weird case with NTL studies
      author_doi <- gsub('Lead,  PI,','Lead PI NTL,', author_doi_temp)

      title_doi_temp_1 <- regmatches(input$doi_citation[i],
                              gregexpr(' [0-9]{4}\\.(?s)(.*)http',
                                       input$doi_citation[i], perl = TRUE) ) 
      
      title_doi_temp_2 <- gsub('^ [0-9]{4}\\.','', title_doi_temp_1) 
      
      title_doi_temp_3 <- gsub('http$','',title_doi_temp_2) 
      
      title_doi <- gsub('\\.\\.','.',title_doi_temp_3) %>% 
                        trimws('both')

      year_doi_temp   <- regmatches(input$doi_citation[i], 
                              gregexpr("[[:digit:]]{4}", 
                              input$doi_citation[i])) %>% 
                              unlist
      
      year_doi <- year_doi_temp[1]
      
      bib <- c(bib, bibentry(
               bibtype = "Misc",
               title   = title_doi,
               author  = author_doi,
               year    = year_doi,
               note    = input$lter_name[i],
               url     = input$doi[i]) )
         
    }
    
  }
  
  # write a bibtex file if path is specified
  if(!is.null(bibtex_path)) {
    sink(file = bibtex_path)
    print(toBibtex(bib))
    sink()
  }
  
  lter_names <- sort(unique(input$lter_name))
  # generate an acknowledgement template
  acknowledgement <- paste0("The data in this study were collected ",
                            "by the following NSF LTER research programs: ", 
                            paste0(lter_names[-length(lter_names)],
                                   collapse=", "),
                            ", and ", lter_names[length(lter_names)], ".")
  
  # return a list
  return(list(bibliography = bib,
              Bibtex = toBibtex(bib),
              acknowledgement = acknowledgement))
}


#' @noRd
# format authros coming from column "doi_citatin"
format_doi_author <- function(author_string){
  
  # initials
  init_n <- regmatches(author_string,
                       gregexpr("^[[:alpha:]]{1}\\.| [[:alpha:]]{1}$| [[:alpha:]]{1}\\.$",
                                author_string)) %>%
                unlist %>% 
                trimws('both')
  
  # last name
  last_n <- regmatches(author_string,
                       gregexpr("[[:alpha:]]{2,20}",
                                author_string)) %>%
                unlist
  
  return( paste(init_n, last_n, sep=' ') )
  
}


#' @noRd
# format authros not coming from column "doi_citatin"
format_author <- function(author_string){
  
  first_n_temp  <- author_string %>% 
                trimws( 'both' ) %>% 
                strsplit( ' ' ) %>% 
                unlist 
  
  first_n <- first_n_temp[1] %>% 
                substr(1,1)
  
  second_n_temp <- author_string %>% 
    trimws( 'both' ) %>% 
    strsplit( ' ' ) %>% 
    unlist 
  
  second_n <- second_n_temp[2]
  # third name accommodates separated last names (e.g. del Moral, van Pelt)
  third_n_temp  <- author_string %>% 
                trimws( 'both' ) %>% 
                strsplit( ' ' ) %>% 
                unlist
  third_n <- third_n_temp[3]
  
  if( is.na(third_n) ) third_n = NULL
  
  return( paste(second_n, third_n, first_n,sep=' ') %>% 
            person )
  
}
