#' Provide citations for a popler browse or get_data object.
#'
#' Returns a bibliography, Bibtex citations, and an acknowledgement template.
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
    if( input$doi[i] == 'NA'){
    
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
      new_aut <- person(trimws(tmp[1], "both"))
      if(length(tmp) > 1) {
        for(j in 2:length(tmp)){
          new_aut <- c(new_aut, person(trimws(tmp[j], "both")))
        }
      }
      
      # if the et al flag was triggered, add an "et al" person
      if(etal){
        new_aut <- c(new_aut, person(given = "et al")) 
      }
      
      # create a bibentry for each proj_metadata_key
      bib <- c(bib, bibentry(
        bibtype = "Misc",
        title = input$title[i],
        author = new_aut,
        year = input$studyendyr[i],
        note = input$lter_name[i],
        url = input$metalink[i]))
      }
    # DOI-based citation
    else{
    
      # author, title, and year affiliated with DOI citation
      authors_l  <- strsplit(input$doi_citation[i], '[0-9]{4}')[[1]][1] %>% 
                      trimws( 'both' ) %>% 
                      gsub("\\.$",'',.) %>% 
                      strsplit(',')  
      
      # format for bibentry
      format_author <- function(x){
        
        # initials
        init_n <- x %>% 
                  regmatches(.,
                             gregexpr("[[:alpha:]]{1}\\.",
                             .)) %>%
                    unlist
        
        # last name
        last_n <- x %>% 
                  regmatches(.,
                             gregexpr("[[:alpha:]]{2,20}",
                             .)) %>%
                    unlist
        
        return( paste(init_n, last_n, sep=' ') )
        
      }
      
      author_doi  <- lapply(authors_l, format_author) %>% 
                        unlist %>% 
                        paste0(collapse=', ')

      title_doi  <- strsplit(input$doi_citation[i], '.[0-9]{4}\\. ')[[1]][2] %>% 
                      strsplit(split='https') %>% 
                      unlist %>% 
                      .[1] %>% 
                      # remove white space and double dots 
                      trimws('both')
      
      year_doi   <- regmatches(input$doi_citation[i], 
                              gregexpr("[[:digit:]]{4}", 
                              input$doi_citation[i])) %>% 
                              unlist %>% 
                              .[1]
      
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
