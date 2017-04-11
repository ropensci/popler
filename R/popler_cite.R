library("urlshorteneR")

# returns a bibentry object
popler_cite = function(input, short_links=F, bibtex_path=NULL){
  
  # determine whether there's a get_data() object or a browse() object; get the
  # appropriate full_tbl browse() object
  
  # if it's not a browse object...
  if(is.null(input$proj_metadata_key)){
    # and if it's not a get_data object
    if(is.null(attributes(input)$unique_projects)){
      stop("Input must be a browse() object or a get_data() object.")
    } else {
      # get unique proj_metadata_keys from get_data object
      pmk <- paste0(attributes(input)$unique_projects,collapse=",")
    }
  }  else {
    #get unique proj_metadata_keys from browse object
    pmk <- paste0(input$proj_metadata_key,collapse=",")
  }
  
  # re-run browse to get full table and untrimmed output
  input <- eval(parse(text=paste0("browse(proj_metadata_key %in% c(", pmk,"), full_tbl=T, trim=F)")))
  
  # initialize a bibentry object
  bib <- bibentry()
  
  # loop over each project_metadata_key
  for(i in 1:nrow(input)){
    
    # remove periods
    input$authors[i] <- gsub("[.]", "", input$authors[i])
    
    # set a flag for et al.
    etal <- FALSE
    
    # if et al is in the authors, trigger the flag
    if(length(grep("et al", input$authors[i]))>0){ etal <- TRUE }
    
    # remove et al if it occurs
    input$authors[i] <- gsub("et al", "", input$authors[i])
    
    # nake a vector of author names
    tmp          <- unlist(strsplit(trimws(input$authors[i],"both"),","))
    
    # create a "person" object for each author
    new_aut      <- person(trimws(tmp[1],"both"))
    if(length(tmp)>1){
      for(j in 2:length(tmp)){
        new_aut <- c(new_aut,person(trimws(tmp[j],"both")))
      }
    }
    
    # if the et al flag was triggered, add an "et al" person
    if(etal){ new_aut <- c(new_aut,person(given="et al")) }
    
    # shorten metadata links?
    if(short_links==T){
      link <- isgd_LinksShorten(input$metalink[i])
    } else {
      link <- input$metalink[i]
    }
    
    # create a bibentry for each proj_metadata_key
    bib <- c(bib,bibentry(
      bibtype = "Misc",
      title = input$title[i],
      author = new_aut,
      year = input$studyendyr[i],
      note = input$lter_name[i],
      url = link))
  }
  
  # write a bibtex file if path is specified
  if(!is.null(bibtex_path)){
    sink(file=bibtex_path)
    print(toBibtex(bib))
    sink()
  }
  
  # generate an acknowledgement template
  acknowledgement = paste0("The data in this study were collected by the following NSF LTER research programs: ", 
                           paste0(unique(input$lter_name),collapse=", "),".")
  
  # return a list
  return(list(bibliography    = bib,
              Bibtex          = toBibtex(bib),
              acknowledgement = acknowledgement))
}

# A <- browse(community=="no" & datatype=="count",trim=F, full_tbl = T)
# B <- browse(community=="no" & datatype=="density")
# C <- get_data(B)
# 
# bib1 <- popler_cite(A, bibtex_path = "./test.bib")
# bib1$bibliography
# bib1$Bibtex
# bib1$acknowledgement
# 
# bib2 <- popler_cite(B)
# bib2$bibliography
# bib2$Bibtex
# bib2$acknowledgement
# 
# bib3 <- popler_cite(C)
# bib3$bibliography
# bib3$Bibtex
# bib3$acknowledgement
# 
# bib4 <- popler_cite(A, short_links=T)
# bib4$bibliography
# bib4$Bibtex
# bib3$acknowledgement
# 
# i <- 4
# bib <- bibentry(
#   bibtype = "Misc",
#   title = A$title[i],
#   author = A$authors[i],
#   year = A$studyendyr[i],
#   note = A$lter_name[i],
#   url = link)