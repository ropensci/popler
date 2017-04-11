# returns a bibentry object
popler_cite = function(input, bibtex_path=NULL){
  
  input <- rebrowse(input)
  
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