#' A user-friendly dictionary of the popler metadata
#'
#' Provides information on the variables of metadata contained in the popler 
#' database, and the kind of data contained in those variables.
#' 
#' @param full_tbl logical; if \code{TRUE} function returns the variables 
#' contained in the full main table. If \code{FALSE}, functions returns only the
#' standard variables. Default is \code{FALSE}.
#' @param md_file Specify the filename and location for 
#' the generated markdown file (optional)
#' @param html_file Specify the filename and location for the 
#' generated html file (optional)
#' 
#' @return This function is called for its side effects and does not 
#' return an R object.
#' 
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @export
#' 
#' @examples
#' \dontrun{
#' # Full dictionary
#' pplr_report_dictionary(full_tbl = TRUE)
#' 
#' # "Abridged" version
#' pplr_report_dictionary()
#' }
#' 
pplr_report_dictionary <- function(full_tbl=FALSE, md_file=NULL, html_file=NULL){
  
  # store explanations as table of contents
  if(full_tbl){
    TOC <- int.data$explanations
    
    # remove contents that do not work
    TOC <- TOC[-77, ]
    
  } else {
    TOC <- int.data$explain_short
  }
  
  if(is.null(md_file)){
    md_file <- paste0(system.file("",package="popler"),"./dictionary.Rmd")
  }
  
  if(is.null(html_file)){
    html_file <- paste0(system.file("",package="popler"),"./dictionary.html")
  }
  
  # which entries should not be expanded?
  wide <- c("proj_metadata_key",
            "lter_project_fkey",
            "studystartyr",
            "studyendyr",
            "spatial_replication_level_1_number-of_unique_reps",
            "spatial_replication_level_2_number-of_unique_reps",
            "spatial_replication_level_3_number-of_unique_reps",
            "spatial_replication_level_4_number-of_unique_reps",
            "spatial_replication_level_5_number-of_unique_reps",
            "tot_spat_rep",
            "duration_years")
  
  # store entries
  entries <- eval(parse(text=paste0("pplr_dictionary(",
                                    paste0(TOC[,1] , 
                                           collapse=" , "),
                                    ")")))
  

  # build the .Rmd file piecewise
  header <- c(
'
---  
output:  
  html_document:  
    self_contained: no  
---  

<br>  

<img src= `r system.file("icon.png",package="popler")` alt="Drawing" style="height: 110px; float: right"/>  

<br>  
  
# *popler* Dictionary  

***  

*Before publishing any data gathered from popler, please review and adhere to the [LTER Network Data Access Policy, Data Access Requirements, and General Data Use Agreement](https://lternet.edu/policies/data-access), as well as any additional requirements indicated by the authors of each study.*  

***  
<a name="defs"></a>  

 Column Name | Definition
 --- | ---------------------------- 
'
  )
  
  defs <- c(
    '| [_NAME_](#C@@@) | _DEFINITION_ |
'
  )
  
  end_defs <- c(
    '|  | <span style="color:white"> ........................................................................................................ </span> |

***  
'    
  )
  
  ents <- c(
'
<a name=C@@@></a>  

#### _NAME_  
**_DEFINITION_**  

_ENTRY_  

<div style="text-align: right"> *[back to Definitions](#defs)* </div>

*** 
'    
  )
  
  # update defs and ents blocks
  defs_new <- rep(NA,nrow(TOC))
  ents_new <- rep(NA,nrow(TOC))
  for(i in seq_len(length(defs_new))){
    # make table of contents + definitions
    defs_new[i] <- gsub("_NAME_", TOC[i,1], defs)
    defs_new[i] <- gsub("_DEFINITION_", TOC[i,2], defs_new[i])
    defs_new[i] <- gsub("@@@",i,defs_new[i])
    
    # make lists of names
    if(TOC[i,1] %in% wide){ coll <- " , " } else { coll <- "<br>" }
    
    ents_new[i] <- gsub("_NAME_", TOC[i,1], ents)
    ents_new[i] <- gsub("_DEFINITION_", TOC[i,2], ents_new[i])
    ents_new[i] <- gsub("_ENTRY_", paste0(sort(entries[[i]]),collapse=coll), ents_new[i])
    ents_new[i] <- gsub("@@@",i,ents_new[i])
    
  }
  
  # make markdown file
  sink(md_file)
    # cat(header, defs_new, end_defs, ents_new)
    cat( iconv(header,   to="UTF-8"), 
         iconv(defs_new, to="UTF-8"), 
         iconv(end_defs, to="UTF-8"), 
         iconv(ents_new, to="UTF-8") )
  sink()
  
  # launch browser window
  rmarkdown::render(md_file, quiet=TRUE, encoding = "UTF-8")
  browseURL(html_file)
}
