#' @title Download data from the popler database
#'
#' @description This function downloads datasets contained in the popler database. 
#' The user can download data directly, using a logical expression, or indirectly, 
#' using objects created by \code{pplr_browse}.
#' @param ... An object produced by \code{pplr_browse} or a logical expression.
#' @param cov_unpack logical; if \code{TRUE}, function \code{pplr_cov_unpack} 
#' is applied to the variable \code{covariates} of the downloaded dataset in 
#' order to extract the variables contained in therein and combine the new
#' columns with the default output. Default is \code{FALSE}.
#'  
#' @return This data fame is of class \code{get_data}, and \code{data.frame}.
#' 
#' 
#' @details. By default, the following variables are included when a user calls
#' \code{pplr_get_data()}.
#' 
#' \itemize{
#'   \item{\code{authors}}
#'   \item{\code{authors_contact}} 
#'   \item{\code{year}} 
#'   \item{\code{day}} 
#'   \item{\code{month}}
#'   \item{\code{sppcode}} 
#'   \item{\code{genus}}
#'   \item{\code{species}}
#'   \item{\code{datatype}}
#'   \item{\code{spatial_replication_level_1_label}}
#'   \item{\code{spatial_replication_level_1}}
#'   \item{\code{spatial_replication_level_2_label}}
#'   \item{\code{spatial_replication_level_2}}
#'   \item{\code{spatial_replication_level_3_label}}
#'   \item{\code{spatial_replication_level_3}}
#'   \item{\code{spatial_replication_level_4_label}}
#'   \item{\code{spatial_replication_level_4}}
#'   \item{\code{spatial_replication_level_5_label}}
#'   \item{\code{spatial_replication_level_5}}
#'   \item{\code{proj_metadata_key}}
#'   \item{\code{structure_type_1}}
#'   \item{\code{structure_type_2}}
#'   \item{\code{structure_type_3}}
#'   \item{\code{structure_type_4}}
#'   \item{\code{treatment_type_1}}
#'   \item{\code{treatment_type_2}}
#'   \item{\code{treatment_type_3}}
#'   \item{\code{covariates}}
#' }
#' @examples
#' \dontrun{
#' # browse a study, then get the data associated with it
#' parasite = pplr_browse(proj_metadata_key == 25)
#' gh_data = pplr_get_data(parasite)
#' 
#' # insect data sets from the SEV lter site
#' insect_sev = pplr_browse(class == "Insecta" & lterid == "SEV")
#' insect_25_yrs96_99 = pplr_get_data(insect_sev)
#' 
#' insect_21_25 = pplr_get_data( (proj_metadata_key == 43 | 
#'                                proj_metadata_key == 25) )
#'}
#'
#' @importFrom dplyr %>% select bind_rows
#' @importFrom rlang .data
#' @export

# Function that connects and gathers the information from the database

pplr_get_data <- function(..., cov_unpack = FALSE){

  # define possible variables ------------------------------------------
  
  # concatenate logical expressions specified in the '...' argument
  # expressions can be specified explicitly, implicitly (through an 
  # object produced by browse()), or both 
  c_calls <- concatenate_queries(...)
  
  # update the concatenated calls, in case c_calls contains
  # "structure" or "treatment"
  updated_calls <- call_update(c_calls)
  

  # query -----------------------------------------------------------------
  
  summary_table <- pplr_summary_table_import()
  
  # get id of studies
  id_vec        <- summary_table %>% 
                      subset( eval(updated_calls) ) %>% 
                      .$proj_metadata_key %>% 
                      unique
  
  # Check if you actually found a dataset
  if( length(id_vec) == 0 ){
    stop('No data found. Check to make sure query is correct',
         call. = FALSE)
  }

  # query popler online
  output_data <- pplr_query( id_vec )

  # format output ---------------------------------------------------------
  
  # set to numeric DATE information
  output_data <- output_data %>% 
                    mutate( year  = as.numeric(year),
                            month = as.numeric(month),
                            day   = as.numeric(day) )
  
  # set to numeric the observation variable
  obs_id      <- grep('observation', names(output_data) )
  output_data[,obs_id] <- output_data[,obs_id] %>% as.numeric
  
  # replace -99999, but only for numeric variables
  
  # function 
  replace_99              <- function(x) replace(x, x == -99999, NA)
  
  # substitute
  num_repl                <- sapply(output_data, 
                                    is.numeric) %>% as.vector()
  output_data[,num_repl]  <- plyr::colwise(replace_99)(as.data.frame(output_data[,num_repl]))

  # remove variables that whose content is just "NA"
  output_data <- base::Filter(function(x) !all(x == "NA"), output_data)
  
  # Change "ordr" and "clss" to "order" and "class"
  output_data <- colname_change("clss", "class", output_data)
  output_data <- colname_change("ordr", "order", output_data)
  output_data <- colname_change("count_observation", "abundance_observation", 
                                output_data)
  
  # unpack the covariates?
  if(cov_unpack == TRUE) {
    
    output_data <- output_data %>%
      dplyr::select(-.data$covariates) %>%
      cbind(pplr_cov_unpack(output_data))
  }
  
  # outputs -----------------------------------------------------------------
  
  # assign class
  output_data <- structure(output_data, 
                unique_projects = unique(output_data$proj_metadata_key),
                unique_authors  = unique(output_data[ ,c("proj_metadata_key",
                                                         "authors",
                                                         "authors_contact")]),
                class = c("get_data", class(output_data)) 
  )
  
  # Informational message
  data_message(output_data)
  
  return(output_data)
  
}


#' @importFrom lazyeval lazy_dots
#' @noRd
# a function to concatenate browse() outputs and new arguments
concatenate_queries <- function(...){
  
  # lazy_dots eval get_data query
  Q <- lazyeval::lazy_dots(...)
  
  # a list to store the outputs
  out <- list()
  
  # counters to check whether more than one browse() or new calls are used
  browse_calls <- 0
  new_calls <- 0
  
  if(length(Q) > 2){
    stop("You cannot enter more than two arguments:\n1) ",
         "an object returned by browse() and/or\n2) a logical", 
         "statement\nPlease refer to the '...' argument in ?get_data.")
  }
  
  # loop over all inputs
  for(i in seq_len(length(Q))) {
    if(class(Q[[i]]$expr) == "name") {
      
      # if class of object is "name" evaluate it to get
      # original browse() query
      tmp <- eval(Q[[i]]$expr)
      
      # if this variable isn't a popler object, throw an error
      if(class(tmp)[1] != "browse"){
        stop(paste0("Error using the following argument:\n\n      ", 
                    Q[[i]]$expr,
                    "\n\n  Only logical expressions or outputs from the ",
                    "'browse()' function may be used"))
      }
      
      # store search argument as output
      out[[i]] <- attributes(tmp)$search_expr
      
      # update counter
      browse_calls <- browse_calls + 1
    } else { 
      
      # if class of object is "call"...
      if(grepl("browse[(]", deparse(Q[[i]]$expr))) {
        
        # if the call is to browse(), evaluate is and then get the search arg
        out[[i]] <- attributes(eval(Q[[i]]$expr))$search_expr
        
        # update browse_calls counter
        browse_calls <- browse_calls + 1
      } else {
        # just save the expression
        out[[i]] <- Q[[i]]$expr
        
        # update calls counter
        new_calls <- new_calls + 1
      }
    }
  }
  
  # if either call counter is more than 1, call an error
  if(browse_calls > 1){
    stop("You cannot enter more than one browse() argument.\n",
         "Please refer to the '...' argument in ?get_data.")
  }
  if(new_calls > 1){
    stop("You cannot enter more than one logical expression.\n ",
         "Please refer to the '...' argument in ?get_data.")
  }

  LoopOut <- paste0(unlist(out), collapse = "&") 
  TextToParse <- paste0("substitute(", LoopOut, ")", collapse = "")
  
  # return a single logical call
  return(eval(parse(text = TextToParse)))
  
}


# query popler
#' @noRd
pplr_query <- function( proj_id ){
  
  if( !('proj_id' %in% ls()) ) {
    stop("No logical expression specified. Please specify what ",
         "data you wish to download from popler" )
  }
  
  # set in limits and offsets
  query_l   <- lapply(proj_id, offset_limit_search)
  
  # collapse in a single lits (if needed)
  if( length(query_l) > 1 ){
    ids_vec  <- purrr::map2(proj_id, query_l, 
                            function(x,y) rep(x,length(y$limit_v)) )
    query_in <- list( limit_v  = lapply(query_l, function(x) x$limit_v)  %>% 
                                    Reduce(function(...) c(...), .),
                      offset_v = lapply(query_l, function(x) x$offset_v) %>% 
                                    Reduce(function(...) c(...), .),
                      proj_id  = ids_vec %>% unlist)
  }else{
    query_in          <- query_l[[1]]
    query_in$proj_id  <- rep(proj_id, length(query_in$limit_v) )
  }
  
  # set up progress bar
  total     <- query_in$limit_v %>% length
  prog_bar  <- txtProgressBar(min = 0, max = total, style = 3)
  
  # actually download summary table
  downld_dataset <- function(lim,off,id,i){
                      setTxtProgressBar(prog_bar, i)
                      pop_search( id, limit = lim, offset = off )$data
                    }
  
  # download dataset piecewise; with progress bar!
  df_l      <- Map( downld_dataset,
                    query_in$limit_v,
                    query_in$offset_v,
                    query_in$proj_id,
                    1:length(query_in$limit_v) )
  
  # put it all in one  together
  out_data <- Reduce( function(...) dplyr::bind_rows(...), df_l ) %>% 
                as.data.frame %>% 
                .[,-grep('count_table_key',names(.))]

  return(out_data)
  
}


#' @noRd
# informational message at every download
data_message <- function(x){
  
  if( length(unique(x$proj_metadata_key)) == 1)
    message(paste0("You have downloaded data from ",
                   length(unique(x$proj_metadata_key)),
                   " project.\nThe identification number of this project is:",
                   paste0(" ", 
                          unique(x$proj_metadata_key),
                          collapse=", "),
                   "."),
            "\n
To learn more about study design, use metadata_url()
To cite the study, use pplr_citation().\n")
  
  else {
    message("\n", paste0("You have downloaded data from ",
                         length(unique(x$proj_metadata_key)),
                         " projects. \nThe identification numbers of these projects are: ",
                         paste0(unique(x$proj_metadata_key),
                               collapse = ", "),
                         "."),
"\n
To learn more about study designs, use metadata_url()
To cite the study, use pplr_citation().\n")
  }
  
}
