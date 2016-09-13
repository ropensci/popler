#' Informational message if no information is entered
#'
#' Service function for the popler() function
#' @param group_factors factors
#' @param possible arguments that popler() can work with (internally defined)
#' @return error messages, potentially
#' @export


# Errors linkes to group_factors
errorsGroup=function(group_factors,possibleargs){

  description=c("lter name","project_id","title of project","experimental or observational study?",
                "does data set contain multiple taxa?","duration of project",
                "type of data (count,biomass,cover,density,individual)",
                "is indidivual data structured?",
                "names of site(higher level spatial replicate)",
                "genus and species","whole taxonomy of each species",
                "kingdom","phylum","class","order","family","genus")

  #Check for spelling mistakes
  if(!is.null(group_factors)){
    if( !all( is.element(group_factors,possibleargs) ) ) {

        opt <- options(error=NULL)
        on.exit(opt)
        stop(paste0("Error: the following 'argument' entry was mispelled: '",
                    setdiff(group_factors,possibleargs),"' "))
    }
  }

}
