#' Error message regarding the group_factors arguments
#'
#' Service function for the popler() function
#' @param group_factors factors
#' @param possibleargs possible arguments
#' @param
#' @param
#' @return error messages, potentially
#' @export

# Errors linkes to group_factors
infoMessage=function(group_factors,tally_by,criteria,possibleargs){

  description=c("lter name","project_id","title of project","url to metadata","experimental or observational study?",
                "does data set contain multiple taxa?","duration of project",
                "type of data (count,biomass,cover,density,individual)",
                "is indidivual data structured?",
                "genus and species","whole taxonomy of each species",
                "kingdom","phylum","class","order","family","genus")

  if(is.null(group_factors) & is.null(tally_by) & is.null(criteria)) {

    print(data.frame(possible_group_factors=possibleargs,
                     description=description))
    opt <- options(show.error.messages=FALSE,error=NULL)
    on.exit(options(opt))
    stop()

  }

}
