#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of the factors (or columns) of the database.
#' @param group_by A vector of characters that specifies the factors by which data should be grouped.
#' @param tally_by A vector of characters that specifies which fields to tally
#' @param criteria A logical expression that specifies which records to keep.
#' Synthax is the same as that used in the base function subset.
#' @param trim should strings be truncated at the 50th character? Default is TRUE.
#' @return A data frame reporting groping factors and tallies, based on the subsetting criteria.
#' @export
#' @examples
#' #returns all of the species included in the data set
#' popler(group_by="species")
#'
#' #returns the number of species species for each study
#' popler(group_by="study",tally_by="species")
#'
#' #returns the studies in which that contain the genus "Abietinaria"
#' popler(group_by="study",criteria=genus=="Abietinaria")

# The actual popler function=============================================================================
browse_popler <- function(group_factors=NULL,tally_by=NULL,criteria=NULL,trim=TRUE){

  # Load main (temporary) main data table
  x <- formatMainTable(poplerTwin:::dataPoplerFunction)

  #"Format" data
  x <- mutate(x, duration_years = studyendyr - studystartyr) #calculate study durations
  x$sp_rep1_ext <- 1

  # Case insensitive matching ("lower" everything)
  names(x) <- tolower(names(x))
  if(!is.null(group_factors)) group_factors=tolower(group_factors)
  if(!is.null(tally_by))  tally_by=tolower(tally_by)

  # Initial group_factors
  possibleargs <- tolower(c("lterid","metarecordid","study","metalink","studytype","community","duration_years",
                            "study_site","data_type", "structured",
                            "species","taxonomy","kingdom","phylum","clss","ordr","family","genus"))

  # ERRORS/MESSAGES------------------------------------------------------------------------------

  # group_factors: i) spelling mistakes or ii) information response after "?"
  errorsGroup(group_factors,possibleargs)

  # tally.b: spelling mistakes
  errorsTally(tally_by,possibleargs)

  # print informational message (is all arguments are null)
  infoMessage(group_factors,tally_by,substitute(criteria),possibleargs)

  # SELECT and SUMMARIZE-------------------------------------------------------------------------

  # Identify which columns to work on
  columnNames <- multipleColumns(group_factors)

  # Filter based on criteria (if criteria provided)
  #subsetDat=selectByCriteria(x,criteria)
  subsetDat <- selectByCriteria(x,substitute(criteria))

  # tally cases, if tally_by is not NULL
  if(!is.null(tally_by)){
    out <- tall(subsetDat,tally_by,columnNames,trim)
  } else { # if is.null(tally_by), provide unique values for the argument(s)
    out <- uniqueValues(subsetDat,columnNames,trim)
  }
  return(out) # return output

}
