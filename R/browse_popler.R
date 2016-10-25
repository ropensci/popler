#' Browse the popler database
#'
#' This function browses LTER studies contained in the popler database. The user can group, tally, and subset the data sets based on a number of factors (or columns) of that characterize datasets in the database.
#' @param group_factors A vector of characters that specifies the factors by which data should be grouped.
#' @param tally_by A vector of characters that specifies which fields to tally
#' @param criteria A logical expression that specifies which records to keep.
#' Synthax is the same as that used in the base function subset.
#' @param trim should strings be truncated at the 50th character? Default is TRUE.
#' @return A data frame reporting grouping factors and tallies, based on the subsetting criteria.
#' @export
#' @details
#' The browse_popler() function searches the popler database based on user-specified terms. 
#' 
#' Specified terms can be used to group the datasets (returning the unique values by group), tally the datasets (returning the number of datasets that match a combination of groups), or filter the database to only retain entries that match certain terms. 
#' Potential terms that can be used with the group_factors, tally_by, or criteria arguments are:
#' \itemize{
#'  \item{lterid: }{The code of the LTER site from which the data were collected (for LTER datasets)}
#'  \item{metarecordid: }{A unique numeric that corresponds with the study}
#'  \item{study: }{The name of the study}
#'  \item{metalink: }{A url that links to the study's metadata}
#'  \item{studytype: }{The type of study (either "obs" for observational or "exp" for experimental)}
#'  \item{community: }{The type of species data (either "yes" for a community of multiple species or "no" for a single-species population study)}
#'  \item{duration_years: }{The number of years over which the study was conducted (i.e., first year subtracted from the last, irregardless of the sampling interval)}
#'  \item{study_site: }{A string of characters that identifies a unique site}
#'  \item{data_type: }{Type type of species data collected. Options include "individual" for data that tracks each individual within a species;
#'  "count" for data that sums the number of individuals in a sampling area/time period, "per_cover" for percent cover data; "biomass" for biomass data,
#'  "density" for count data standardized by unit area; "presence-abs" for presence-absence data}
#'  \item{structured: }{The type of life stage data collected when data_type is "individual"; defaults to NA for other data types}
#'  \item{species: }{The species name in the database (group_factors will return both genus and species columns; but criteria will only search the species column)}
#'  \item{taxonomy: }{The entire taxonomy of species in the databasse (only to be used for group_factors)}
#'  \item{kingdom: }{The kingdom of species in the database}
#'  \item{phylum: }{The phlylum of species in the database}
#'  \item{clss: }{The class of species in the database}
#'  \item{ordr: }{The order of species in the database}
#'  \item{family: }{The family of species in the database}
#'  \item{genus: }{The genus of species in the database}
#' }
#' Terms can also be viewed by specifying browse_popler()
#' @examples
#' The group_factors argument can be used to view all possible values for a specified term. For example, the following allows users to view all species represented in the database:
#' browse_popler(group_factors="species")
#' 
#' Multiple terms can be specified for each argument. For example, the following returns the unique combinations of species by study:
#' browse_popler(group_factors=c("study", "species"))
#'
#' The tally_by argument allows users see the number of datasets that correspond with each grouping factors. For example, the following allows users to view how many studies have data for each species:
#' #returns the number of species species for each study
#' browse_popler(group_factors="species",tally_by="study")
#' 
#' Similarly, the following indicates how many species are represented in the database for each genus:
#' browse_popler(group_factors="genus",tally_by="species")
#' 
#' Using group_factors() and/or tally_by() allows users to see what values are included in each term (e.g., what species names are present).
#' By first running group_factors() to identify relevant values, users can then select datasets that include those values.
#' For example, the following returns the studies in which that contain the genus "Abietinaria"
#' browse_popler(group_factors="study",criteria=genus=="Abietinaria")

# The actual popler function=============================================================================
browse_popler <- function(group_factors=NULL,tally_by=NULL,criteria=NULL,trim=TRUE){

  # Load main (temporary) main data table
  x <- formatMainTable(popler:::dataPoplerFunction)

  #"Format" data
  x <- mutate(x, duration_years = studyendyr - studystartyr) #calculate study durations
  x$sp_rep1_ext <- 1

  # Case insensitive matching ("lower" everything)
  names(x) <- tolower(names(x))
  if(!is.null(group_factors)) group_factors=tolower(group_factors)
  if(!is.null(tally_by))  tally_by=tolower(tally_by)

  # Initial group_factors
  possibleargs <- tolower(c("lterid","proj_metadata_key","title","metalink","studytype","community","duration_years",
                            "study_site_key","datatype", "structured",
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
