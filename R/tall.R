#' Tally records from the popler main table
#'
#' Service function for the popler() function
#' @param The popler main table
#' @param A logical expression that specifies which records to keep.
#' @return an error message, if one of the vector elements is misspelled
#' @export


#Calculate tallies
tall=function(subsetDat,tally_by,columnNames,trim){

  dfList=list()
  for(i in 1:length(tally_by)){

    #does 'tally_by' refers to multiple columns
    multiTally=multipleColumns(tally_by[i])

    #columns referring to both group_factors and tallies
    argTallyCol=c(columnNames,multiTally)

    #Data of interest for the tally
    tallyData=select(subsetDat,one_of(argTallyCol))

    #lower case of grouping factors (should get rid of doubles such as "yes" "Yes")
    tallyData=as.data.frame(tallyData)
    for(co in 1:ncol(tallyData)) { tallyData[,co]=tolower(tallyData[,co])}
    tallyData=as.tbl(tallyData)

    #only unique values
    tallyData=distinct(tallyData)

    #store tallies
    tallyName=paste0(tally_by[i],"_tally") #name of column containing tallies
    if(!is.null(columnNames)){ #If you have a GROUPING FACTOR
      dfList[[i]] <- tallyData %>%
        group_by_(.dots=columnNames) %>%
        summarise_(.dots=setNames(list(~n()),tallyName))
    } else {#IF you NO GROUPING FACTOR
      dfList[[i]] <- tallyData %>% summarise_(.dots=setNames(list(~n()),tallyName))
    }

  }
  out=trimDisplay(Reduce(function(...) merge(...),dfList),trim)
  return(out)

}
