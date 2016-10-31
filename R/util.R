
# Formats the main table of browse_popler()
formatMainTable=function(x){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[,i]=as.character(x[,i])
  }
  return(x)
  
}


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

# Errors in tally_by
errorsTally=function(tally_by,possibleargs){
  
  if(!is.null(tally_by)) {
    if( !all( is.element(tally_by,possibleargs) ) ){
      stop(paste0("Error: the following 'tally_by' entry was mispelled: '",
                  setdiff(tally_by,possibleargs),"' "))
    }
  }
  
}

# Errors linkes to group_factors
infoMessage=function(group_factors,tally_by,criteria,possibleargs){
  
  description=c("lter name","project_id","title of project","url to metadata","experimental or observational study?",
                "does data set contain multiple taxa?","duration of project",
                "name of experimental sites","type of data (count,biomass,cover,density,individual)",
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


# function converts entries to multiple columns - if need be
multipleColumns=function(x) {
  
  taxonomy            <-  c("kingdom","phylum","clss","ordr","family","genus","species")
  species             <-  c("genus","species")
  spatialLevels       <-  c("sp_rep1_ext","sp_rep2_ext","sp_rep3_ext","sp_rep4_ext")
  wrapperNames        <-  c("taxonomy","species","spatialLevels") #list of multiple columns entries (the above threee)
  
  if( any(x %in% wrapperNames) ){
    oldIds   <- which(x %in% wrapperNames)
    newIds   <- which(wrapperNames %in% x)
    eval(parse(n=1,text=paste0("newArg=",wrapperNames[newIds])))
    columnNames=c(x[-oldIds],newArg)
    return(columnNames)
  } else(return(x))
}


# Summarizing function
selectByCriteria=function(x,criteria){
  
  if(!is.null(criteria)) {
    r <- which(eval(criteria, x, parent.frame()))
    subsetDat=tbl_df(x[r,,drop=F]) #tbl_df() to make object "work" with dplyr functions
  } else { subsetDat=tbl_df(x) }
  return(subsetDat)
  
}


# trim the display of character values. Mostly for project "titles"
trimDisplay=function(x,trim){
  
  if(trim==T){
    tmp=as.data.frame(x)
    for(i in 1:ncol(tmp)){
      if(is.character(tmp[,i])){ tmp[,i]=strtrim(tmp[,i],50) }
    }
    tmp=as.tbl(tmp)
    return(tmp)
  } else{
    return(x)
  }
}


# Calculate tallies
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


# Unique values for arguments
uniqueValues=function(subsetDat,columnNames,trim){
  if(!is.null(columnNames)) out=popler::trimDisplay(distinct(select(subsetDat,one_of(columnNames))),trim)
  if(is.null(columnNames) ) out=popler::trimDisplay(distinct(subsetDat),trim)
  return(out)
}
