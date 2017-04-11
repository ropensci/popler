#W <- browse(community=="no" & datatype=="count", full_tbl=T, trim=F)

fancy_browse=function(input){
  
  p0 <- c(
    '
---
title: 
author: 
output: html_document
---
<br>
<img src="icon_leaf.png" alt="Drawing" style="height: 110px; float: right"/><br>

# Metadata Summary  

***  

*Before publishing any data gathered from popler, please review and adhere to the [LTER Network Data Access Policy, Data Access Requirements, and General Data Use Agreement](https://lternet.edu/policies/data-access), as well as any additional requirements indicated by the authors of each study.*  

***  
<a name="contents"></a>  

### Table of Contents
[Geographic overview](#geo)  
[Project list](#projects)  
[References](#refs)  

***  
<a name="geo"></a>  

### Geographic overview of sites
'
  )
  
  p1 <- c(
    '
```{r echo=FALSE, warning=FALSE, message=FALSE}
library(popler)
library(maps)
library(mapdata)
A <- browse(BROWSE_QUERY, full_tbl=T, trim=F)
NN <- nrow(A)
map("worldHires","usa", xlim=c(-125,-66.6),ylim=c(24.5,49.9), col="grey90", fill=TRUE, boundary=F)
points(A$lng_lter, A$lat_lter, pch=19, col="dodgerblue", cex=1.2)
```  

<div style="text-align: right"> *[back to Table of Contents](#contents)* </div>  
  
***  
<a name="projects"></a>  
    
### Project list
`r paste0("<br>",1:NN,". [",A$title,"](#",1:NN,")", collapse="")`  
  
***  
'
  )
  
  p2 <- c(
    '
`r N<-X`
<a name="`r N`"></a>  

#### `r N`. `r A$title[N]` <a name="`r N`"></a>  
*[metadata link](`r A$metalink[N]`)*  

##### LTER site overview  
* **Site name:** `r A$lter_name[N]` (`r A$lterid[N]`)  
* **lat/long:**  (`r A$lat_lter[N]`, `r A$lng_lter[N]`)  

##### Project overview  

* **study years:** `r A$studystartyr[N]` - `r A$studyendyr[N]` (`r A$duration_years[N]` years total)  
* **data type:** `r A$datatype[N]`  
* **community data?:** `r A$community[N]`  
* **is data derived?:** `r A$derived[N]`
* **popler project ID:** `r A$proj_metadata_key[N]`  
* [**citation**](#c`r N`)  

##### Study design information  

```{r echo=FALSE}
# get population structure information
st_p <- c(A$structured_type_1[N],A$structured_type_2[N],A$structured_type_3[N],A$structured_type_4[N])
un_p <- c(A$structured_type_1_units[N], A$structured_type_2_units[N], A$structured_type_3_units[N], A$structured_type_4_units[N])
st_p <- st_p[!st_p %in% "NA"]
un_p <- paste0("(",un_p[!un_p %in% "NA"],")")
if(length(st_p)==0){st_p <- "none recorded"; un_p <- ""}

# get spatial structure information
la_s <- c(A$spatial_replication_level_1_label[N],A$spatial_replication_level_2_label[N],A$spatial_replication_level_3_label[N],A$spatial_replication_level_4_label[N],A$spatial_replication_level_5_label[N])

ex_s <- c(A$spatial_replication_level_1_extent[N],A$spatial_replication_level_2_extent[N],A$spatial_replication_level_3_extent[N],A$spatial_replication_level_4_extent[N],A$spatial_replication_level_5_extent[N])

un_s <- c(A$spatial_replication_level_1_extent_units[N],A$spatial_replication_level_2_extent_units[N],A$spatial_replication_level_3_extent_units[N],A$spatial_replication_level_4_extent_units[N],A$spatial_replication_level_5_extent_units[N])

rp_s <- c(A$spatial_replication_level_1_number_of_unique_reps[N],A$spatial_replication_level_2_number_of_unique_reps[N],A$spatial_replication_level_3_number_of_unique_reps[N],A$spatial_replication_level_4_number_of_unique_reps[N],A$spatial_replication_level_5_number_of_unique_reps[N])

ex_s <- as.character(ex_s)
rp_s <- as.character(rp_s)
la_s[la_s %in% "NA"]     <- ""
ex_s[ex_s %in% "-99999"] <- ""
un_s[un_s %in% "NA"]     <- ""
rp_s[rp_s %in% NA]       <- ""
spat <- paste(la_s," (",ex_s," ",un_s," , _N_ = ",rp_s,")",sep="")
spat <- spat[!la_s == ""]
if(length(spat)==0){spat <- "none recorded"}
spat <- gsub("  , ","",spat)

# get treatment structure information
st_t <- c(A$treatment_type_1[N],A$treatment_type_2[N],A$treatment_type_3[N])
st_t <- st_t[!st_t %in% c("NA",NA)]
if(length(st_t)==0){st_t <- "none recorded"}

# get control information
st_c <- c(A$control_group)
st_c <- st_c[!st_C %in% c("NA",NA)]
if(length(st_c)==0){st_c <- "none recorded"}
```
* **treatment(s):** `r paste0(paste(st_t),collapse=", ")`  
* **control:** `r st_c`  
* **poplulation structure:** `r paste0(paste(st_p,un_p),collapse=", ")`  
* **sampling frequency:** `r A$samplefreq[N]`  
* **spatial replication levels:**  `r spat`  
* **total spatial replication:**  `r A$tot_spat_rep[N]`  

<div style="text-align: right"> *[back to Project list](#projects)* </div>

***  
'
  )
  
  p3 <- c(
    '
<a name="refs"></a>  

### References
`r paste(paste0("<br><a name=c",1:NN,"></a>[",1:NN,".](#",1:NN,") ",format(popler_cite(A)$bibliography),collapse="<br>"))`  

<div style="text-align: right"> *[back to Project list](#projects)* </div>
'  
  )
  
  p1_new <- gsub("BROWSE_QUERY",deparse(attributes(input)$search_argument),p1)
  
  p2_new <- rep(NA,nrow(input))
  for(i in 1:nrow(input)){
    p2_new[i] <- gsub("`r N<-X",paste0("`r N<-",i),p2)
  }
  
  sink("test.Rmd")
  cat(p0,p1_new,p2_new,p3)
  sink()
  rmarkdown::render(file.path(path.to.file, paste0(file.name, ".Rmd")),quiet=T)
  browseURL( file.path(path.to.file, paste0(file.name, ".html")))
}
