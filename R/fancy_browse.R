#The main difference between library() and require() is what happens if a package isn’t found. While library() throws an error, require() prints a warning message and returns FALSE. In practice, this distinction isn’t important because when building a package you should NEVER use either inside a package. See package dependencies for what you should do instead.

## need to add number of taxa

fancy_browse=function(input, md_file="./browse.Rmd", html_file="./browse.html"){
  
  input <- rebrowse(input)
  
  # build the .Rmd file piecewise
  header <- c(
    '
<br>
<img src="icon_leaf.png" alt="Drawing" style="height: 110px; float: right"/><br>

# Metadata Summary  

***  

*Before publishing any data gathered from popler, please review and adhere to the [LTER Network Data Access Policy, Data Access Requirements, and General Data Use Agreement](https://lternet.edu/policies/data-access), as well as any additional requirements indicated by the authors of each study.*  

***  
<a name="contents"></a>  

### Table of Contents
* [Geographic overview of sites](#geo)  
* [Project list](#projects)  
* [Data type descriptions](#dat)  
* [References](#refs)  

```{r echo=FALSE, warning=FALSE, message=FALSE}
# required calculations
library(popler)
library(maps)
library(mapdata)
A <- browse(BROWSE_QUERY, full_tbl=T, trim=F)
NN <- nrow(A)
n_taxa <- rep(NA,NN)
for(i in 1:NN){
n_taxa[i] <- nrow(A$taxas[[i]])
}
```

'
  )

  # geographic overview of sites  
  geo <- c(
'
***  
<a name="geo"></a>  
    
### Geographic overview of sites
```{r echo=FALSE, warning=FALSE, message=FALSE}
suppressWarnings(map <- lter_maps(A))
```
<div style="text-align: right"> *[back to Table of Contents](#contents)* </div>  
  
'
  )
  
  # project list
  proj_list <- c(
'
***  
<a name="projects"></a>  

### Project list
`r paste0("<br>",1:NN,". [",A$title,"](#",1:NN,")", collapse="")`  

***  
'    
  )
  
  # project descriptions
  proj <- c(
'
`r N<-X`
<a name="`r N`"></a>  

#### `r N`. `r A$title[N]` <a name="`r N`"></a>  

##### LTER site overview  
* **Site name:** `r A$lter_name[N]` (`r A$lterid[N]`)  
* **lat/long:**  (`r A$lat_lter[N]`, `r A$lng_lter[N]`)  

##### Project overview  

```{r echo=FALSE}
su <- gsub("m2","m$^{2}$",A$samplingunits[N])
```

* **study years:** `r A$studystartyr[N]` - `r A$studyendyr[N]` (`r A$duration_years[N]` years total)  
* **data type:** `r A$datatype[N]` `r if(su!="NA"){paste0(" (",su,")")}`  
* **number of unique taxa:** `r n_taxa[N]`  
* **popler project ID:** `r A$proj_metadata_key[N]`  
* [**citation**](#c`r N`)  
* [**metadata link**](`r A$metalink[N]`)  

##### Study design information  

```{r echo=FALSE}
# get population structure information
st_p <- c(A$structured_type_1[N],A$structured_type_2[N],A$structured_type_3[N],A$structured_type_4[N])
un_p <- c(A$structured_type_1_units[N], A$structured_type_2_units[N], A$structured_type_3_units[N], A$structured_type_4_units[N])
st_p <- st_p[!st_p %in% "NA"]
un_p <- paste0("(",un_p[!un_p %in% "NA"],")")
un_p <- gsub("m2","m$^{2}$",un_p)
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
un_s <- gsub("m2","m$^{2}$",un_s)
rp_s[rp_s %in% NA]       <- ""
spat <- paste(la_s," (",ex_s," ",un_s," , _N_ = ",rp_s,")",sep="")
spat <- spat[!la_s == ""]
if(length(spat)==0){spat <- "none recorded"}
spat <- gsub("  , ","",spat)

# get treatment structure information
st_t <- c(A$treatment_type_1[N],A$treatment_type_2[N],A$treatment_type_3[N])
st_t <- st_t[!st_t %in% c("NA",NA)]
if(length(st_t)==0){st_t <- "none recorded"}

```
* **treatment(s):** `r paste0(paste(st_t),collapse=", ")`  
* **poplulation structure:** `r paste0(paste(st_p,un_p),collapse=", ")`  
* **sampling frequency:** `r gsub("yr","year",A$samplefreq[N])`  
* **spatial replication levels:**  `r spat`  

<div style="text-align: right"> *[back to Project list](#projects)* </div>

***  
'
  )

  # datatype descriptions
  dat <- c(
'
<a name="dat"></a>  

### Data type descriptions  

* **count:** An integer count of individuals.  
* **cover:** A measure of the area occupied by individuals. Cover can be recorded as any of the following:  
    + a *categotical variable;* for example, “1” if individuals cover between 0 and 5% of surface, “2” if they cover 5-20% of surface, etc.  
    + a *percentage;* for example, 0-100% of a sampled area is covered.  
    + a *count;* for example, in the case of a line transect with 40 observations, cover could be an integer from 0 to 40.  
* **biomass:** A measure of the biomass of sampled individuals.  
* **density:** A derived quantity obtained by dividing a count of individuals over a line, an area, or a volume.  
* **individual:** Each observation refers to an individual. This individual will be characterized by a measure of structure (see `structured_type`, and `structured_type_units`)  

<div style="text-align: right"> *[back to Project list](#projects)* </div>  

***  
'    
  )  
  
  # references  
  refs <- c(
'
<a name="refs"></a>  

### References
`r paste(paste0("<br><a name=c",1:NN,"></a>[",1:NN,".](#",1:NN,") ",format(popler_cite(A)$bibliography),collapse="<br>"))`  

<div style="text-align: right"> *[back to Project list](#projects)* </div>
'  
  )

  # change browse query  in header
  header <- gsub("BROWSE_QUERY",paste0(deparse(attributes(input)$search_expr),collapse=""),header)
  
  # update project block
  proj_new <- rep(NA,nrow(input))
  for(i in 1:nrow(input)){
    proj_new[i] <- gsub("`r N<-X",paste0("`r N<-",i),proj)
  }
  
  # make markdown file
  sink(md_file)
  cat(header, geo, proj_list, proj_new, dat, refs)
  sink()
  
  # launch browser window
  rmarkdown::render(md_file,quiet=T)
  browseURL(html_file)
}
