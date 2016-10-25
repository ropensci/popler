# Attempt to analyze the grasshopper dataset straight from popler!

# Data ---------------------------------------------------------------
# Select grasshopper data set 
grasshop <- tunnel_query(proj_metadata_key == 21)
# Open up metadata 
metadata_webpage(21)

#FIRST, sum up all individuals BY DATE
#This is actually an individual data set, so it should be treated as such
grasshop <- grasshop %>% group_by(year,day,month,genus,species,
                                  spatial_replication_level_1,
                                  spatial_replication_level_2,
                                  spatial_replication_level_3) %>%
              summarise( counts = sum(count_observation))

# Select most abundant species --------------------------------------
tmp = grasshop %>% 
        group_by(genus,species) %>% 
          summarise(abundance = sum(counts))
tmp = arrange(tmp,desc(abundance)) #most abundant species

# Select most abundant species
trpa = subset(grasshop, genus == "Trimerotropis" & species == "pallidipennis")


# Format data ----------------------------------------------------------

# Ignore summer months (ignore August through October (8, 9, 10)
trpa <- subset(trpa, month  < 8)

# Use BOGR only (most abundant site, by far)
trpa %>% group_by(spatial_replication_level_1) %>% summarise(totCounts = sum(counts))
trpa <- subset(trpa, spatial_replication_level_1 == "BOGR")

# Spatial structure
spat_str <- distinct(select(as.data.frame(trpa),spatial_replication_level_1,
                            spatial_replication_level_2,
                            spatial_replication_level_3))

#spatial structure plus years
str_years <- NULL
for(i in 1:length(unique(trpa$year))) {

  str_years <- rbind(str_years, 
                     cbind(unique(trpa$year)[i],spat_str))

}
names(str_years)[1] <- "year"

# To include all zeros, merge year by spatial structure to data 
trpa_all <- merge(str_years,trpa,all.x=T)
trpa_all$counts[is.na(trpa_all$counts)] <- 0
trpa_all$counts <- trpa_all$counts + 1

# Log abundance
trpa_all$logN <- log(trpa_all$counts)

#Check that all years have one observation per spatial replicate only
#tRep <- trpa_all %>% group_by(year, spatial_replication_level_1, 
#                  spatial_replication_level_2, spatial_replication_level_3) %>% 
#                      summarise( nObs = n())
#tRep$nObs

#N at time t0 and t1
#trpat0 <- select(as.data.frame(trpa_all),-month,-day)
#trpat1 <- select(as.data.frame(trpa_all),-month,-day)

#N columns
#trpat0 <- mutate(trpat0, logNt0 = logN, logN = NULL, counts = NULL)
#trpat1 <- mutate(trpat1, logNt1 = logN, logN = NULL, counts = NULL)

# format trpat1
#trpat1$year <- trpat1$year-1

# data to fit gompertz model
#trpa_gr <- merge(trpat0, trpat1)


#Z = matrix(c(rep(1,len),rep(0,len*4),
#         rep(0,len),rep(1,len),rep(0,len*3),
#         rep(0,len*2),rep(1,len),rep(0,len*2),
#         rep(0,len*3),rep(1,len),rep(0,len*1),
#         rep(0,len*4),rep(1,len)),len*5,5)
#dat_marss <- trpa_all[,c("year","spatial_replication_level_2","logN")]
#tidyr::spread(dat_marss,spatial_replication_level_2,logN)

######################################################
# MARSS EXAMPLES --------------------------------------
######################################################


library(MARSS)

# Simple MARSS model ----------------------------------------------------------- 
data(graywhales)
years = graywhales[,1]
loggraywhales = log(graywhales[,2])
kem = MARSS(loggraywhales)

# Identify spatial structure -----------------------------------------------------------
dat = t(log(harborSeal[,4:7]))
harbor1 = MARSS(dat, model = list(Z = factor(rep(1, 4))))
harbor2 = MARSS(dat, model = list(Z = factor(1:4)))

harbor1$AICc
harbor2$AICc

# Multispecies interactions ---------------------------------------------------------
plank.dat = t(log(ivesDataByWeek[,c("Large Phyto","Small Phyto","Daphnia","Non-daphnia")]))
d.plank.dat = (plank.dat - apply(plank.dat, 1,mean, na.rm = TRUE))

Z = factor(rownames(d.plank.dat))
U = "zero"
A = "zero"
B = "unconstrained"
Q = matrix(list(0), 4, 4)
diag(Q) = c("Phyto", "Phyto", "Zoo", "Zoo")
R = diag(c(0.04, 0.04, 0.16, 0.16))
plank.model = list(Z = Z, U = U, Q = Q, R = R, B = B, A = A, tinitx=1)

kem.plank = MARSS(d.plank.dat, model = plank.model)
B.est = matrix(kem.plank$par$B, 4, 4)
rownames(B.est) = colnames(B.est) = c("LP", "SP", "D", "ND")
print(B.est)

# Examples in 
dat = t(harborSealWA)
dat = dat[2:4,] #remove the year row
#fit a model with 1 hidden state and 3 observation time series
kemfit = MARSS(dat, model=list(Z=matrix(1,3,1),
                               R="diagonal and equal"))
kemfit$model #This gives a description of the model
print(kemfit$model) # same as kemfit$model
summary(kemfit$model) #This shows the model structure


#add CIs to a marssMLE object
#default uses an estimated Hessian matrix
kem.with.hess.CIs = MARSSparamCIs(kemfit)
kem.with.hess.CIs #print with se's and CIs
#fit a model with 3 hidden states (default)
kemfit = MARSS(dat, silent=TRUE) #suppress printing
kemfit #print information on the marssMLE object
#fit a model with 3 correlated hidden states
# with one variance and one covariance
#maxit set low to speed up example, but more iterations are need for convergence
kemfit = MARSS(dat, model=list(Q="equalvarcov"), control=list(maxit=50))
# use Q="unconstrained" to allow different variances and covariances
#fit a model with 3 independent hidden states
#where each observation time series is independent
#the hidden trajectories 2-3 share their U parameter
kemfit = MARSS(dat, model=list(U=matrix(c("N","S","S"),3,1)))
#same model, but with fixed independent observation errors
#and the 3rd x processes are forced to have a U=0
#Notice how a list matrix is used to combine fixed and estimated elements
#all parameters can be specified in this way using list matrices
kemfit = MARSS(dat, model=list(U=matrix(list("N","N",0),3,1), R=diag(0.01,3)))
#fit a model with 2 hidden states (north and south)
#where observation time series 1-2 are north and 3 is south
#Make the hidden state process independent with same process var
#Make the observation errors different but independent
#Make the growth parameters (U) the same
#Create a Z matrix as a design matrix that assigns the "N" state to the first 2 rows of dat
#and the "S" state to the 3rd row of data
Z=matrix(c(1,1,0,0,0,1),3,2)
#You can use factor is a shortcut making the above design matrix for Z
#Z=factor(c("N","N","S"))
#name the state vectors
colnames(Z)=c("N","S")
kemfit = MARSS(dat, model=list(Z=Z,
                               Q="diagonal and equal",R="diagonal and unequal",U="equal"))
#print the model followed by the marssMLE object
kemfit$model
kemfit
## Not run:
#simulate some new data from our fitted model
sim.data=MARSSsimulate(kemfit, nsim=10, tSteps=10)
#Compute bootstrap AIC for the model; this takes a long, long time
kemfit.with.AICb = MARSSaic(kemfit, output = "AICbp")
kemfit.with.AICb
## End(Not run)
## Not run:
#Many more short examples can be found in the
#Quick Examples chapter in the User Guide
RShowDoc("UserGuide",package="MARSS")
#You can find the R scripts from the chapters by
#going to the index page
RShowDoc("index",package="MARSS")
## End(Not run)
