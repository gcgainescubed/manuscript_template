source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

### Some definitions for FIADB UG:
# Estimation unit (ESTN_UNIT): "...the specific geographic area that is stratified. Estimation units are often determined by a combination of geographical boundaries, sampling intensity and ownership."
# Stratum (STRATUMCD, STRATUM_DESCR): "A stratum is a non-overlapping subdivision of the population. Each plot is assigned to one and only one stratum; the relative sizes of strata are used to compute strata weights. Strata are usually based on land use (e.g., forest or nonforest) but may also be based on other criteria (e.g., ownership, crown cover)."
# Evaluation (EVALID): "The unique identifier that represents the population used to produce a type of estimate. The EVALID is generally a concatenation of a 2-digit State code, a 2-digit year code, and a 2-digit evaluation type code.
# Expansion factor (EXPNS): "The area, in acres, that a stratum represents divided by the number of sampled plots in that stratum: EXPNS = (POP_ESTN_UNIT.AREA_USED*P1POINTCNT / POP_ESTN_UNIT.P1PNTCNT_EU) / P2POINTCNT. This attribute can be used to obtain estimates of population area when summed across all the plots in the population of interest."
# Adjustment factor (ADJ_FACTOR_*): "A value that adjusts population estimates to account for partially nonsampled plots (* = MICR, SUBP, or MACR) due to hazardous conditions or denied access. Used with COND.CONDPROP_UNADJ and EXPNS for area estimates; used with EXPNS and TREE.TPA_UNADJ for tree estimates."

# Specify an Evaluation (EVALID)
#my_evalids <- c(232401) # This is the evaluation for [Maine, 2020-2024, Sampled plots used for current area and condition-level estimates.])
my_evalids <- c(302201) # This is the evaluation for [Montana, 2013-2022, Sampled plots used for current area and condition-level estimates.])

# Specify a Research Station (RSCD)
#my_stations <- c(24) # 24 is NERS, appropriate Research Station for Maine (Does not matter for FIADB because "we don't publish NFS crossover plots")

### 1. Read Population Stratum (POP_STRATUM) table for EVALIDs and RSCDs of interest 
# Specify POP_STRATUM query
# pop_stratum_query <- paste0("SELECT * FROM POP_STRATUM WHERE ",
#                             "EVALID IN (",paste(unique(my_evalids),collapse=","),") AND ",
#                             "RSCD IN (",paste(unique(my_stations),collapse=","),") ")

# Or if you only care about the EVALID and not RSCD
pop_stratum_query <- paste0("SELECT * FROM POP_STRATUM WHERE ",
                            "EVALID IN (",paste(unique(my_evalids),collapse=","),") ")

# connect to FIA sql database
db_name <- "SQLite_FIADB_ENTIRE.db"
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))

# print db version
dbver <- dbReadTable(con,'REF_FIADB_VERSION')
version_info <- dbver[nrow(dbver),c("VERSION","CREATED_DATE")]
version_info 

# get plot data for our survey units
pop_stratum <- dbGetQuery(con,pop_stratum_query)

# Disconnect from FIA database
dbDisconnect(con)

pop_stratum <- pop_stratum[,c("CN","ESTN_UNIT_CN","RSCD","EVALID","ESTN_UNIT",
                              "STRATUMCD","STRATUM_DESCR","STATECD","P1POINTCNT",
                              "P2POINTCNT","EXPNS","ADJ_FACTOR_MICR","ADJ_FACTOR_SUBP","ADJ_FACTOR_MACR")]
colnames(pop_stratum)[colnames(pop_stratum) == "CN"] <- "POP_STRATUM.CN"
head(pop_stratum) # CN = unique population stratum record

unique_combinations <- unique(pop_stratum[c("STRATUMCD", "ESTN_UNIT")])
unique_combinations <- unique_combinations[order(unique_combinations$ESTN_UNIT),]
dim(pop_stratum);dim(unique_combinations) # So POP_STRATUM for particular EVALID has as many rows as there are unique combinations of Stratum and Estimation Unit 

### 2. Read Population Plot Stratum Assignment (POP_PLOT_STRATUM_ASSGN) table (why?)
# Specify POP_PLOT_STRATUM_ASSGN query
pop_plot_stratum_query <- paste0("SELECT * FROM POP_PLOT_STRATUM_ASSGN WHERE ",
                                 "STRATUM_CN IN (",paste(unique(pop_stratum$POP_STRATUM.CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
pop_plot_stratum <- dbGetQuery(con,pop_plot_stratum_query)
dbDisconnect(con)

dim(pop_plot_stratum) # This is the number of plot measurements assigned to these unique strata

pop_plot_stratum <- pop_plot_stratum[,c("CN","STRATUM_CN","PLT_CN")] # We only need this to link strata to plot measurements
colnames(pop_plot_stratum)[colnames(pop_plot_stratum) == "CN"] <- "POP_PLOT_STRATUM_ASSGN.CN"
head(pop_plot_stratum)

# merge pop_stratum with pop_plot_stratum_assignment 
stratum_plt_assgn <- merge(pop_stratum, pop_plot_stratum, by.x = "POP_STRATUM.CN", by.y = "STRATUM_CN")
head(stratum_plt_assgn)
dim(stratum_plt_assgn) #same dims

### 3. Read Plot (PLOT) table
# Specify PLOT query
plot_query <- paste0("SELECT * FROM PLOT WHERE ",
                     "CN IN (",paste(unique(stratum_plt_assgn$PLT_CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
plots <- dbGetQuery(con,plot_query)
dbDisconnect(con)

plots <- plots[,c("CN","COUNTYCD","UNITCD","PLOT","INVYR","MEASYEAR","KINDCD","PLOT_STATUS_CD",
                  "REMPER","DESIGNCD","P2PANEL","INTENSITY","MACRO_BREAKPOINT_DIA")]
head(plots)
dim(plots)

# Merge plots onto stratum info
stratum_plots <- merge(plots,stratum_plt_assgn, by.x = "CN", by.y = "PLT_CN")

head(stratum_plots)
dim(stratum_plots);length(unique(stratum_plots$CN)) # Still same dims; "CN" now references a unique plot remeasurement

### 4. Read Condition (COND) table
cond_query <- paste0("SELECT * FROM COND WHERE ",
                     "PLT_CN IN (",paste(unique(stratum_plots$CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
cond <- dbGetQuery(con,cond_query)
dbDisconnect(con)

head(cond);dim(cond) # 4658 unique conditions on these plots

colnames(cond)[colnames(cond) == "CN"] <- "COND.CN"

# Keep only sampled accessible forest land for forest land area estimation?
cond <- cond[ (cond$COND_STATUS_CD%in%c(1,2,3,4,5)),
              c("COND.CN","PLT_CN","CONDID","CONDPROP_UNADJ","PROP_BASIS",
                "MICRPROP_UNADJ","SUBPPROP_UNADJ","MACRPROP_UNADJ",
                "COND_STATUS_CD")]

cond$MACRPROP_UNADJ <- as.numeric(cond$MACRPROP_UNADJ)
cond$MACRPROP_UNADJ <- ifelse(is.na(cond$MACRPROP_UNADJ),0,
                                 fiacond$MACRPROP_UNADJ) # MACRPROP_UNADJ is NA if a macroplot was never installed
table(cond$COND_STATUS_CD)

plots_cond <- merge(stratum_plots,cond, by.x = "CN", by.y = "PLT_CN")

head(plots_cond);dim(plots_cond) # now this has grown by 105 rows (when only keeping COND_STATUS_CD = 1)?
length(unique(stratum_plots$CN)) # I think this and the next line should be equivalent because we don't want to eliminate 0 plots?
length(unique(plots_cond$CN))

table(plots_cond$COND_STATUS_CD)
table(plots_cond$CONDID)

length(unique(plots_cond$CN[plots_cond$COND_STATUS_CD==1]))

head(plots_cond)
sum(plots_cond$CONDPROP_UNADJ==0)

summary(plots_cond$CONDPROP_UNADJ)
summary(plots_cond$ADJ_FACTOR_MACR) # "the inverse of the mean proportion of the sample [macro] plot areas that were within the sampled population." Pop Est User Guide p 3-1 
summary(plots_cond$ADJ_FACTOR_SUBP) # "the inverse of the mean proportion of the sample [sub] plot areas that were within the sampled population." Pop Est User Guide p 3-1

# The two ADJ_FACTOR_'s above are, I believe, 1/p_bar_h (i.e., the inverse of eq 17 in GBII)
# That's why you're multiplying COND_PROP_UNADJ by these below, instead of dividing

plots_cond$ESTIMATED_VALUE_AREA <- 0
for(i in 1:nrow(plots_cond)){
  PROP_BASIS <- plots_cond$PROP_BASIS[i] 
  # Get ESTIMATED_VALUE for area attribute
  if(plots_cond$COND_STATUS_CD[i]!=1){plots_cond$ESTIMATED_VALUE_AREA[i] <- 0}
  else if(PROP_BASIS=="MACR"){plots_cond$ESTIMATED_VALUE_AREA[i] <- plots_cond$CONDPROP_UNADJ[i] * plots_cond$ADJ_FACTOR_MACR[i]}
  else {plots_cond$ESTIMATED_VALUE_AREA[i] <- plots_cond$CONDPROP_UNADJ[i] * plots_cond$ADJ_FACTOR_SUBP[i]}
}

ESTIMATED_AREA <- sum(plots_cond$ESTIMATED_VALUE_AREA * plots_cond$EXPNS)

# Read population estimation unit (POP_ESTN_UNIT) table
# I guess I only need this to get total population area (A_T)? Or...?
# Specify POP_ESTN_UNIT query
pop_estn_unit_query <- paste0("SELECT * FROM POP_ESTN_UNIT WHERE ",
                              "EVALID IN (",paste(unique(plots_cond$EVALID),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
pop_estn_unit <- dbGetQuery(con,pop_estn_unit_query)
dbDisconnect(con)

pop_estn_unit <- pop_estn_unit[,c("CN","EVAL_CN","EVALID","ESTN_UNIT","ESTN_UNIT_DESCR",
                                  "AREA_USED","P1PNTCNT_EU")]
head(pop_estn_unit)
dim(pop_estn_unit)


# create a new version of tree plots in case something gets messed up
plots_cond2 <- plots_cond
# make p-adjusted condition proportions zero (not non-zero or NA) for any NON-accessible forest land conditions 
# (COND_STATUS_CD%in%1,2,3,4)? This was already done above, but I'll do it anyway:
plots_cond2$ESTIMATED_VALUE_AREA <- ifelse(plots_cond2$COND_STATUS_CD==1,plots_cond2$ESTIMATED_VALUE_AREA,0)
# sum p-adjusted condition proportions to plot level (keep some stratum and estn unit stuff)
# And I guess this is what makes these 
for_area_plots <- aggregate(plots_cond2$ESTIMATED_VALUE_AREA,
                             by=list(CN=plots_cond2$CN,
                                     EXPNS=plots_cond2$EXPNS,
                                     ESTN_UNIT=plots_cond2$ESTN_UNIT,
                                     STRATUMCD=plots_cond2$STRATUMCD),
                             sum)
dim(for_area_plots) # should be 3498 plots
head(for_area_plots)
colnames(for_area_plots)[ncol(for_area_plots)] <- "COND_PROP_plot" # I believe the correct interpretation of this is "total plot proportion of all measured, accessible forest conditions"?
dim(for_area_plots[for_area_plots$COND_PROP_plot>0,]) # should be 3117 non-zero plots?

# get within stratum standard errors [GB2 eq 4 on page 8]
v_Yhd <- aggregate(for_area_plots$COND_PROP_plot,
                   by=list(ESTN_UNIT=for_area_plots$ESTN_UNIT,
                           STRATUMCD=for_area_plots$STRATUMCD),
                   FUN=function(z){var(z)/length(z)}) 
# note: var includes /(n-1), /n added via /length(z)

colnames(v_Yhd)[ncol(v_Yhd)] <- "COND_PROP_eu_strat_se"

# add the stratum point/pixel count stuff to the latter
v_Yhd_plus_area <- merge(v_Yhd,pop_stratum)

# copy the list of estimation units for building estn unit level variances 
pop_estn_unit_area <- pop_estn_unit
pop_estn_unit_area$var_cond_prop <- 0
# loop through the estn units
for (eu in unique(pop_estn_unit_area$ESTN_UNIT)){
  # pull all strata in this estn unit
  strata_in_unit <- v_Yhd_plus_area[v_Yhd_plus_area$ESTN_UNIT==eu,]
  # get the W_h weights for each strata within the estimation unit
  strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
  # get the total p2 sample size in this estimation unit
  n <- sum(strata_in_unit$P2POINTCNT)
  # implement GB2 equation 3 page 8 in two parts for this estimation unit
  part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*COND_PROP_eu_strat_se))
  part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*COND_PROP_eu_strat_se)/n)
  # stick the result on the  copied list of estimation units
  pop_estn_unit_area$var_cond_prop[pop_estn_unit_area$ESTN_UNIT==eu] <- (part1 + part2)/n
}

# combine the estimation unit level variances together, using the area variable
area_var <- sum(pop_estn_unit_area$var_cond_prop*pop_estn_unit_area$AREA_USED^2)
se <- sqrt(area_var)

se / ESTIMATED_AREA * 100 # Should be 0.568 for Montana 302201

# Save ESTIMATED_AREA, for_area_plots, v_Yhd_plus_area, pop_estn_unit_2 for Maine 232401
#save(pop_estn_unit_area,ESTIMATED_AREA, for_area_plots,v_Yhd_plus_area,file=file.path("data","total_area_me.Rdata"))
# Last saved 7/16/2026

# Save ESTIMATED_AREA, for_area_plots, v_Yhd_plus_area, pop_estn_unit_2 for Montana 302201
#save(pop_estn_unit_area,ESTIMATED_AREA, for_area_plots,v_Yhd_plus_area,file=file.path("data","total_area_mt.Rdata"))
# Last saved 7/16/2026
