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
my_evalids <- c(232401) # This is the evaluation for [Maine, 2020-2024, Sampled plots used for current area and condition-level estimates.])
#my_evalids <- c(302201) # This is the evaluation for [Montana, 2013-2022, Sampled plots used for current area and condition-level estimates.])

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

plots <- plots[,c("CN","STATECD","COUNTYCD","UNITCD","PLOT","INVYR","MEASYEAR","KINDCD","PLOT_STATUS_CD",
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

# Keep only sampled AF -- OR -- unsampled AF, but only unsampled because of hazardous or denied access.
# cond <- cond[ (cond$COND_STATUS_CD==1) |
#                       (cond$COND_STATUS_CD==5 &
#                          cond$COND_NONSAMPLE_REASN_CD %in% c(2,3)),
#                     c("COND.CN","PLT_CN","CONDID","CONDPROP_UNADJ",
#                       "MICRPROP_UNADJ","SUBPPROP_UNADJ","MACRPROP_UNADJ",
#                       "COND_STATUS_CD")]

# I tried it this way first, because the SQL query doesn't keep non-sampled conditions;
# However, to reproduce stratum adjustment factors (and, to modify them as we 
# start to perturb panel assignments later in this project), we might need them as in the above?
#cond <- cond[ #(cond$COND_STATUS_CD==1) # commented this out to avoid eliminating 0 plots...?
              # ,
              # c("COND.CN","PLT_CN","CONDID","CONDPROP_UNADJ",
              #   "MICRPROP_UNADJ","SUBPPROP_UNADJ","MACRPROP_UNADJ",
              #   "COND_STATUS_CD")]

cond <- cond[ (cond$COND_STATUS_CD%in%c(1,2,3,4,5)) # Keeping all condition status cds to avoid eliminating 0 plots...?
              ,
              c("COND.CN","PLT_CN","CONDID","CONDPROP_UNADJ",
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

### 5. Read Tree (TREE) table 
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
tree_query <- paste0("SELECT * FROM TREE WHERE ",
                     "STATUSCD IN (1) AND ", # only live trees
                     "PLT_CN IN (",paste(unique(plots_cond$CN),collapse=","),")")
tree <- dbGetQuery(con,tree_query)
dbDisconnect(con)

head(tree);dim(tree)

colnames(tree)[colnames(tree) == "CN"] <- "TREE.CN"

# subset to trees above 1" (DIA is NA for removed trees? for woodland species?) Why would there be DIA<1" in TREE?
tree <- tree[!is.na(tree$DIA) &
             #tree$DIA>=1 &
             #tree$DIA>=5.0 & # This is what the SQL query says I think?
             #!is.na(tree$TPA_UNADJ) & # This is what the SQL query says I think? NA if DIA < 5.0"...commenting it out for now, and assigning these 0 VOL's below?
             #!is.na(tree$VOLCFSND) & # This is what the SQL query says I think? NA if DIA < 5.0"...commenting it out for now, and assigning these 0 VOL's below?
             tree$STATUSCD==1,
                  c("TREE.CN","TREE","PLT_CN","SUBP","SPCD","STATUSCD","CONDID",
                      "TPA_UNADJ",  # should be 1/(plot size)
                      "DIA",        # diameter in inches 
                      "VOLCFGRS",   # gross vol from 1' stump to 4" top in cu ft 
                      "VOLCFSND",   # sound vol from 1' stump to 4" top in cu ft
                      "VOLCFNET",   # sound vol from 1' stump to 4" top in cu ft Net merchantable bole wood volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land
                      "DRYBIO_AG",  # in pounds 
                      "CARBON_AG","CARBON_BG")] # in pounds here 

# Set NA attribute values to 0's
vars_to_fix <- c("VOLCFSND","VOLCFGRS","VOLCFNET","DRYBIO_AG","CARBON_AG","CARBON_BG")
for (vr in vars_to_fix){
  tree[,vr] <- ifelse(is.na(tree[,vr]),0,tree[,vr])
}
sum(is.na(tree$VOLCFNET))

# Merge trees and plots 
head(tree)
tree_plots <- merge(plots_cond, tree, by.x = c("CN","CONDID"), by.y = c("PLT_CN","CONDID"),all.x=T,all.y=T)
#tree_plots <- merge(plots_cond, tree, by.x = c("CN"), by.y = c("PLT_CN"),all.x=T,all.y=T)
#tree_plots <- merge(plots_cond, tree, by.x = c("CN","CONDID"), by.y = c("PLT_CN","CONDID"),all.y=T)
#tree_plots <- merge(plots_cond, tree, by.x = c("CN","CONDID"), by.y = c("PLT_CN","CONDID"),all.x=T)
dim(tree)
dim(tree_plots) 
#head(tree_plots)
length(unique(tree_plots$CN))

sum(is.na(tree_plots$DIA))

test <- tree_plots[is.na(tree_plots$DIA),]
table(test$COND_STATUS_CD)

test[test$COND_STATUS_CD==1,]

# Why are there COND_STATUS_CD==1 trees here?

# dim(test)
# length(unique(test$CN))

# Because of an error I made, I was trying to find duplicates, but resolved now:
# duplicated_rows <- tree_plots[duplicated(tree_plots), ]
# dup_tree_cns <- tree_plots[duplicated(tree_plots[, "TREE.CN"]), ]
# head(dup_tree_cns)
# tree_plots <- tree_plots[!duplicated(tree_plots[, "TREE.CN"]), ]
# dim(tree_plots)

### 6. Read Species Reference (REF_SPECIES) Table 
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
ref_species_query <- paste0("SELECT * FROM REF_SPECIES ")
ref_species <- dbGetQuery(con,ref_species_query)
dbDisconnect(con)

head(ref_species)

ref_species <- ref_species[,c("SPCD","WOODLAND")]

tree_plots <- merge(tree_plots, ref_species, by = "SPCD", all.x=T) # need to keep emptry (NA) tree records from previous merge, so all.x=T

table(tree_plots$INVYR)
table(tree_plots$MEASYEAR)

# Eliminate woodland species
table(tree_plots$WOODLAND) # I guess they already are gone but I'll do it anyway
tree_plots <- tree_plots[tree_plots$WOODLAND=="N"|is.na(tree_plots$WOODLAND),] # some tree-level records came in as NA in last merge, so keep those too

# Now compute the tree-level values the SQL script calls "ESTIMATED_VALUE"?
tree_plots$ESTIMATED_VALUE_VOLCFNET <- 0
for(i in 1:nrow(tree_plots)){
  TPA_UNADJ <- tree_plots$TPA_UNADJ[i]
  VOLCFNET <- tree_plots$VOLCFNET[i]
  prod <- TPA_UNADJ * VOLCFNET
  MACR_BREAK <- tree_plots$MACRO_BREAKPOINT_DIA[i]
  if(is.na(tree_plots$DIA[i])){tree_plots$ESTIMATED_VALUE_VOLCFNET[i] <- 0}
  else if(tree_plots$DIA[i]<=4.999){tree_plots$ESTIMATED_VALUE_VOLCFNET[i] <- prod * tree_plots$ADJ_FACTOR_MICR[i]}
  else if(!is.na(MACR_BREAK) & tree_plots$DIA[i]>=MACR_BREAK){tree_plots$ESTIMATED_VALUE_VOLCFNET[i] <- prod * tree_plots$ADJ_FACTOR_MACR[i]}
  else {tree_plots$ESTIMATED_VALUE_VOLCFNET[i] <- prod * tree_plots$ADJ_FACTOR_SUBP[i]}
}

### Now compute the total estimate

ESTIMATED_TOTAL <- sum(tree_plots$ESTIMATED_VALUE_VOLCFNET * tree_plots$EXPNS)
# Should be 27,418,439,444 for VOLCFNET for Maine 232024

### Now compute standard error

# Read population estimation unit (POP_ESTN_UNIT) table
# I guess I only need this to get total population area (A_T)? Or...?
# Specify POP_ESTN_UNIT query
pop_estn_unit_query <- paste0("SELECT * FROM POP_ESTN_UNIT WHERE ",
                              "EVALID IN (",paste(unique(tree_plots$EVALID),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
pop_estn_unit <- dbGetQuery(con,pop_estn_unit_query)
dbDisconnect(con)

pop_estn_unit <- pop_estn_unit[,c("CN","EVAL_CN","EVALID","ESTN_UNIT","ESTN_UNIT_DESCR",
                                  "AREA_USED","P1PNTCNT_EU")]
head(pop_estn_unit)
dim(pop_estn_unit)

# create a new version of tree plots in case something gets messed up
for_trees <- tree_plots
# make volume zero (not non-zero or NA) for any trees or fake-tree-records that are not on accessible forest land
for_trees$VOLCFNET <- ifelse(for_trees$COND_STATUS_CD==1,for_trees$ESTIMATED_VALUE_VOLCFNET,0)
# sum tree volumes to plot level (keep some stratum and estn unit stuff)
for_trees_plots <- aggregate(for_trees$VOLCFNET,
                             by=list(CN=for_trees$CN,
                                     EXPNS=for_trees$EXPNS,
                                     ESTN_UNIT=for_trees$ESTN_UNIT,
                                     STRATUMCD=for_trees$STRATUMCD),
                             sum)
dim(for_trees_plots) # should be 3498 plots for Maine 232024
head(for_trees_plots)
colnames(for_trees_plots)[ncol(for_trees_plots)] <- "VOLCFNET_plot"
dim(for_trees_plots[for_trees_plots$VOLCFNET_plot>0,]) # should be 3073 non-zero plots

# get within stratum standard errors [GB2 eq 4 on page 8]
v_Yhd <- aggregate(for_trees_plots$VOLCFNET_plot,
                   by=list(ESTN_UNIT=for_trees_plots$ESTN_UNIT,
                           STRATUMCD=for_trees_plots$STRATUMCD),
                   FUN=function(z){var(z)/length(z)}) 
# note: var includes /(n-1), /n added via /length(z)

colnames(v_Yhd)[ncol(v_Yhd)] <- "VOLCFNET_eu_strat_se"

# add the stratum point/pixel count stuff to the latter
v_Yhd_plus_total <- merge(v_Yhd,pop_stratum)

# copy the list of estimation units for building estn unit level variances 
pop_estn_unit_total <- pop_estn_unit
pop_estn_unit_total$var_vol <- 0
# loop through the estn units
for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
  # pull all strata in this estn unit
  strata_in_unit <- v_Yhd_plus_total[v_Yhd_plus_total$ESTN_UNIT==eu,]
  # get the W_h weights for each strata within the estimation unit
  strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
  # get the total p2 sample size in this estimation unit
  n <- sum(strata_in_unit$P2POINTCNT)
  # implement GB2 equation 3 page 8 in two parts for this estimation unit
  part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se))
  part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se)/n)
  # stick the result on the  copied list of estimation units
  pop_estn_unit_total$var_vol[pop_estn_unit_total$ESTN_UNIT==eu] <- (part1 + part2)/n
}

# combine the estimation unit level variances together, using the area variable
total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se <- sqrt(total_var)

se / ESTIMATED_TOTAL * 100 # should be 1.233 for VOLCFNET for Maine 232401 ...
                           # and 1.347 for VOLCFNET for Montana 302201...(getting something slightly different because I'm apparently using different DB version than EVALIDATOR)

# Merge relevant plot info back onto for_trees_plots 
for_trees_plots2 <- merge(for_trees_plots,
                          plots[,c("CN","STATECD","UNITCD","COUNTYCD","PLOT","INVYR","MEASYEAR","P2PANEL","PLOT_STATUS_CD")],
                          by="CN")
colnames(for_trees_plots2)[colnames(for_trees_plots2)=="PLOT"] <- "PLOT_FIADB"
head(for_trees_plots2)

# Save ESTIMATED_TOTAL, for_trees_plots_2, and v_Yhd_plus_total for Maine 232401
save(pop_estn_unit_total,ESTIMATED_TOTAL, for_trees_plots2, v_Yhd_plus_total, file=file.path("data","total_vol_me.Rdata"))
# Last saved 7/17/2026

# Save ESTIMATED_TOTAL, for_trees_plots_2, and v_Yhd_plus_total for Montana 302022
#save(pop_estn_unit_total,ESTIMATED_TOTAL, for_trees_plots2, v_Yhd_plus_total, file=file.path("data","total_vol_mt.Rdata"))
# Last saved 7/17/2026




# The formula below for an SE estimator came from a SQL script I got from John Shaw.
# I haven't been able to get any sensible output from it.

# SE_pct <- sqrt(sum(A_T * A_T * (
#   Wh * nh / sump2 * ((
#     ysqr - nh * (ESTIMATED_SUM / nh) * (ESTIMATED_SUM / nh)
#   ) / (nh - 1.0) / nh) + (1.0 - Wh) / SUMP2 * nh / SUMP2 * ((
#     ysqr - nh * (ESTIMATED_SUM / nh) * (ESTIMATED_SUM / nh)
#   ) / (nh - 1.0) / nh)
# ))) / sum(EXPEST) * 100

# ### Estimate SE of total using GBII Eq. 6? Never quite got this working either...
# stratum_w_n <- pop_stratum
# stratum_w_n <- stratum_w_n[order(stratum_w_n$ESTN_UNIT), ]
# 
# head(stratum_w_n)
# 
# eus <- sort(unique(tree_plots$ESTN_UNIT))
# 
# #eu <- 4
# #rm(eu)
# 
# # Initiate vectors to fill with EU total estimates and variance estimates 
# tau_hat_eu <- NULL
# v_Yd <- NULL
# 
# for(eu in eus){
#   # Get tree-level data for current EU
#   estn_unit_trees <- tree_plots[tree_plots$ESTN_UNIT==eu,] 
#   # Compute estimated total of [y] for the current EU and stick it in the vector
#   tau_hat_eu <- c(tau_hat_eu, sum(estn_unit_trees$ESTIMATED_VALUE_VOLCFSND * estn_unit_trees$EXPNS))
#   # Subset POP_STRATUM to stratum info for current EU
#   estn_unit_strata_info <- pop_stratum[pop_stratum$ESTN_UNIT==eu,] 
#   # Isolate stratum weights, stratum sample sizes, EU sample size, and total EU area 
#   Wh <- estn_unit_strata_info$P1POINTCNT / sum(estn_unit_strata_info$P1POINTCNT)
#   nh <- estn_unit_strata_info$P2POINTCNT # Should I use this to get EU X stratum sample size?
#   n <- sum(estn_unit_strata_info$P2POINTCNT) # Should I use this to get EU sample size?
#   #n <- length(unique(estn_unit_trees$CN)) # Or should I use this to get EU sample size?
#   A_T <- pop_estn_unit$AREA_USED[pop_estn_unit$ESTN_UNIT==eu]
#   # Get vector of unique stratum codes for strata intersecting current EU
#   strata <- sort(unique(estn_unit_trees$STRATUMCD))
#   # Initiate vector to fill with stratum-level sample variances (eq. 4 in GB II)
#   v_Yhd <- NULL
#   #strat <- 12345 # to test loop
#   #nh <- NULL # Or should I use this to get EU sample size?
#   for(strat in strata){
#     # I'm getting NaN sample variances this way because there are some EU X stratum combos with 1 plot:
#     estn_unit_stratum_trees <- estn_unit_trees[estn_unit_trees$STRATUMCD==strat,]
#     estn_unit_stratum_trees$VOLCFSND <- ifelse(is.na(estn_unit_stratum_trees$VOLCFSND),0,estn_unit_stratum_trees$VOLCFSND) # need to turn NA values to 0's
#     estn_unit_stratum_plots <- aggregate(estn_unit_stratum_trees[,"VOLCFSND"],
#                                          by=list(PLT_CN=estn_unit_stratum_trees$CN,
#                                                  EXPNS=estn_unit_stratum_trees$EXPNS),
#                                          sum)
#     #nh <- c(nh,nrow(estn_unit_stratum_plots)) # save stratum sample sizes to calculate EU-level variance
#     num <- sum((estn_unit_stratum_plots$x - mean(estn_unit_stratum_plots$x))^2)
#     denom <- nrow(estn_unit_stratum_plots) * (nrow(estn_unit_stratum_plots) - 1)
#     v_Yhd <- c(v_Yhd,(num/denom))
#     
#     # # In the below, I was doing this by trees, but should this be by plot?? 
#     # estn_unit_stratum_trees <- estn_unit_trees[estn_unit_trees$STRATUMCD==strat,]
#     # num <- sum((estn_unit_stratum_trees$VOLCFSND - mean(estn_unit_stratum_trees$VOLCFSND))^2)
#     # denom <- nrow(estn_unit_stratum_trees) * (nrow(estn_unit_stratum_trees) - 1)
#     # v_Yhd <- c(v_Yhd,(num/denom))
#   }
#   # Finally compute EU-level variance estimate (eq. 3 in GB II)
#   part0 <- sum(Wh * nh * v_Yhd)
#   part1 <- sum((1 - Wh) * (nh/n) * v_Yhd)
#   #v_Yd <- c(v_Yd, (A_T^2 * ((1/n) * (part0 + part1))))
#   v_Yd <- c(v_Yd, ((1/n) * (part0 + part1)))
# }
# 
# v_Yd
# tau_hat_eu
# 
# eu_estimates <- data.frame(eus, tau_hat_eu, v_Yd)
# 
# total_var <- sum(eu_estimates$v_Yd*pop_estn_unit$AREA_USED^2)
# 
# se <- sqrt(total_var)
# se / ESTIMATED_TOTAL * 100 # should be 1.237...and works for me
