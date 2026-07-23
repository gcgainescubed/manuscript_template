source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

# Specify an Evaluation (EVALID)
my_evalids <- c(232401) # This is the evaluation for [Maine, 2020-2024, Sampled plots used for current area and condition-level estimates.])

# Specify a Research Station (RSCD)
my_stations <- c(24) # 24 is NERS, appropriate Research Station for Maine (Does not matter for FIADB because "we don't publish NFS crossover plots")

### 1. Read Population Stratum (POP_STRATUM) table for EVALIDs and RSCDs of interest 
# Specify POP_STRATUM query
pop_stratum_query <- paste0("SELECT * FROM POP_STRATUM WHERE ",
                            "EVALID IN (",paste(unique(my_evalids),collapse=","),") AND ",
                            "RSCD IN (",paste(unique(my_stations),collapse=","),") ")

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

### 2. Read Population Plot Stratum Assignment (POP_PLOT_STRATUM_ASSGN) table (why?)
# Specify POP_PLOT_STRATUM_ASSGN query
pop_plot_stratum_query <- paste0("SELECT * FROM POP_PLOT_STRATUM_ASSGN WHERE ",
                                 "STRATUM_CN IN (",paste(unique(pop_stratum$POP_STRATUM.CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
pop_plot_stratum <- dbGetQuery(con,pop_plot_stratum_query)
dbDisconnect(con)

pop_plot_stratum <- pop_plot_stratum[,c("CN","STRATUM_CN","PLT_CN")] # We only need this to link strata to plot measurements
colnames(pop_plot_stratum)[colnames(pop_plot_stratum) == "CN"] <- "POP_PLOT_STRATUM_ASSGN.CN"

# merge pop_stratum with pop_plot_stratum_assignment 
stratum_plt_assgn <- merge(pop_stratum, pop_plot_stratum, by.x = "POP_STRATUM.CN", by.y = "STRATUM_CN")

### 3. Read Plot (PLOT) table
# Specify PLOT query
plot_query <- paste0("SELECT * FROM PLOT WHERE ",
                     "CN IN (",paste(unique(stratum_plt_assgn$PLT_CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
plots <- dbGetQuery(con,plot_query)
dbDisconnect(con)

plots <- plots[,c("CN","STATECD","COUNTYCD","UNITCD","PLOT","INVYR","MEASYEAR","KINDCD","PLOT_STATUS_CD",
                  "REMPER","DESIGNCD","P2PANEL","INTENSITY","MACRO_BREAKPOINT_DIA")]

# Merge plots onto stratum info
stratum_plots <- merge(plots,stratum_plt_assgn, by.x = "CN", by.y = "PLT_CN")

### 4. Read Condition (COND) table
cond_query <- paste0("SELECT * FROM COND WHERE ",
                     "PLT_CN IN (",paste(unique(stratum_plots$CN),collapse=","),") ")
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
cond <- dbGetQuery(con,cond_query)
dbDisconnect(con)

colnames(cond)[colnames(cond) == "CN"] <- "COND.CN"

cond <- cond[ (cond$COND_STATUS_CD%in%c(1,2,3,4,5)) # Keeping all condition status cd's to avoid eliminating 0 plots...?
              ,
              c("COND.CN","PLT_CN","CONDID","CONDPROP_UNADJ",
                "MICRPROP_UNADJ","SUBPPROP_UNADJ","MACRPROP_UNADJ",
                "COND_STATUS_CD")]

# Set macro plot condition proportion to zero if a macroplot was never installed 
cond$MACRPROP_UNADJ <- as.numeric(cond$MACRPROP_UNADJ)
cond$MACRPROP_UNADJ <- ifelse(is.na(cond$MACRPROP_UNADJ),0,
                                 fiacond$MACRPROP_UNADJ) # MACRPROP_UNADJ is NA if a macroplot was never installed

plots_cond <- merge(stratum_plots,cond, by.x = "CN", by.y = "PLT_CN")

length(unique(stratum_plots$CN)) # I think this and the next line should be equivalent because we don't want to eliminate 0 plots?
length(unique(plots_cond$CN))

### 5. Read Tree (TREE) table 
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
tree_query <- paste0("SELECT * FROM TREE WHERE ",
                     "STATUSCD IN (1) AND ", # only live trees
                     "PLT_CN IN (",paste(unique(plots_cond$CN),collapse=","),")")
tree <- dbGetQuery(con,tree_query)
dbDisconnect(con)

colnames(tree)[colnames(tree) == "CN"] <- "TREE.CN"

# subset to live trees with integer DIA's (DIA is NA for removed trees? for woodland species?)
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
                      "DRYBIO_AG",  # in pounds 
                      "CARBON_AG","CARBON_BG")] # in pounds here 

# Set NA attribute values to 0's
vars_to_fix <- c("VOLCFSND","VOLCFGRS","DRYBIO_AG","CARBON_AG","CARBON_BG")
for (vr in vars_to_fix){
  tree[,vr] <- ifelse(is.na(tree[,vr]),0,tree[,vr])
}

# Merge trees and plots 
tree_plots <- merge(plots_cond, tree, by.x = c("CN","CONDID"), by.y = c("PLT_CN","CONDID"),all.x=T,all.y=T)

### 6. Read Species Reference (REF_SPECIES) Table 
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
ref_species_query <- paste0("SELECT * FROM REF_SPECIES ")
ref_species <- dbGetQuery(con,ref_species_query)
dbDisconnect(con)

ref_species <- ref_species[,c("SPCD","WOODLAND")]

tree_plots <- merge(tree_plots, ref_species, by = "SPCD", all.x=T) # need to keep empty (NA) tree records from previous merge, so all.x=T

table(tree_plots$INVYR)
table(tree_plots$MEASYEAR)

# Eliminate woodland species
table(tree_plots$WOODLAND) 
tree_plots <- tree_plots[tree_plots$WOODLAND=="N"|is.na(tree_plots$WOODLAND),] # some tree-level records came in as NA in last merge, so keep those too

# Now compute the tree-level values the SQL script calls "ESTIMATED_VALUE"?
tree_plots$ESTIMATED_VALUE_VOLCFSND <- 0
for(i in 1:nrow(tree_plots)){
  TPA_UNADJ <- tree_plots$TPA_UNADJ[i]
  VOLCFSND <- tree_plots$VOLCFSND[i]
  prod <- TPA_UNADJ * VOLCFSND
  MACR_BREAK <- tree_plots$MACRO_BREAKPOINT_DIA[i]
  if(is.na(tree_plots$DIA[i])){tree_plots$ESTIMATED_VALUE_VOLCFSND[i] <- 0}
  else if(tree_plots$DIA[i]<=4.999){tree_plots$ESTIMATED_VALUE_VOLCFSND[i] <- prod * tree_plots$ADJ_FACTOR_MICR[i]}
  else if(!is.na(MACR_BREAK) & tree_plots$DIA[i]>=MACR_BREAK){tree_plots$ESTIMATED_VALUE_VOLCFSND[i] <- prod * tree_plots$ADJ_FACTOR_MACR[i]}
  else {tree_plots$ESTIMATED_VALUE_VOLCFSND[i] <- prod * tree_plots$ADJ_FACTOR_SUBP[i]}
}

# create a new version of tree plots in case something gets messed up
for_trees <- tree_plots
# make volume zero (not non-zero or NA) for any trees or fake-tree-records that are not on accessible forest land
for_trees$VOLCFSND <- ifelse(for_trees$COND_STATUS_CD==1,for_trees$ESTIMATED_VALUE_VOLCFSND,0)
# sum tree volumes to plot level (keep some stratum and estn unit stuff)
for_trees_plots <- aggregate(for_trees$VOLCFSND,
                             by=list(CN=for_trees$CN,
                                     EXPNS=for_trees$EXPNS,
                                     ESTN_UNIT=for_trees$ESTN_UNIT,
                                     STRATUMCD=for_trees$STRATUMCD),
                             sum)
colnames(for_trees_plots)[ncol(for_trees_plots)] <- "VOLCFSND_plot" # These the y_hid's in GBII?

### Compute total estimate
ESTIMATED_TOTAL <- sum(for_trees_plots$VOLCFSND_plot * for_trees_plots$EXPNS)

# Read population estimation unit (POP_ESTN_UNIT) table
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

############ Now DO SOME P70 STUFF

# Merge relevant plot info back onto for_trees_plots 
for_trees_plots2 <- merge(for_trees_plots,
                          plots[,c("CN","STATECD","UNITCD","COUNTYCD","PLOT","INVYR","MEASYEAR","P2PANEL","PLOT_STATUS_CD")],
                          by="CN")
colnames(for_trees_plots2)[colnames(for_trees_plots2)=="PLOT"] <- "PLOT_FIADB"
head(for_trees_plots2)

# Load previous cycle data to 
load(file.path("data","prev_cycle.Rdata"))
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="VOLCFSND_plot"] <- "VOLCFSND_plot_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="INVYR"] <- "INVYR_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="MEASYEAR"] <- "MEASYEAR_0"
head(for_trees_plots_prev_cycle)

for_trees_plots3 <- merge(for_trees_plots2,
                          for_trees_plots_prev_cycle[,c("STATECD","UNITCD","COUNTYCD","PLOT_FIADB","INVYR_0","MEASYEAR_0","VOLCFSND_plot_0")],
                          by=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                          all.x = T)
dim(for_trees_plots3)

for_trees_plots3 <- for_trees_plots3[,c(5,1:4,13,7,8,6,12,10,11,9,14,15,16)]
head(for_trees_plots3)

### Now read in tables required to get Panel 70?
# NIMS_BASE_HEX
library(odbc)
TNSname <- "FIADB01P"
DBI::dbCanConnect(odbc::odbc(), TNSname) 
con <- DBI::dbConnect(odbc::odbc(), TNSname, rows_at_time = 500)

stations <- "NRS"
#states <- c(4,8,16,30,32,35,49,56) # RMRS
#states <- c(2,6,25,41,53) # PNWRS
states <- as.numeric(substr(my_evalids,1,2))

hex.tbl <- "NIMS_BASE_HEX"
# hex.vars <- "CN, STATECD, P2HEX, P2PANEL, SUBPANEL, PANEL_70, 
#              SUBPANEL_14, HAS_BASE_PLOT, INTENSITY, LON_NAD83_CENTER, LAT_NAD83_CENTER"
hex.vars <- "CN, P2HEX, SUBPANEL, PANEL_70, 
             SUBPANEL_14, INTENSITY"

# Loop through stations to: create SQL queries using that info, read in tables, 
# merge them, and then rbind results into a nationwide dataframe
all_hex <- data.frame()

for(station in stations){
  station.schema <- paste("FS_NIMS_",station,".",sep="")
  hex.qry <- paste0("select ", hex.vars," from ",
                    paste0(station.schema,hex.tbl), 
                    " where STATECD in ",
                    paste0("(",toString(states), ")"))
  hex <- dbGetQuery(con, hex.qry)
  all_hex <- rbind(all_hex, hex)
}

#all_hex <- all_hex[all_hex$INTENSITY==1,] # I don't think I'm supposed to do this in Maine, because EVALIDATOR uses intensified R9 plots or something?
head(all_hex)

# NIMS_BASE_PLOT
plot.tbl <- "NIMS_BASE_PLOT"
plot.vars <- "NBH_CN, STATECD, COUNTYCD, UNITCD, PLOT_FIADB"

# Loop through stations to: create SQL queries using that info, read in tables, 
# merge them, and then rbind results into a nationwide dataframe

all_plot <- data.frame()

for(station in stations){
  station.schema <- paste("FS_NIMS_",station,".",sep="")
  plot.qry <- paste0("select ", plot.vars," from ",
                     paste0(station.schema,plot.tbl), 
                     " where STATECD in ",
                     paste0("(",toString(states), ")"))
  plot <- dbGetQuery(con, plot.qry)
  all_plot <- rbind(all_plot, plot)
}

dbDisconnect(con)

nims_hex_plot <- merge(all_hex,all_plot,by.x="CN", by.y = "NBH_CN")

head(nims_hex_plot)

# merge NIMS hex/plot info onto plots?
plots_p70 <- merge(nims_hex_plot[,c("P2HEX","SUBPANEL","PANEL_70","SUBPANEL_14","STATECD","COUNTYCD","UNITCD","PLOT_FIADB","INTENSITY")],
                   for_trees_plots3,
                   by.x=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                   by.y=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                   all.x=F)  
# Compute annualized change (T2 vol - T1 vol / realized remeasurement inerval)
plots_p70$REMPER <- plots_p70$MEASYEAR - plots_p70$MEASYEAR_0
plots_p70$VOLCFSND_plot_change <- (plots_p70$VOLCFSND_plot - plots_p70$VOLCFSND_plot_0) / plots_p70$REMPER

dim(for_trees_plots3)
dim(plots_p70)

length(unique(plots_p70$P2HEX))
nrow(unique(plots_p70[,c("STATECD","COUNTYCD","UNITCD","P2HEX")]))
length(unique(plots_p70$CN)) # so this suggests...

# Trying to figure out where the duplicates come from:
duplicated_rows <- plots_p70[duplicated(plots_p70), ]
# duplicated_rows2 <- for_trees_plots3[duplicated(for_trees_plots3), ]
# duplicated_rows3 <- nims_hex_plot[duplicated(nims_hex_plot), ]
# dim(duplicated_rows)
# nrow(plots_p70)-nrow(for_trees_plots3)

# Let's just eliminate the duplicate hex CN's, since they appear to be true duplicates from a merge or something?
plots_p70 <- plots_p70[!duplicated(plots_p70[,"CN"]), ]

#save(plots_p70,file=file.path("data","for_bryce.Rdata")) # Saved plots_p70 for Bryce on 06/01/2026

dim(plots_p70);dim(for_trees_plots2) # Should be same length
head(plots_p70)

# How many P70's per panel in 5 year cycle?
table(ceiling(plots_p70$PANEL_70/14)) # How many P70's per panel in 5 year cycle?

table(plots_p70$P2PANEL,plots_p70$PANEL_70)

#hist(plots_p70$VOLCFSND_plot)
plot(density(plots_p70$VOLCFSND_plot), main = "Distribution of current cycle volume measurements")

# The following shows that plot volume change values = NA result from sampled plots
# in the previous cycle that weren't sampled in the current cycle:
# test_plotcns <- plots_p70$PLOT_FIADB[is.na(plots_p70$VOLCFSND_plot_change)]
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots3$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB) + length(test_plotcns) # same dims as total num plots in current cycle

changes <- plots_p70$VOLCFSND_plot_change[plots_p70$PLOT_STATUS_CD==1&
                                            !is.na(plots_p70$VOLCFSND_plot_change)]

#hist(changes)
plot(density(changes),
     main = "Change in annualized plot-level volume, previous to current cycle")

# Determine the largest negative annualized change value and add 1
shift <- abs(min(changes)) + 1
# then add this value to all annualized change values to eliminate non-positive values
# (because weibull can't handle negative and 0 values)
shifted_changes <- changes + shift

# Fit Weibull distribution to the shifted annualized change values
library(fitdistrplus)
fit <- fitdist(shifted_changes, "weibull")
fit[1]$estimate[1]
fit[1]$estimate[2]

plot(density(changes),
     main = "Change in annualized plot-level volume, previous to current cycle")
lines(density(rweibull(n=1000000,
                       shape = fit[1]$estimate[1],
                       scale = fit[1]$estimate[2])-shift),
      col="red")
lines(density(rweibull(n=1000000,
                       shape = 13.86533,
                       scale = 922.7328)-878),
      col="blue")

mean(rweibull(n=100000,shape = fit[1]$estimate[1],
              scale = fit[1]$estimate[2]))-shift

# Slow start scenario
slow <- plots_p70

# NEED TO ACOUNT FOR THE FACT THAT 2015 INVYR WAS NOT PANEL70 1-14, 2015 STARTED WITH P70=15;

# slow$NEWINVYR <- 0
# for(i in 1:nrow(slow)){
#   if(slow$PANEL_70[i] > 14 & slow$PANEL_70[i] <= 24){slow$NEWINVYR[i] <- 2015} else
#     if(slow$PANEL_70[i] > 24 & slow$PANEL_70[i] <= 34){slow$NEWINVYR[i] <- 2016} else
#       if(slow$PANEL_70[i] > 34 & slow$PANEL_70[i] <= 53){slow$NEWINVYR[i] <- 2017} else
#         if(slow$PANEL_70[i] > 53 & slow$PANEL_70[i] <= 70){slow$NEWINVYR[i] <- 2018} else
#           if(slow$PANEL_70[i] > 0 & slow$PANEL_70[i] <= 14){slow$NEWINVYR[i] <- 2019}
# }

slow$NEWINVYR <- 0
for(i in 1:nrow(slow)){
  if(slow$PANEL_70[i] > 14 & slow$PANEL_70[i] <= 24){slow$NEWINVYR[i] <- 2020} else
    if(slow$PANEL_70[i] > 24 & slow$PANEL_70[i] <= 34){slow$NEWINVYR[i] <- 2021} else
      if(slow$PANEL_70[i] > 34 & slow$PANEL_70[i] <= 53){slow$NEWINVYR[i] <- 2022} else
        if(slow$PANEL_70[i] > 53 & slow$PANEL_70[i] <= 70){slow$NEWINVYR[i] <- 2023} else
          if(slow$PANEL_70[i] > 0 & slow$PANEL_70[i] <= 14){slow$NEWINVYR[i] <- 2024}
}

table(slow$NEWINVYR)

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#slow$INVDIFF <- slow$NEWINVYR - slow$INVYR_0
slow$INVDIFF <- slow$NEWINVYR - slow$INVYR

table(slow$INVDIFF,useNA = "always")

nsim <- 10
#set.seed(687416871)

for(i in 1:nsim){
set.seed(i)
slow$NEWVOL <- 0
ifelse(slow$PLOT_STATUS_CD==1,
       slow$NEWVOL <- slow$VOLCFSND_plot + (slow$INVDIFF * (rweibull(n=1,
                                                                   shape = fit[1]$estimate[1],
                                                                   scale = fit[1]$estimate[2])-shift)),
       0)
}

slow$NEWVOL <- ifelse(slow$NEWVOL < 0, 0, slow$NEWVOL)

plot(density(plots_p70$VOLCFSND_plot))
lines(density(slow$NEWVOL),col="red")

summary(slow$VOLCFSND_plot)
summary(slow$NEWVOL)


sum(is.na(slow$NEWVOL))

NEW_ESTIMATED_TOTAL <- sum(slow$NEWVOL * for_trees_plots$EXPNS)
ESTIMATED_TOTAL

# create a new version of tree plots in case something gets messed up
for_trees <- slow
# make volume zero (not non-zero or NA) for any non-sampled plots?
for_trees$NEWVOL <- ifelse(for_trees$PLOT_STATUS_CD==1,for_trees$NEWVOL,0)
# sum tree volumes to plot level (keep some stratum and estn unit stuff)
for_trees_plots <- aggregate(for_trees$NEWVOL,
                             by=list(CN=for_trees$CN,
                                     EXPNS=for_trees$EXPNS,
                                     ESTN_UNIT=for_trees$ESTN_UNIT,
                                     STRATUMCD=for_trees$STRATUMCD),
                             sum)
dim(for_trees_plots) # should be 3498 plots
head(for_trees_plots)
colnames(for_trees_plots)[ncol(for_trees_plots)] <- "NEWVOL"
dim(for_trees_plots[for_trees_plots$NEWVOL>0,]) # should be XXX non-zero plots?

# get within stratum standard errors [GB2 eq 4 on page 8]
v_Yhd <- aggregate(for_trees_plots$NEWVOL,
                   by=list(ESTN_UNIT=for_trees_plots$ESTN_UNIT,
                           STRATUMCD=for_trees_plots$STRATUMCD),
                   FUN=function(z){var(z)/length(z)}) 
# note: var includes /(n-1), /n added via /length(z)

colnames(v_Yhd)[ncol(v_Yhd)] <- "VOLCFSND_eu_strat_se"

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
  part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFSND_eu_strat_se))
  part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFSND_eu_strat_se)/n)
  # stick the result on the  copied list of estimation units
  pop_estn_unit_total$var_vol[pop_estn_unit_total$ESTN_UNIT==eu] <- (part1 + part2)/n
}

# combine the estimation unit level variances together, using the area variable
total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se <- sqrt(total_var)

se / NEW_ESTIMATED_TOTAL * 100 #

