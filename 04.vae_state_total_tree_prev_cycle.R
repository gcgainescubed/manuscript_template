source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

# Specify an Evaluation (EVALID)
#my_evalids <- c(231901) # This is the evaluation for [Maine, 2015-2019, Sampled plots used for current area and condition-level estimates.])
#my_evalids <- c(301201) # This is the evaluation for [Montana, 2003-2012, Sampled plots used for current area and condition-level estimates.])
my_evalids <- c(011201) # This is the evaluation for [Alabama, 2006-2012 (most recent previous complete 7 yr cycle?), Sampled plots used for current area and condition-level estimates.])

# Specify a Research Station (RSCD)
#my_stations <- c(24) # 24 is NERS, appropriate Research Station for Maine (Does not matter for FIADB because "we don't publish NFS crossover plots")

### 1. Read Population Stratum (POP_STRATUM) table for EVALIDs and RSCDs of interest 
# Specify POP_STRATUM query for EVALID and RSCD
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
                      "VOLCFNET",   # sound vol from 1' stump to 4" top in cu ft Net merchantable bole wood volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land
                      "DRYBIO_AG",  # in pounds 
                      "CARBON_AG","CARBON_BG")] # in pounds here 

# Set NA attribute values to 0's
vars_to_fix <- c("VOLCFSND","VOLCFGRS","VOLCFNET","DRYBIO_AG","CARBON_AG","CARBON_BG")
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
colnames(for_trees_plots)[ncol(for_trees_plots)] <- "VOLCFNET_plot" # These the y_hid's in GBII?

### Compute total estimate
ESTIMATED_TOTAL <- sum(for_trees_plots$VOLCFNET * for_trees_plots$EXPNS)

# Merge relevant plot info back onto for_trees_plots 
for_trees_plots2 <- merge(for_trees_plots,
                          plots[,c("CN","STATECD","UNITCD","COUNTYCD","PLOT","INVYR","MEASYEAR","P2PANEL")],
                          by="CN")
colnames(for_trees_plots2)[colnames(for_trees_plots2)=="PLOT"] <- "PLOT_FIADB"
head(for_trees_plots2)
dim(for_trees_plots2)

for_trees_plots_prev_cycle <- for_trees_plots2

# Save for_trees_plots_prev_cycle, Maine, 2015-2019
#save(for_trees_plots_prev_cycle,file=file.path("data","prev_cycle_me.Rdata"))
# Last saved 7/17/2026

# Save for_trees_plots_prev_cycle, Montana, 2003-2012
#save(for_trees_plots_prev_cycle,file=file.path("data","prev_cycle_mt.Rdata"))
# Last saved 7/17/2026

# Save for_trees_plots_prev_cycle, Alabama, 2006-2012
save(for_trees_plots_prev_cycle,file=file.path("data","prev_cycle_al.Rdata"))
# Last saved 9/18/2026
