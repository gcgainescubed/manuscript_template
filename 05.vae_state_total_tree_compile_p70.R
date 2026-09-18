source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

# Specify EVALID and Research Station for Maine
#my_evalids <- c(232401) # This is the evaluation for [Maine, 2020-2024, Sampled plots used for current area and condition-level estimates.])
#stations <- "NRS"

# Load current cycle data for Maine
#load(file.path("data","total_vol_me.Rdata"))

# Load previous cycle data for Maine
#load(file.path("data","prev_cycle_me.Rdata"))

# # Specify EVALID and Research Station for Montana 
# my_evalids <- c(302201) # This is the evaluation for [Montana, 2013-2022, Sampled plots used for current area and condition-level estimates.])
# stations <- "RMRS"
# 
# # Load current cycle data for Montana
# load(file.path("data","total_vol_mt.Rdata"))
# 
# # Load previous cycle data for Montana
# load(file.path("data","prev_cycle_mt.Rdata"))

# Specify EVALID and Research Station for Alabama
my_evalids <- c(011901) # This is the evaluation for [Alabama, 2013-2019 (most recent complete 7 yr cycle?), Sampled plots used for current area and condition-level estimates.])
stations <- "SRS"

# Load current cycle data for Maine
load(file.path("data","total_vol_al.Rdata"))

# Load previous cycle data for Maine
load(file.path("data","prev_cycle_al.Rdata"))

# Rearrange/rename some stuff from the previous cycle plot data
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="VOLCFNET_plot"] <- "VOLCFNET_plot_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="INVYR"] <- "INVYR_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="MEASYEAR"] <- "MEASYEAR_0"
head(for_trees_plots_prev_cycle)

# Merge previous cycle data onto current cycle data
for_trees_plots3 <- merge(for_trees_plots2,
                          for_trees_plots_prev_cycle[,c("STATECD","UNITCD","COUNTYCD","PLOT_FIADB","INVYR_0","MEASYEAR_0","VOLCFNET_plot_0")],
                          by=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                          all.x = T)
dim(for_trees_plots3)

for_trees_plots3 <- for_trees_plots3[,c(5,1:4,13,7,8,6,12,10,11,9,14,15,16)] # Get columns in order you want
head(for_trees_plots3)

# Or if you only care about the EVALID and not RSCD
pop_stratum_query <- paste0("SELECT * FROM POP_STRATUM WHERE ",
                            "EVALID IN (",paste(unique(my_evalids),collapse=","),") ")

# connect to FIA sql database
db_name <- "SQLite_FIADB_ENTIRE.db"
con <- dbConnect(SQLite(),file.path(FIAdata,db_name))

# get plot data for our survey units
pop_stratum <- dbGetQuery(con,pop_stratum_query)

# Disconnect from FIA database
dbDisconnect(con)

pop_stratum <- pop_stratum[,c("CN","ESTN_UNIT_CN","RSCD","EVALID","ESTN_UNIT",
                              "STRATUMCD","STRATUM_DESCR","STATECD","P1POINTCNT",
                              "P2POINTCNT","EXPNS","ADJ_FACTOR_MICR","ADJ_FACTOR_SUBP","ADJ_FACTOR_MACR")]
colnames(pop_stratum)[colnames(pop_stratum) == "CN"] <- "POP_STRATUM.CN"

# # Read population estimation unit (POP_ESTN_UNIT) table
# # Specify POP_ESTN_UNIT query
# pop_estn_unit_query <- paste0("SELECT * FROM POP_ESTN_UNIT WHERE ",
#                               "EVALID IN (",paste(unique(tree_plots$EVALID),collapse=","),") ")
# con <- dbConnect(SQLite(),file.path(FIAdata,db_name))
# pop_estn_unit <- dbGetQuery(con,pop_estn_unit_query)
# dbDisconnect(con)
# 
# pop_estn_unit <- pop_estn_unit[,c("CN","EVAL_CN","EVALID","ESTN_UNIT","ESTN_UNIT_DESCR",
#                                   "AREA_USED","P1PNTCNT_EU")]
# head(pop_estn_unit)
# dim(pop_estn_unit)

############ Now DO SOME P70 STUFF
# NIMS_BASE_HEX
library(odbc)
TNSname <- "FIADB01P"
DBI::dbCanConnect(odbc::odbc(), TNSname) 
con <- DBI::dbConnect(odbc::odbc(), TNSname, rows_at_time = 500)

#states <- c(4,8,16,30,32,35,49,56) # RMRS
#states <- c(2,6,25,41,53) # PNWRS
#states <- as.numeric(substr(my_evalids,1,2))
states <- 01 # Alabama

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

# I don't think I'm supposed to do this in Maine, because EVALIDATOR uses intensified R9 plots or something?
# But do I need to do it for Montana? And if so, maybe intensified plots need to be removed for MT in previous scripts?
#all_hex <- all_hex[all_hex$INTENSITY==1,] 
head(all_hex)
dim(all_hex)

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

head(all_plot)
dim(all_plot)

dbDisconnect(con)

# Merge hexagon and plot info from NIMS
nims_hex_plot <- merge(all_hex, all_plot, by.x="CN", by.y = "NBH_CN")
dim(nims_hex_plot)

head(nims_hex_plot)

# merge NIMS hex/plot info onto FIADB previous/current plot data
plots_p70 <- merge(nims_hex_plot[,c("P2HEX","SUBPANEL","PANEL_70","SUBPANEL_14","STATECD","COUNTYCD","UNITCD","PLOT_FIADB","INTENSITY")],
                   for_trees_plots3,
                   by.x=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                   by.y=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                   all.x=F)  

# Compute annualized change in volume from previous to current measurement
# (T2 vol - T1 vol / realized remeasurement inerval)
plots_p70$REMPER <- plots_p70$MEASYEAR - plots_p70$MEASYEAR_0 # Compute the actual realized remeasurement period
plots_p70$VOLCFNET_plot_change <- (plots_p70$VOLCFNET_plot - plots_p70$VOLCFNET_plot_0) / plots_p70$REMPER 

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
table(ceiling(plots_p70$PANEL_70/10)) # How many P70's per panel in 10 year cycle?
table(ceiling(plots_p70$PANEL_70/7)) # How many P70's per panel in 10 year cycle?

table(plots_p70$P2PANEL,plots_p70$PANEL_70)
table(plots_p70$INVYR,plots_p70$PANEL_70)

#hist(plots_p70$VOLCFNET_plot)
plot(density(plots_p70$VOLCFNET_plot), main = "Distribution of current cycle volume measurements")
max(plots_p70$VOLCFNET_plot)
plots_p70[plots_p70$VOLCFNET_plot>14000,] # What is up with this insane MT plot? 

# The following shows that plot volume change values = NA result from sampled plots
# in the previous cycle that weren't sampled in the current cycle:
# test_plotcns <- plots_p70$PLOT_FIADB[is.na(plots_p70$VOLCFSND_plot_change)]
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots3$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB) + length(test_plotcns) # same dims as total num plots in current cycle

# Save plots_p70 for Maine 232401
save(plots_p70, pop_stratum, file=file.path("data","plots_p70_me.Rdata"))

# Save plots_p70 and pop_stratum for Montana 302022
#save(plots_p70, pop_stratum, file=file.path("data","plots_p70_mt.Rdata"))

# Save plots_p70 for Alabama 011901
save(plots_p70, pop_stratum, file=file.path("data","plots_p70_al.Rdata"))

