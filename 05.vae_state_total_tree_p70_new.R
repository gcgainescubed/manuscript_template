source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

# Specify EVALID Research Station for Maine 
my_evalids <- c(232401) # This is the evaluation for [Maine, 2020-2024, Sampled plots used for current area and condition-level estimates.])
stations <- "NRS"

# Load current cycle data for Maine
load(file.path("data","total_vol_me.Rdata"))

# Load previous cycle data for Maine 
load(file.path("data","prev_cycle_me.Rdata"))

# Specify EVALID and Research Station for Montana 
#my_evalids <- c(302201) # This is the evaluation for [Montana, 2013-2022, Sampled plots used for current area and condition-level estimates.])
#stations <- "RMRS"

# Load current cycle data for Montana
#load(file.path("data","total_vol_mt.Rdata"))

# Load previous cycle data for Montana
#load(file.path("data","prev_cycle_mt.Rdata"))

# Rearrange/rename some stuff from the previous cycle plot data
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="VOLCFNET_plot"] <- "VOLCFNET_plot_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="INVYR"] <- "INVYR_0"
colnames(for_trees_plots_prev_cycle)[colnames(for_trees_plots_prev_cycle)=="MEASYEAR"] <- "MEASYEAR_0"
head(for_trees_plots_prev_cycle)

for_trees_plots3 <- merge(for_trees_plots2,
                          for_trees_plots_prev_cycle[,c("STATECD","UNITCD","COUNTYCD","PLOT_FIADB","INVYR_0","MEASYEAR_0","VOLCFNET_plot_0")],
                          by=c("STATECD","COUNTYCD","UNITCD","PLOT_FIADB"),
                          all.x = T)
dim(for_trees_plots3)

for_trees_plots3 <- for_trees_plots3[,c(5,1:4,13,7,8,6,12,10,11,9,14,15,16)]
head(for_trees_plots3)

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

# I don't think I'm supposed to do this in Maine, because EVALIDATOR uses intensified R9 plots or something?
# But do I need to do it for Montana? And if so, maybe intensified plots need to be removed for MT in previous scripts?
#all_hex <- all_hex[all_hex$INTENSITY==1,] 
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

table(plots_p70$P2PANEL,plots_p70$PANEL_70)

#hist(plots_p70$VOLCFSND_plot)
plot(density(plots_p70$VOLCFNET_plot), main = "Distribution of current cycle volume measurements")

# The following shows that plot volume change values = NA result from sampled plots
# in the previous cycle that weren't sampled in the current cycle:
# test_plotcns <- plots_p70$PLOT_FIADB[is.na(plots_p70$VOLCFSND_plot_change)]
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots3$PLOT_FIADB%in%test_plotcns)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB)
# sum(for_trees_plots_prev_cycle$PLOT_FIADB%in%plots_p70$PLOT_FIADB) + length(test_plotcns) # same dims as total num plots in current cycle

changes <- plots_p70$VOLCFNET_plot_change[plots_p70$PLOT_STATUS_CD==1&
                                            !is.na(plots_p70$VOLCFNET_plot_change)]

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

