source("00.initialize.R")
# libraries
library(DBI)
library(RSQLite)
library(lattice)
library(knitr)
library(sf)

### 1. Compute variances/standard errors of numerator and denominator separately, and  
# then compute the ratio estimate

# Read in data from previous scripts for Maine
#load(file.path("data","total_vol_me.Rdata"))
#load(file.path("data","total_area_me.Rdata"))

# Read in data from previous scripts for Montana
#load(file.path("data","total_vol_mt.Rdata"))
#load(file.path("data","total_area_mt.Rdata"))

# Read in data from previous scripts for Montana
load(file.path("data","total_vol_al.Rdata"))
load(file.path("data","total_area_al.Rdata"))

# Compute variance (and se %) of the tree-attriubte total
total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se <- sqrt(total_var)

ESTIMATED_TOTAL
se / ESTIMATED_TOTAL * 100 

# Compute variance (and se %) of the area total
area_var <- sum(pop_estn_unit_area$var_cond_prop*pop_estn_unit_area$AREA_USED^2)
se <- sqrt(area_var)

ESTIMATED_AREA
se / ESTIMATED_AREA * 100 

# Compute ratio estimate
RAT <- ESTIMATED_TOTAL / ESTIMATED_AREA
RAT # Should be 1,574.0762 for VOLCFNET in Maine 232401
    # Should be 1,514.3170 for VOLCFNET in Montana 302201 (but slightly different DB, so, off)

# 2. Compute estimated variance of the ratio estimate

# merge plot totals of tree attribute and forest condition proportion
# (in the following data frames, VOLCFSND_plot and COND_PROP_plot are 
# the p-adjusted plot observations (eq 18/19 GBII) "y_hid")
plots <- merge(for_trees_plots2,for_area_plots,by=c("CN","EXPNS","ESTN_UNIT","STRATUMCD"))
head(plots);dim(plots) # as many rows as unique plot measurements (zero and non-zero alike) 

head(pop_estn_unit_area);dim(pop_estn_unit_area) # as many rows as estimation units
head(pop_estn_unit_total);dim(pop_estn_unit_total)

head(v_Yhd_plus_area);dim(v_Yhd_plus_area) # as many rows as unique combos of estimation unit x stratum
head(v_Yhd_plus_total);dim(v_Yhd_plus_total)

# Compute plot covariances within each estimation unit X stratum (GBII eq 13)
a <- array2DF(tapply(plots[,c("VOLCFNET_plot","COND_PROP_plot")],
                     plots[,c("ESTN_UNIT","STRATUMCD")],
                     FUN=function(x){cov(x)[1,2]/nrow(x)}))

a <- a[!is.na(a$Value),]
a$ESTN_UNIT <- as.numeric(a$ESTN_UNIT);a$STRATUMCD <- as.numeric(a$STRATUMCD)
colnames(a) <- c("ESTN_UNIT", "STRATUMCD","cov_1")
dim(a) 
a <- a[order(a$ESTN_UNIT,a$STRATUMCD),]

# Now compute estimation unit-level covariances (GBII eq 12)

pop_estn_unit_total$cov_1 <- 0

# loop through the estn units
for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
  # pull all strata in this estn unit
  strata_in_unit <- v_Yhd_plus_total[v_Yhd_plus_total$ESTN_UNIT==eu,]
  # get the W_h weights for each strata within the estimation unit
  strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
  # get the total p2 sample size in this estimation unit
  n <- sum(strata_in_unit$P2POINTCNT)
  # implement GB2 equation 12 page 10 in two parts for this estimation unit
  strata_in_unit <- merge(strata_in_unit, a, by=c("ESTN_UNIT","STRATUMCD"))
  part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*cov_1))
  part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*cov_1)/n)
  # stick the result on the  copied list of estimation units
  pop_estn_unit_total$cov_1[pop_estn_unit_total$ESTN_UNIT==eu] <- (part1 + part2)/n
}

pop_estn_unit_total

# GBII eq. 12
ratio_cov <- sum(pop_estn_unit_total$cov_1*pop_estn_unit_total$AREA_USED^2)

# GBII eq. 11
ratio_var <- (total_var + RAT^2 * area_var - 2*RAT*ratio_cov) / ESTIMATED_AREA^2

se <- sqrt(ratio_var)

se / RAT * 100 # Should be 1.215 % for VOLCFNET in Maine 232401...
               # Should be 1.386 % for VOLCFNET in Montana 302201 (but slightly different DB, so, off)

