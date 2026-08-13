# Initialize
source("00.initialize.R")
# Load plots_p70 (combined previous and current cycle data) for Maine
load(file.path("data","plots_p70_mt.Rdata"))
# Load current cycle data for Maine
load(file.path("data","total_vol_mt.Rdata"))

changes <- plots_p70$VOLCFNET_plot_change[plots_p70$PLOT_STATUS_CD==1&
                                            !is.na(plots_p70$VOLCFNET_plot_change)]

# Determine the largest negative annualized change value and add 1
shift <- abs(min(changes)) + 1
# then add this value to all annualized change values to eliminate non-positive values
# (because Weibull distribution can't handle negative and 0 values)
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

mean(rweibull(n=100000,shape = fit[1]$estimate[1],
              scale = fit[1]$estimate[2]))-shift

### SLOW START SCENARIO
# Here, we simulate a slow start (Some panel 70's pushed forward from first few P2 panels)
ext_cycle <- plots_p70

# NEED TO ACOUNT FOR THE FACT THAT 2020 INVYR WAS NOT PANEL70 1-14, 2020 STARTED WITH P70=15 as the following shows:
table(plots_p70$INVYR,plots_p70$PANEL_70)
table(plots_p70$P2PANEL,plots_p70$PANEL_70) # P70's are aligned with P2 Panels, not necessarily INVYRS at beginning of cycle...
# I.e., as the following shows for Maine,
table(plots_p70$INVYR) # Original INVYR distribution
table(ceiling(plots_p70$PANEL_70/14)) # How many P70's per panel in 5 year cycle?

table(plots_p70$INVYR,plots_p70$PANEL_70)

# what prop/pct of plots are meant to be measured in each INVYR?
invyrs <- unique(sort(ext_cycle$INVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(ext_cycle[ext_cycle$INVYR==invyrs[i],])/nrow(ext_cycle)
}

round(props_measured*100,2) # 9.74 / 10.21 / 10.19 / 9.87 / 10.11 / 9.58 / 10.15 / 9.98 / 10.17 / 9.99

### Five year cycle length: 14 P70's/INVYR
ext_cycle$NEWINVYR_5 <- 0
for(i in 1:nrow(ext_cycle)){
  if(ext_cycle$PANEL_70[i] > 0 & ext_cycle$PANEL_70[i] <= 14){ext_cycle$NEWINVYR_5[i] <- 2013} else 
    if(ext_cycle$PANEL_70[i] > 14 & ext_cycle$PANEL_70[i] <= 28){ext_cycle$NEWINVYR_5[i] <- 2014} else 
      if(ext_cycle$PANEL_70[i] > 28 & ext_cycle$PANEL_70[i] <= 42){ext_cycle$NEWINVYR_5[i] <- 2015} else 
        if(ext_cycle$PANEL_70[i] > 42 & ext_cycle$PANEL_70[i] <= 56){ext_cycle$NEWINVYR_5[i] <- 2016} else 
          if(ext_cycle$PANEL_70[i] > 56 & ext_cycle$PANEL_70[i] <= 70){ext_cycle$NEWINVYR_5[i] <- 2017}  
}

table(ext_cycle$INVYR) # Original INVYR distribution
table(ext_cycle$NEWINVYR_5,useNA = "always") # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(ext_cycle$NEWINVYR_5))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(ext_cycle[ext_cycle$NEWINVYR_5==invyrs[i],])/nrow(ext_cycle)
}

round(props_measured*100,2) # 19.95 / 20.06 / 19.69 / 20.14 / 20.16

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
ext_cycle$INVDIFF_5 <- ext_cycle$NEWINVYR_5 - ext_cycle$INVYR
table(ext_cycle$INVDIFF_5,useNA = "always") 

### Seven year cycle length: 10 P70's/INVYR
ext_cycle$NEWINVYR_7 <- 0
for(i in 1:nrow(ext_cycle)){
  if(ext_cycle$PANEL_70[i] > 0 & ext_cycle$PANEL_70[i] <= 10){ext_cycle$NEWINVYR_7[i] <- 2013} else 
    if(ext_cycle$PANEL_70[i] > 10 & ext_cycle$PANEL_70[i] <= 20){ext_cycle$NEWINVYR_7[i] <- 2014} else 
      if(ext_cycle$PANEL_70[i] > 20 & ext_cycle$PANEL_70[i] <= 30){ext_cycle$NEWINVYR_7[i] <- 2015} else 
        if(ext_cycle$PANEL_70[i] > 30 & ext_cycle$PANEL_70[i] <= 40){ext_cycle$NEWINVYR_7[i] <- 2016} else 
          if(ext_cycle$PANEL_70[i] > 40 & ext_cycle$PANEL_70[i] <= 50){ext_cycle$NEWINVYR_7[i] <- 2017} else 
            if(ext_cycle$PANEL_70[i] > 50 & ext_cycle$PANEL_70[i] <= 60){ext_cycle$NEWINVYR_7[i] <- 2018} else 
              if(ext_cycle$PANEL_70[i] > 60 & ext_cycle$PANEL_70[i] <= 70){ext_cycle$NEWINVYR_7[i] <- 2019} 
}

table(ext_cycle$INVYR) # Original INVYR distribution
table(ext_cycle$NEWINVYR_7,useNA = "always") # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(ext_cycle$NEWINVYR_7))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(ext_cycle[ext_cycle$NEWINVYR_7==invyrs[i],])/nrow(ext_cycle)
}

round(props_measured*100,2) # 14.13 / 14.52 / 14.29 / 13.98 / 14.30 / 14.36 / 14.42

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#ext_cycle$INVDIFF <- ext_cycle$NEWINVYR_7 - ext_cycle$INVYR_0
ext_cycle$INVDIFF_7 <- ext_cycle$NEWINVYR_7 - ext_cycle$INVYR
table(ext_cycle$INVDIFF_7,useNA = "always")

### 14 year cycle length: 5 P70's/INVYR
ext_cycle$NEWINVYR_14 <- 0
for(i in 1:nrow(ext_cycle)){
  if(ext_cycle$PANEL_70[i] > 0 & ext_cycle$PANEL_70[i] <= 5){ext_cycle$NEWINVYR_14[i] <- 2013} else 
    if(ext_cycle$PANEL_70[i] > 5 & ext_cycle$PANEL_70[i] <= 10){ext_cycle$NEWINVYR_14[i] <- 2014} else 
      if(ext_cycle$PANEL_70[i] > 10 & ext_cycle$PANEL_70[i] <= 15){ext_cycle$NEWINVYR_14[i] <- 2015} else 
        if(ext_cycle$PANEL_70[i] > 15 & ext_cycle$PANEL_70[i] <= 20){ext_cycle$NEWINVYR_14[i] <- 2016} else 
          if(ext_cycle$PANEL_70[i] > 20 & ext_cycle$PANEL_70[i] <= 25){ext_cycle$NEWINVYR_14[i] <- 2017} else 
            if(ext_cycle$PANEL_70[i] > 25 & ext_cycle$PANEL_70[i] <= 30){ext_cycle$NEWINVYR_14[i] <- 2018} else 
              if(ext_cycle$PANEL_70[i] > 30 & ext_cycle$PANEL_70[i] <= 35){ext_cycle$NEWINVYR_14[i] <- 2019} else
                if(ext_cycle$PANEL_70[i] > 35 & ext_cycle$PANEL_70[i] <= 40){ext_cycle$NEWINVYR_14[i] <- 2020} else
                  if(ext_cycle$PANEL_70[i] > 40 & ext_cycle$PANEL_70[i] <= 45){ext_cycle$NEWINVYR_14[i] <- 2021} else
                    if(ext_cycle$PANEL_70[i] > 45 & ext_cycle$PANEL_70[i] <= 50){ext_cycle$NEWINVYR_14[i] <- 2022} else
                      if(ext_cycle$PANEL_70[i] > 50 & ext_cycle$PANEL_70[i] <= 55){ext_cycle$NEWINVYR_14[i] <- 2023} else
                        if(ext_cycle$PANEL_70[i] > 55 & ext_cycle$PANEL_70[i] <= 60){ext_cycle$NEWINVYR_14[i] <- 2024} else
                          if(ext_cycle$PANEL_70[i] > 60 & ext_cycle$PANEL_70[i] <= 65){ext_cycle$NEWINVYR_14[i] <- 2025} else
                            if(ext_cycle$PANEL_70[i] > 65 & ext_cycle$PANEL_70[i] <= 70){ext_cycle$NEWINVYR_14[i] <- 2026}
}

table(ext_cycle$INVYR) # Original INVYR distribution
table(ext_cycle$NEWINVYR_14) # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(ext_cycle$NEWINVYR_14))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(ext_cycle[ext_cycle$NEWINVYR_14==invyrs[i],])/nrow(ext_cycle)
}

round(props_measured*100,2) # 6.94 / 7.19 / 7.25 / 7.27 / 7.15 / 7.13 / 7.18 / 6.80 / 7.17 / 7.13 / 7.17 / 7.19 / 7.33 / 7.10

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#ext_cycle$INVDIFF <- ext_cycle$NEWINVYR_14 - ext_cycle$INVYR_0
ext_cycle$INVDIFF_14 <- ext_cycle$NEWINVYR_14 - ext_cycle$INVYR
table(ext_cycle$INVDIFF_14,useNA = "always") # So in Maine under this case 

nsim <- 1000

ext_cycle_results <- data.frame(matrix(NA, nrow = nsim, ncol = 6))
colnames(ext_cycle_results) <- c("total_5","se_pct_5","total_7","se_pct_7", "total_14","se_pct_14")

#i <- 1

for(i in 1:nsim){
  # Set the seed to get same random draws in future simulations
  set.seed(i)
  # draw vector of random annual volume growth values
  growths <- rweibull(n=nrow(ext_cycle), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  
  # initiate new volume variables for the 3 different extended cycle lengths
  ext_cycle$NEWVOL_5 <- ext_cycle$NEWVOL_7 <- ext_cycle$NEWVOL_14 <- 0
  
  # Compute new volumes for 5 year cycle
  ext_cycle$NEWVOL_5 <- ifelse(ext_cycle$PLOT_STATUS_CD==1,
                               ext_cycle$VOLCFNET_plot + (ext_cycle$INVDIFF_5 * growths),
                                0)
  ext_cycle$NEWVOL_5 <- ifelse(ext_cycle$NEWVOL_5 < 0, 0, ext_cycle$NEWVOL_5)
  
  # Compute new volumes for 7 year cycle
  ext_cycle$NEWVOL_7 <- ifelse(ext_cycle$PLOT_STATUS_CD==1,
                               ext_cycle$VOLCFNET_plot + (ext_cycle$INVDIFF_7 * growths),
                               0)
  ext_cycle$NEWVOL_7 <- ifelse(ext_cycle$NEWVOL_7 < 0, 0, ext_cycle$NEWVOL_7)

  # Compute new volumes for 14 year cycle
  ext_cycle$NEWVOL_14 <- ifelse(ext_cycle$PLOT_STATUS_CD==1,
                                ext_cycle$VOLCFNET_plot + (ext_cycle$INVDIFF_14 * growths),
                               0)
  ext_cycle$NEWVOL_14 <- ifelse(ext_cycle$NEWVOL_14 < 0, 0, ext_cycle$NEWVOL_14)
  
  # Compare original volumes to new "grown" volumes
  # plot(density(plots_p70$VOLCFNET_plot))
  # lines(density(ext_cycle$NEWVOL),col="red")
  # summary(ext_cycle$VOLCFNET_plot)
  # summary(ext_cycle$NEWVOL)
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL_5 <- sum(ext_cycle$NEWVOL_5 * ext_cycle$EXPNS)
  NEW_ESTIMATED_TOTAL_7 <- sum(ext_cycle$NEWVOL_7 * ext_cycle$EXPNS)
  NEW_ESTIMATED_TOTAL_14 <- sum(ext_cycle$NEWVOL_14 * ext_cycle$EXPNS)
  
  # SE
  
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(cbind(ext_cycle$NEWVOL_5,
                               ext_cycle$NEWVOL_7,
                               ext_cycle$NEWVOL_14),
                         by=list(ESTN_UNIT=ext_cycle$ESTN_UNIT,
                                 STRATUMCD=ext_cycle$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)-2] <- "VOLCFNET_eu_strat_se_5"
  colnames(v_Yhd_new)[ncol(v_Yhd_new)-1] <- "VOLCFNET_eu_strat_se_7"
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se_14"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  
  # copy the list of estimation units for building estn unit level variances 
  pop_estn_unit_total_new <- pop_estn_unit_total
  pop_estn_unit_total_new$var_vol_new_5 <- 0
  pop_estn_unit_total_new$var_vol_new_7 <- 0
  pop_estn_unit_total_new$var_vol_new_14 <- 0
  # loop through the estn units
  for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
    # pull all strata in this estn unit
    strata_in_unit <- v_Yhd_plus_total_new[v_Yhd_plus_total_new$ESTN_UNIT==eu,]
    # get the W_h weights for each strata within the estimation unit
    strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
    # get the total p2 sample size in this estimation unit
    n <- sum(strata_in_unit$P2POINTCNT)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1_5 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_5))
    part2_5 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_5)/n)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1_7 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_7))
    part2_7 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_7)/n)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1_14 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_14))
    part2_14 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_14)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new_5[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_5 + part2_5)/n
    pop_estn_unit_total_new$var_vol_new_7[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_7 + part2_7)/n
    pop_estn_unit_total_new$var_vol_new_14[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_14 + part2_14)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new_5 <- sum(pop_estn_unit_total_new$var_vol_new_5*pop_estn_unit_total_new$AREA_USED^2)
  se_new_5 <- sqrt(total_var_new_5)
  total_var_new_7 <- sum(pop_estn_unit_total_new$var_vol_new_7*pop_estn_unit_total_new$AREA_USED^2)
  se_new_7 <- sqrt(total_var_new_7)
  total_var_new_14 <- sum(pop_estn_unit_total_new$var_vol_new_14*pop_estn_unit_total_new$AREA_USED^2)
  se_new_14 <- sqrt(total_var_new_14)
  
  #se_new / NEW_ESTIMATED_TOTAL * 100 # Compute new SE%
  
  ext_cycle_results$total_5[i] <- NEW_ESTIMATED_TOTAL_5
  ext_cycle_results$se_pct_5[i] <- se_new_5 / NEW_ESTIMATED_TOTAL_5 * 100
  ext_cycle_results$total_7[i] <- NEW_ESTIMATED_TOTAL_7
  ext_cycle_results$se_pct_7[i] <- se_new_7 / NEW_ESTIMATED_TOTAL_7 * 100
  ext_cycle_results$total_14[i] <- NEW_ESTIMATED_TOTAL_14
  ext_cycle_results$se_pct_14[i] <- se_new_14 / NEW_ESTIMATED_TOTAL_14 * 100
}

head(ext_cycle_results)

total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se_pct <- sqrt(total_var) / ESTIMATED_TOTAL * 100

x_bump <- 0.08
y_bump <- 0.05

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL, se_pct, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct - (y_bump), se_pct + (y_bump)),xlim = c(ESTIMATED_TOTAL - (x_bump*ESTIMATED_TOTAL),ESTIMATED_TOTAL + (x_bump*ESTIMATED_TOTAL)),
     xaxt = "n",
     main = "Montana")
points(ext_cycle_results$total_5,ext_cycle_results$se_pct_5, pch=19, col = t_blk) # 7 year cycle
points(ext_cycle_results$total_7,ext_cycle_results$se_pct_7, pch=19, col = t_grn) # 7 year cycle
points(ext_cycle_results$total_14,ext_cycle_results$se_pct_14, pch=19, col = t_org) # 14 year cycle

x_vals <- ESTIMATED_TOTAL * (1 + seq(-0.1, 0.1, by = 0.005))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-10.0, 10.0, 0.5),1), "%"))
abline(h = se_pct, v = ESTIMATED_TOTAL, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL, se_pct, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","5 Year Cycle", "7 Year Cycle", "14 Year Cycle"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")
