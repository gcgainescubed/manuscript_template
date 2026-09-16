# Initialize
source("00.initialize.R")
# Load plots_p70 (combined previous and current cycle data) for Maine
load(file.path("data","plots_p70_me.Rdata"))
# Load current cycle data for Maine
load(file.path("data","total_vol_me.Rdata"))

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
lines(density(rweibull(n=1000000, # Westfall's parameter fits fo comparison
                       shape = 13.86533,
                       scale = 922.7328)-878),
      col="blue")

mean(rweibull(n=100000,shape = fit[1]$estimate[1],
              scale = fit[1]$estimate[2]))-shift
abline(v=0,col="gray",lty=3)

### SLOW START SCENARIO
# Here, we simulate a slow start (Some panel 70's pushed forward from first few P2 panels)
slow <- plots_p70

# NEED TO ACOUNT FOR THE FACT THAT 2020 INVYR WAS NOT PANEL70 1-14, 2020 STARTED WITH P70=15 as the following shows:
table(plots_p70$INVYR,plots_p70$PANEL_70)
table(plots_p70$P2PANEL,plots_p70$PANEL_70) # P70's are aligned with P2 Panels, not necessarily INVYRS at beginning of cycle...
# I.e., as the following shows for Maine,
table(plots_p70$INVYR) # Original INVYR distribution
table(ceiling(plots_p70$PANEL_70/14)) # How many P70's per panel in 5 year cycle?

table(plots_p70$P2PANEL,plots_p70$PANEL_70)
table(plots_p70$INVYR,plots_p70$PANEL_70)

# what prop/pct of plots are meant to be measured in each INVYR?
invyrs <- unique(sort(slow$INVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(slow[slow$INVYR==invyrs[i],])/nrow(slow)
}

round(props_measured*100,2) # 20.38 / 20.07 / 19.95 / 19.84 / 19.75

# P70 values are staggered off of the P2 Panel values  1 - 5 by one panel... 
# *Check if this is so for MT as well*

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
  if(slow$PANEL_70[i] > 14 & slow$PANEL_70[i] <= 24){slow$NEWINVYR[i] <- 2020} else # slow: 9 P70's
    if(slow$PANEL_70[i] > 24 & slow$PANEL_70[i] <= 34){slow$NEWINVYR[i] <- 2021} else # slow: 9 P70's
      if(slow$PANEL_70[i] > 34 & slow$PANEL_70[i] <= 53){slow$NEWINVYR[i] <- 2022} else # fast: 18 P70's
        if(slow$PANEL_70[i] > 53 & slow$PANEL_70[i] <= 70){slow$NEWINVYR[i] <- 2023} else # fast: 16 P70's
          if(slow$PANEL_70[i] > 0 & slow$PANEL_70[i] <= 14){slow$NEWINVYR[i] <- 2024} # on time: 14 P70's
}

table(slow$INVYR) # Original INVYR distribution
table(slow$NEWINVYR) # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(slow$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(slow[slow$NEWINVYR==invyrs[i],])/nrow(slow)
}

round(props_measured*100,2) # 14.38 / 14.61 / 27.10 / 24.16 / 19.75

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#slow$INVDIFF <- slow$NEWINVYR - slow$INVYR_0
slow$INVDIFF <- slow$NEWINVYR - slow$INVYR

table(slow$INVDIFF,useNA = "always") # So in Maine under this case 

nsim <- 1000

changes_df <- data.frame(difs = changes, id = 1:length(changes)) 

slow_results <- data.frame(matrix(NA, nrow = nsim, ncol = 4))
colnames(slow_results) <- c("total_weibull","se_pct_weibull","total_empirical","se_pct_empirical")

#i <- 1

for(i in 1:nsim){
  set.seed(i)
  # Weibull growth
  growths_1 <- rweibull(n=nrow(slow), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  slow$growths_1 <- growths_1
  
  slow$NEWVOL_1 <- 0
  slow$NEWVOL_1 <- ifelse(slow$PLOT_STATUS_CD==1,
                          slow$VOLCFNET_plot + (slow$INVDIFF * slow$growths_1),
                          0)
  slow$NEWVOL_1 <- ifelse(slow$NEWVOL_1 < 0, 0, slow$NEWVOL_1)
  
  # Empirical CDF growth: SRSwR
  growths_2 <- sample(changes, size = nrow(slow), replace = TRUE)
  slow$growths_2 <- growths_2
  slow$NEWVOL_2 <- 0
  slow$NEWVOL_2 <- ifelse(slow$PLOT_STATUS_CD==1,
                          slow$VOLCFNET_plot + (slow$INVDIFF * slow$growths_2),
                          0)
  slow$NEWVOL_2 <- ifelse(slow$NEWVOL_2 < 0, 0, slow$NEWVOL_2)
  
  # Empirical CDF growth: SRSwoR
  # slow$NEWVOL_2 <- 0
  # #j <- 1
  # for(j in 1:nrow(slow)){
  #   my_id <- round(runif(n=1, min = 1, max = max(changes_df$id)),0)
  #   my_growth <- changes_df$difs[changes_df$id==my_id]
  #   if(slow$PLOT_STATUS_CD[j]==1){slow$NEWVOL_2[j] <- slow$VOLCFNET_plot[j] + (slow$INVDIFF[j] * my_growth)} else {
  #   slow$NEWVOL_2[j] <- 0}
  # }
  # slow$NEWVOL_2 <- ifelse(slow$NEWVOL_2 < 0, 0, slow$NEWVOL_2)
  
  # Compare original volumes to new "grown" volumes
  # plot(density(plots_p70$VOLCFNET_plot))
  # lines(density(slow$NEWVOL),col="red")
  # summary(slow$VOLCFNET_plot)
  # summary(slow$NEWVOL)
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL_1 <- sum(slow$NEWVOL_1 * slow$EXPNS)
  NEW_ESTIMATED_TOTAL_2 <- sum(slow$NEWVOL_2 * slow$EXPNS)
  
  # Standard error estimator
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(cbind(slow$NEWVOL_1,
                               slow$NEWVOL_2),
                         by=list(ESTN_UNIT=slow$ESTN_UNIT,
                                 STRATUMCD=slow$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)-1] <- "VOLCFNET_eu_strat_se_1"
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se_2"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  
  # copy the list of estimation units for building estn unit level variances 
  pop_estn_unit_total_new <- pop_estn_unit_total
  pop_estn_unit_total_new$var_vol_new_1 <- 0
  pop_estn_unit_total_new$var_vol_new_2 <- 0
  
  # loop through the estn units
  for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
    # pull all strata in this estn unit
    strata_in_unit <- v_Yhd_plus_total_new[v_Yhd_plus_total_new$ESTN_UNIT==eu,]
    # get the W_h weights for each strata within the estimation unit
    strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
    # get the total p2 sample size in this estimation unit
    n <- sum(strata_in_unit$P2POINTCNT)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1_1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_1))
    part2_1 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_1)/n)
    part1_2 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_2))
    part2_2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_2)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new_1[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_1 + part2_1)/n
    pop_estn_unit_total_new$var_vol_new_2[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_1 + part2_1)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new_1 <- sum(pop_estn_unit_total_new$var_vol_new_1*pop_estn_unit_total_new$AREA_USED^2)
  se_new_1 <- sqrt(total_var_new_1)
  total_var_new_2 <- sum(pop_estn_unit_total_new$var_vol_new_2*pop_estn_unit_total_new$AREA_USED^2)
  se_new_2 <- sqrt(total_var_new_2)
  
  #se_new / NEW_ESTIMATED_TOTAL * 100 # Compute new SE%
  
  slow_results$total_weibull[i] <- NEW_ESTIMATED_TOTAL_1
  slow_results$se_pct_weibull[i] <- se_new_1 / NEW_ESTIMATED_TOTAL_1 * 100
  slow_results$total_empirical[i] <- NEW_ESTIMATED_TOTAL_2
  slow_results$se_pct_empirical[i] <- se_new_2 / NEW_ESTIMATED_TOTAL_2 * 100
}

head(slow_results)

### FAST START THEN SLOW
fast <- plots_p70
fast$NEWINVYR <- 0

table(fast$INVYR) # Original INVYR distribution
table(ceiling(fast$PANEL_70/14)) # How many P70's per panel in 5 year cycle?
table(fast$INVYR,fast$P2PANEL)
table(fast$INVYR,fast$PANEL_70)

# Need to account for fact that Panel 70s 1 - 14 are in Panel 5, not Panel 1... 

for(i in 1:nrow(fast)){
  if(fast$PANEL_70[i] > 14 & fast$PANEL_70[i] <= 33){fast$NEWINVYR[i] <- 2020} else # Fast: 18 P70's
    if(fast$PANEL_70[i] > 33 & fast$PANEL_70[i] <= 50){fast$NEWINVYR[i] <- 2021} else # Fast: 16 P70's
      if(fast$PANEL_70[i] > 50 & fast$PANEL_70[i] <= 60){fast$NEWINVYR[i] <- 2022} else # Slow: 9 P70's
        if(fast$PANEL_70[i] > 60 & fast$PANEL_70[i] <= 70){fast$NEWINVYR[i] <- 2023} else # Slow: 9 P70's
          if(fast$PANEL_70[i] > 0 & fast$PANEL_70[i] <= 14){fast$NEWINVYR[i] <- 2024} # On time: ==14 P70's
}

table(fast$INVYR) # Original INVYR distribution
table(fast$NEWINVYR) # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(fast$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(fast[fast$NEWINVYR==invyrs[i],])/nrow(fast)
}

round(props_measured*100,2) # 27.56 / 24.36 / 14.15 / 14.18 / 19.75

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#fast$INVDIFF <- fast$NEWINVYR - fast$INVYR_0
fast$INVDIFF <- fast$NEWINVYR - fast$INVYR

table(fast$INVDIFF,useNA = "always") # So in Maine under this case 

nsim <- 1000

fast_results <- data.frame(matrix(NA, nrow = nsim, ncol = 4))
colnames(fast_results) <- c("total_weibull","se_pct_weibull","total_empirical","se_pct_empirical")

for(i in 1:nsim){
  set.seed(i)
  # Weibull growth
  growths_1 <- rweibull(n=nrow(fast), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  fast$growths_1 <- growths_1

  fast$NEWVOL_1 <- 0
  fast$NEWVOL_1 <- ifelse(fast$PLOT_STATUS_CD==1,
                          fast$VOLCFNET_plot + (fast$INVDIFF * fast$growths_1),
                          0)
  fast$NEWVOL_1 <- ifelse(fast$NEWVOL_1 < 0, 0, fast$NEWVOL_1)
  
  # Empirical CDF growth: SRSwR
  growths_2 <- sample(changes, size = nrow(fast), replace = TRUE)
  fast$growths_2 <- growths_2
  fast$NEWVOL_2 <- 0
  fast$NEWVOL_2 <- ifelse(fast$PLOT_STATUS_CD==1,
                          fast$VOLCFNET_plot + (fast$INVDIFF * fast$growths_2),
                          0)
  fast$NEWVOL_2 <- ifelse(fast$NEWVOL_2 < 0, 0, fast$NEWVOL_2)
  
  # Empirical CDF growth: SRSwoR
  # fast$NEWVOL_2 <- 0
  # for(j in 1:nrow(fast)){
  #   my_id <- round(runif(n=1, min = 1, max = max(changes_df$id)),0)
  #   my_growth <- changes_df$difs[changes_df$id==my_id]
  #   if(fast$PLOT_STATUS_CD[j]==1){fast$NEWVOL_2[j] <- fast$VOLCFNET_plot[j] + (fast$INVDIFF[j] * my_growth)} else {
  #     fast$NEWVOL_2[j] <- 0}
  # }
  # fast$NEWVOL_2 <- ifelse(fast$NEWVOL_2 < 0, 0, fast$NEWVOL_2)
  
  # Compare original volumes to new "grown" volumes
  # plot(density(plots_p70$VOLCFNET_plot))
  # lines(density(fast$NEWVOL),col="red")
  # summary(fast$VOLCFNET_plot)
  # summary(fast$NEWVOL)
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL_1 <- sum(fast$NEWVOL_1 * fast$EXPNS)
  NEW_ESTIMATED_TOTAL_2 <- sum(fast$NEWVOL_2 * fast$EXPNS)
  
  # Standard error estimator
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(cbind(fast$NEWVOL_1,
                               fast$NEWVOL_2),
                         by=list(ESTN_UNIT=fast$ESTN_UNIT,
                                 STRATUMCD=fast$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)-1] <- "VOLCFNET_eu_strat_se_1"
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se_2"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  
  # copy the list of estimation units for building estn unit level variances 
  pop_estn_unit_total_new <- pop_estn_unit_total
  pop_estn_unit_total_new$var_vol_new_1 <- 0
  pop_estn_unit_total_new$var_vol_new_2 <- 0
  
  # loop through the estn units
  for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
    # pull all strata in this estn unit
    strata_in_unit <- v_Yhd_plus_total_new[v_Yhd_plus_total_new$ESTN_UNIT==eu,]
    # get the W_h weights for each strata within the estimation unit
    strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
    # get the total p2 sample size in this estimation unit
    n <- sum(strata_in_unit$P2POINTCNT)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1_1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_1))
    part2_1 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_1)/n)
    part1_2 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se_2))
    part2_2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se_2)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new_1[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_1 + part2_1)/n
    pop_estn_unit_total_new$var_vol_new_2[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1_1 + part2_1)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new_1 <- sum(pop_estn_unit_total_new$var_vol_new_1*pop_estn_unit_total_new$AREA_USED^2)
  se_new_1 <- sqrt(total_var_new_1)
  total_var_new_2 <- sum(pop_estn_unit_total_new$var_vol_new_2*pop_estn_unit_total_new$AREA_USED^2)
  se_new_2 <- sqrt(total_var_new_2)
  
  #se_new / NEW_ESTIMATED_TOTAL * 100 # Compute new SE%
  
  fast_results$total_weibull[i] <- NEW_ESTIMATED_TOTAL_1
  fast_results$se_pct_weibull[i] <- se_new_1 / NEW_ESTIMATED_TOTAL_1 * 100
  fast_results$total_empirical[i] <- NEW_ESTIMATED_TOTAL_2
  fast_results$se_pct_empirical[i] <- se_new_2 / NEW_ESTIMATED_TOTAL_2 * 100
}

head(fast_results)

total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se <- sqrt(total_var)

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL, se / ESTIMATED_TOTAL * 100, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(1.22,1.245),xlim = c(ESTIMATED_TOTAL - (0.0043*ESTIMATED_TOTAL),ESTIMATED_TOTAL + (0.0043*ESTIMATED_TOTAL)),
     xaxt = "n", main = "Maine")
points(fast_results$total_empirical,fast_results$se_pct_empirical, pch=19, col = t_org)
points(fast_results$total_weibull,fast_results$se_pct_weibull, pch=19, col = t_grn)
points(slow_results$total_empirical,slow_results$se_pct_empirical, pch=19, col = t_org)
points(slow_results$total_weibull,slow_results$se_pct_weibull, pch=19, col = t_blk)


legend("bottomleft", 
       legend = c("Slow start Weibull", "Fast start Weibull", "Slow/Fast Empirical"), 
       col = c(t_blk, t_grn,t_org), 
       pch = 19, 
       title = "VAE Regimen")

x_vals <- ESTIMATED_TOTAL * (1 + seq(-0.005, 0.005, by = 0.001))
axis(side = 1, at = x_vals, labels = paste0(seq(-0.5, 0.5, 0.1), "%"))
abline(h = se / ESTIMATED_TOTAL * 100, v = ESTIMATED_TOTAL, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL,se / ESTIMATED_TOTAL * 100, col="red", pch = 19, cex = 1.5)
