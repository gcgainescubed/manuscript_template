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
abline(v=0,col="gray",lty=3)

mean(rweibull(n=100000,shape = fit[1]$estimate[1],
              scale = fit[1]$estimate[2]))-shift

### SLOW START SCENARIO
# Here, we simulate a slow start (Some panel 70's pushed forward from first few P2 panels) scenario,
# 
plots_p70$panel <- (plots_p70$SUBPANEL-1)*5+plots_p70$P2PANEL # Convert panel/subpanel to 10-year panels
slow <- plots_p70

# Need to check to see if Panel 1 = Panel 70 1:7; i.e., if Panel 70s were synced with  
# Panel 1 in Montana. If not, stagger the new inventory years appropriately...
table(plots_p70$INVYR) # Original INVYR distribution
table(ceiling(plots_p70$PANEL_70/7)) # How many P70's per panel in 10 year cycle? Should be same as plots/invyr?
table(plots_p70$INVYR,plots_p70$PANEL_70) # In MT, first INVYR in this cycle (2013) = P70 1:7, etc. 

# What proportion of plots are measured in each INVYR?
invyrs <- unique(sort(plots_p70$INVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(plots_p70[plots_p70$INVYR==invyrs[i],])/nrow(plots_p70)
}

round(props_measured*100,2) # 9.74 / 10.21 / 10.19 / 9.87 / 10.11 / 9.58 / 10.15 / 9.98 / 10.17 / 9.99

slow$NEWINVYR <- 0
for(i in 1:nrow(slow)){
  if(slow$PANEL_70[i] > 0 & slow$PANEL_70[i] <= 5){slow$NEWINVYR[i] <- 2013} else # Slow: 5 P70's
    if(slow$PANEL_70[i] > 5 & slow$PANEL_70[i] <= 10){slow$NEWINVYR[i] <- 2014} else # Slow: 5 P70's
      if(slow$PANEL_70[i] > 10 & slow$PANEL_70[i] <= 16){slow$NEWINVYR[i] <- 2015} else # Slow: 5 P70's
        if(slow$PANEL_70[i] > 16 & slow$PANEL_70[i] <= 21){slow$NEWINVYR[i] <- 2016} else # Slow: 5 P70's
          if(slow$PANEL_70[i] > 21 & slow$PANEL_70[i] <= 29){slow$NEWINVYR[i] <- 2017} else # Fast: 9 P70's
            if(slow$PANEL_70[i] > 29 & slow$PANEL_70[i] <= 39){slow$NEWINVYR[i] <- 2018} else # Fast: 9 P70's
              if(slow$PANEL_70[i] > 39 & slow$PANEL_70[i] <= 48){slow$NEWINVYR[i] <- 2019} else # Fast: 8 P70's
                if(slow$PANEL_70[i] > 48 & slow$PANEL_70[i] <= 56){slow$NEWINVYR[i] <- 2020} else # Fast: 8 P70's
                  if(slow$PANEL_70[i] > 56 & slow$PANEL_70[i] <= 63){slow$NEWINVYR[i] <- 2021} else # On time: 7 P70's
                    if(slow$PANEL_70[i] > 63 & slow$PANEL_70[i] <= 70){slow$NEWINVYR[i] <- 2022} # On time: 7 P70's
}

table(slow$INVYR) # Original INVYR distribution
table(slow$NEWINVYR) # New INVYR distribution

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(slow$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(slow[slow$NEWINVYR==invyrs[i],])/nrow(slow)
}

round(props_measured*100,2) # 6.94 / 7.19 / 8.71 / 7.29 / 11.33 / 14.07 / 12.87 / 11.43 / 10.17 / 9.99

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#slow$INVDIFF <- slow$NEWINVYR - slow$INVYR_0
slow$INVDIFF <- slow$NEWINVYR - slow$INVYR

table(slow$INVDIFF,useNA = "always")  
table(slow$INVYR[slow$INVDIFF==2],slow$NEWINVYR[slow$INVDIFF==2])

nsim <- 1000

slow_results <- data.frame(matrix(NA, nrow = nsim, ncol = 2))
colnames(slow_results) <- c("total","se_pct")

for(i in 1:nsim){
  set.seed(i)
  growths <- rweibull(n=nrow(slow), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  slow$NEWVOL <- 0
  
  slow$NEWVOL <- ifelse(slow$PLOT_STATUS_CD==1,
                        slow$VOLCFNET_plot + (slow$INVDIFF * growths),
                        0)
  slow$NEWVOL <- ifelse(slow$NEWVOL < 0, 0, slow$NEWVOL)
  
  # Compare original volumes to new "grown" volumes
  # plot(density(plots_p70$VOLCFNET_plot))
  # lines(density(slow$NEWVOL),col="red")
  # summary(slow$VOLCFNET_plot)
  # summary(slow$NEWVOL)
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL <- sum(slow$NEWVOL * slow$EXPNS)
  
  # Standard Error:
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(slow$NEWVOL,
                         by=list(ESTN_UNIT=slow$ESTN_UNIT,
                                 STRATUMCD=slow$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  
  # copy the list of estimation units for building estn unit level variances 
  pop_estn_unit_total_new <- pop_estn_unit_total
  pop_estn_unit_total_new$var_vol_new <- 0
  # loop through the estn units
  for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
    # pull all strata in this estn unit
    strata_in_unit <- v_Yhd_plus_total_new[v_Yhd_plus_total_new$ESTN_UNIT==eu,]
    # get the W_h weights for each strata within the estimation unit
    strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
    # get the total p2 sample size in this estimation unit
    n <- sum(strata_in_unit$P2POINTCNT)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se))
    part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1 + part2)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new <- sum(pop_estn_unit_total_new$var_vol_new*pop_estn_unit_total_new$AREA_USED^2)
  se_new <- sqrt(total_var_new)
  
  #se_new / NEW_ESTIMATED_TOTAL * 100 # Compute new SE%
  
  slow_results$total[i] <- NEW_ESTIMATED_TOTAL
  slow_results$se_pct[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
}

head(slow_results)

#plot(slow_results$total,slow_results$se_pct)

### FAST START THEN SLOW
fast <- plots_p70

table(fast$INVYR,fast$PANEL_70)

# Fast 1: 6508 plots measured 1 year early, 8727 plots measured on time
# This one shifts simulated totals quite high, I guess because the slow down from 2017-2020 overwhelms the fast start?
# But there are no positive INVDIFFs (plots measured late), meaning none are actually intentionally grown forward...
# The earliest a plot is measured is 1 year (INVDIFF = 1), and ~43% of plots are measured early,
# Which means we'd expect the simulated totals to get smaller, as happens in Maine under fast start.
# But, if you plot the fitted Weibull, there is a pretty high probability of a negative random growth draw,
# which gets multiplied by a negative number (INVDIFF) in the fast start case for plots measured early, 
# and this number gets added to the original volume, which would then actually apply positive backward growth?
# Given what we know about growth and mortality is this realistic enough?

fast$NEWINVYR <- 0
for(i in 1:nrow(fast)){
  if(fast$PANEL_70[i] > 0 & fast$PANEL_70[i] <= 9){fast$NEWINVYR[i] <- 2013} else # Fast: 9 P70's
    if(fast$PANEL_70[i] > 9 & fast$PANEL_70[i] <= 19){fast$NEWINVYR[i] <- 2014} else # Fast: 10 P70's
      if(fast$PANEL_70[i] > 19 & fast$PANEL_70[i] <= 28){fast$NEWINVYR[i] <- 2015} else # Fast: 9 P70's
        if(fast$PANEL_70[i] > 28 & fast$PANEL_70[i] <= 35){fast$NEWINVYR[i] <- 2016} else # On time: 7 P70's
          if(fast$PANEL_70[i] > 35 & fast$PANEL_70[i] <= 40){fast$NEWINVYR[i] <- 2017} else # Slow: 5 P70's
            if(fast$PANEL_70[i] > 40 & fast$PANEL_70[i] <= 45){fast$NEWINVYR[i] <- 2018} else # Slow: 5 P70's
              if(fast$PANEL_70[i] > 45 & fast$PANEL_70[i] <= 50){fast$NEWINVYR[i] <- 2019} else # Slow: 5 P70's
                if(fast$PANEL_70[i] > 50 & fast$PANEL_70[i] <= 56){fast$NEWINVYR[i] <- 2020} else # Slow: 6 P70's
                  if(fast$PANEL_70[i] > 56 & fast$PANEL_70[i] <= 63){fast$NEWINVYR[i] <- 2021} else # On time: 7 P70's
                    if(fast$PANEL_70[i] > 63 & fast$PANEL_70[i] <= 70){fast$NEWINVYR[i] <- 2022} # On time: 7 P70's
}

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(fast$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(fast[fast$NEWINVYR==invyrs[i],])/nrow(fast)
}

round(props_measured*100,2) # 12.67 / 14.51 / 12.83 / 10.11 / 6.80 / 7.17 / 7.13 / 8.61 / 10.17 / 9.99

# Fast 2: 3283 plots measured 1 year early, 11287 plots measured on time, 665 plots measured 1 year late
# This one moderates that behavior by a "less fast" start, and by not slowing down as much mid-late cycle,
# and by having 1 more speed-up in penultimate INVYR;
# But, it still slows down enough to push some plots forward from their original INVYRS (INVDIFF==1):
fast$NEWINVYR <- 0
for(i in 1:nrow(fast)){
  if(fast$PANEL_70[i] > 0 & fast$PANEL_70[i] <= 9){fast$NEWINVYR[i] <- 2013} else # Fast: 9 P70's
    if(fast$PANEL_70[i] > 9 & fast$PANEL_70[i] <= 17){fast$NEWINVYR[i] <- 2014} else # Fast: 8 P70's
      if(fast$PANEL_70[i] > 17 & fast$PANEL_70[i] <= 25){fast$NEWINVYR[i] <- 2015} else # Fast: 8 P70's
        if(fast$PANEL_70[i] > 25 & fast$PANEL_70[i] <= 32){fast$NEWINVYR[i] <- 2016} else # On time: 7 P70's
          if(fast$PANEL_70[i] > 32 & fast$PANEL_70[i] <= 37){fast$NEWINVYR[i] <- 2017} else # Slow: 5 P70's
            if(fast$PANEL_70[i] > 37 & fast$PANEL_70[i] <= 42){fast$NEWINVYR[i] <- 2018} else # Slow: 5 P70's
              if(fast$PANEL_70[i] > 42 & fast$PANEL_70[i] <= 48){fast$NEWINVYR[i] <- 2019} else # Slow: 6 P70's
                if(fast$PANEL_70[i] > 48 & fast$PANEL_70[i] <= 54){fast$NEWINVYR[i] <- 2020} else # Slow: 6 P70's
                  if(fast$PANEL_70[i] > 54 & fast$PANEL_70[i] <= 63){fast$NEWINVYR[i] <- 2021} else # Fast: 9 P70's
                    if(fast$PANEL_70[i] > 63 & fast$PANEL_70[i] <= 70){fast$NEWINVYR[i] <- 2022} # On time: 7 P70's
}

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(fast$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(fast[fast$NEWINVYR==invyrs[i],])/nrow(fast)
}

round(props_measured*100,2) # 12.67 / 11.66 / 11.47 / 10.03 / 7.02 / 6.85 / 8.71 / 8.51 / 13.09 / 9.99

# # Fast 3: 5433 plots measured 1 year late, 9802 measured on time 
# # This one...
# fast$NEWINVYR <- 0
# for(i in 1:nrow(fast)){
#   if(fast$PANEL_70[i] > 0 & fast$PANEL_70[i] <= 9){fast$NEWINVYR[i] <- 2013} else # Fast: 9 P70's
#     if(fast$PANEL_70[i] > 9 & fast$PANEL_70[i] <= 17){fast$NEWINVYR[i] <- 2014} else # Fast: 8 P70's
#       if(fast$PANEL_70[i] > 17 & fast$PANEL_70[i] <= 26){fast$NEWINVYR[i] <- 2015} else # Fast: 9 P70's
#         if(fast$PANEL_70[i] > 26 & fast$PANEL_70[i] <= 33){fast$NEWINVYR[i] <- 2016} else # On time: 7 P70's
#           if(fast$PANEL_70[i] > 33 & fast$PANEL_70[i] <= 39){fast$NEWINVYR[i] <- 2017} else # Slow: 6 P70's
#             if(fast$PANEL_70[i] > 39 & fast$PANEL_70[i] <= 45){fast$NEWINVYR[i] <- 2018} else # Slow: 6 P70's
#               if(fast$PANEL_70[i] > 45 & fast$PANEL_70[i] <= 51){fast$NEWINVYR[i] <- 2019} else # Slow: 6 P70's
#                 if(fast$PANEL_70[i] > 51 & fast$PANEL_70[i] <= 57){fast$NEWINVYR[i] <- 2020} else # Slow: 6 P70's
#                   if(fast$PANEL_70[i] > 57 & fast$PANEL_70[i] <= 63){fast$NEWINVYR[i] <- 2021} else # slow: 6 P70's
#                     if(fast$PANEL_70[i] > 63 & fast$PANEL_70[i] <= 70){fast$NEWINVYR[i] <- 2022} # On time: 7 P70's
# }
# 
# # Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
# invyrs <- unique(sort(fast$NEWINVYR))
# props_measured <- vector(mode = "numeric", length = length(invyrs))
# for(i in 1:length(invyrs)){
#   props_measured[i] <- nrow(fast[fast$NEWINVYR==invyrs[i],])/nrow(fast)
# }
# 
# round(props_measured*100,2) # 12.67 / 11.66 / 12.85 / 10.04 / 8.32 / 8.55 / 8.57 / 8.65 / 8.70 / 9.99

# For plots getting shifted forward, compute difference in years between original INVYR and new INVYR
#fast$INVDIFF <- fast$NEWINVYR - fast$INVYR_0
fast$INVDIFF <- fast$NEWINVYR - fast$INVYR

table(fast$INVDIFF,useNA = "always") # So in Maine under this case 

nsim <- 1000

fast_results <- data.frame(matrix(NA, nrow = nsim, ncol = 2))
colnames(fast_results) <- c("total","se_pct")

#i <- 1

for(i in 1:nsim){
  set.seed(i)
  growths <- rweibull(n=nrow(fast), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  fast$NEWVOL <- 0
  
  fast$NEWVOL <- ifelse(fast$PLOT_STATUS_CD==1,
                        fast$VOLCFNET_plot + (fast$INVDIFF * growths),
                        0)
  fast$NEWVOL <- ifelse(fast$NEWVOL < 0, 0, fast$NEWVOL)
  
  # Plot to check some stuff
  # plot(density(changes)) # observed, non-NA annual changes from T1 (previous) to T2 (current)
  # lines(density(rweibull(n=1000000,
  #                        shape = fit[1]$estimate[1],
  #                        scale = fit[1]$estimate[2])-shift),
  #       col="red") # The weibull fitted to these changes
  # lines(density(growths),col="orange") # The random growth vector from this particular iteration
  # lines(density(fast$NEWVOL),col="blue") # The distribution of new volumes (Which are currently forced to be >=0)
  # lines(density(fast$VOLCFNET_plot[!is.na(fast$VOLCFNET_plot)]),col="lightblue") # Distribution of original volumes
  # 
  # abline(v=0,col="gray",lty=3)
  # summary(fast$NEWVOL)
  # summary(fast$VOLCFNET_plot)
  # summary(growths)
  
  # Plot original vs new volumes...
  # plot(fast$VOLCFNET_plot,fast$NEWVOL,xlab="Original Volume",ylab="New Volume")
  # abline(1,1,col="red")
   
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL <- sum(fast$NEWVOL * fast$EXPNS)
  
  # Compute the ith new estimated variance of estimated total in the simulation
  
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(fast$NEWVOL,
                         by=list(ESTN_UNIT=fast$ESTN_UNIT,
                                 STRATUMCD=fast$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  
  # copy the list of estimation units for building estn unit level variances 
  pop_estn_unit_total_new <- pop_estn_unit_total
  pop_estn_unit_total_new$var_vol_new <- 0
  # loop through the estn units
  for (eu in unique(pop_estn_unit_total$ESTN_UNIT)){
    # pull all strata in this estn unit
    strata_in_unit <- v_Yhd_plus_total_new[v_Yhd_plus_total_new$ESTN_UNIT==eu,]
    # get the W_h weights for each strata within the estimation unit
    strata_in_unit$W_h <- strata_in_unit$P1POINTCNT/sum(strata_in_unit$P1POINTCNT)
    # get the total p2 sample size in this estimation unit
    n <- sum(strata_in_unit$P2POINTCNT)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1 <- sum(with(strata_in_unit,W_h*P2POINTCNT*VOLCFNET_eu_strat_se))
    part2 <- sum(with(strata_in_unit,(1-W_h)*P2POINTCNT*VOLCFNET_eu_strat_se)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1 + part2)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new <- sum(pop_estn_unit_total_new$var_vol_new*pop_estn_unit_total_new$AREA_USED^2)
  se_new <- sqrt(total_var_new)
  
  #se_new / NEW_ESTIMATED_TOTAL * 100 # Compute new SE%
  
  fast_results$total[i] <- NEW_ESTIMATED_TOTAL
  fast_results$se_pct[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
}

head(fast_results)
range(fast_results$total)
range(slow_results$total)
range(fast_results$se_pct)

total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se <- sqrt(total_var)

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)
t_gry <- rgb(0.5, 0.5, 0.5, alpha = 0.4)
#t_prp <- rgb(160, 32, 240, alpha = 125, maxColorValue = 255)
#t_blu <- rgb(0, 0, 255, alpha = 125, maxColorValue = 255)

x_bump <- 0.01 # Fast 1
y_bump <- 0.02

#x_bump <- 0.02 # Fast 2 
#y_bump <- 0.04
  
plot(ESTIMATED_TOTAL, se / ESTIMATED_TOTAL * 100, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     #ylim = c(1.22,1.24),xlim = c(ESTIMATED_TOTAL - 100000000,ESTIMATED_TOTAL + 100000000))
     ylim = c(se / ESTIMATED_TOTAL * 100 - y_bump, se / ESTIMATED_TOTAL * 100 + y_bump),
     xlim = c(ESTIMATED_TOTAL - (x_bump*ESTIMATED_TOTAL),ESTIMATED_TOTAL + (x_bump*ESTIMATED_TOTAL)),
     xaxt = "n",
     main="Montana")
abline(h = se / ESTIMATED_TOTAL * 100, v = ESTIMATED_TOTAL, col = "lightgray", lty = 3)
points(slow_results$total,slow_results$se_pct, pch=19, col = t_blk)
points(fast_results$total,fast_results$se_pct, pch=19, col = t_grn) # Fast 1
points(fast_results$total,fast_results$se_pct, pch=19, col = t_gry) # Fast 2
#points(fast_results$total,fast_results$se_pct, pch=19, col = t_prp) # Fast 3

points(ESTIMATED_TOTAL, se / ESTIMATED_TOTAL * 100, col="red", pch = 19,)


x_vals <- ESTIMATED_TOTAL * (1 + seq(-0.04, 0.04, by = 0.002))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-4.0, 4.0, 0.2),1), "%"))

legend("bottomleft", 
       legend = c("Original Estimate","Slow start", "Fast Start 1", "Fast Start 2"), 
       col = c("red",t_blk, t_grn, t_gry), 
       pch = 19, 
       title = "VAE Regimen")


