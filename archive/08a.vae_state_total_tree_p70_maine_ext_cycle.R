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
abline(v=0,col="gray",lty=3)

### Panel Creep Scenario
# Here, we simulate a scenario where a fixed number of Panel 70's per Panel are pushed to the 
# following inventory year, including in the final year in the cycle, leading to an extended cycle length.
# Save new version of plot measurement dataframe
delayed <- plots_p70

# NEED TO ACOUNT FOR THE FACT THAT 2020 INVYR WAS NOT PANEL70 1-14, 2020 STARTED WITH P70=15 as the following shows:
table(plots_p70$INVYR,plots_p70$PANEL_70)
table(plots_p70$P2PANEL,plots_p70$PANEL_70) # P70's are aligned with P2 Panels, not necessarily INVYRS at beginning of cycle...

# What prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(delayed$INVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(delayed[delayed$INVYR==invyrs[i],])/nrow(delayed)
}

round(props_measured*100,2) # 20.38 / 20.07 / 19.95 / 19.84 / 19.75

# Set number of panel 70s per INVYR you want to push to the following INVYR (i.e., delay their measurement by 1 year) 
p70s_delayed <- 1
# Create vector of unique inventory years 
invyrs <- unique(sort(delayed$INVYR))
# Create empty list
p70_by_invyr <- vector(mode = "list", length = length(invyrs))
# Fill that list with vectors of Panel 70 values for each inventory year
for(i in 1:length(invyrs)){
  p70_by_invyr[[i]] <- unique(sort(delayed$PANEL_70[delayed$INVYR==invyrs[i]]))
}

# Create vector of the P70's from each INVYR to be shifted forward (measured late)
p70s_to_shift <- vector(mode = "numeric")
#i <- 1
for(i in 1:length(p70_by_invyr)){
  # Isolate vector of Panel 70s assigned to ith INVYR in the cycle 
  invyr_p70s <- unlist(p70_by_invyr[i]) 
  # Now isolate the last n = 'p70s_delayed' P70 values for each of those inventory years
  if (p70s_delayed == 1) {
    p70s_to_shift <- c(invyr_p70s[length(invyr_p70s)], p70s_to_shift)
  } else {
    p70s_to_shift <- c(invyr_p70s[(length(invyr_p70s) + 1 - p70s_delayed):(length(invyr_p70s))], p70s_to_shift)
  }
}

# Finally, make each of the plots assigned to the P70s identified above 1 year late
delayed$NEWINVYR <- ifelse(delayed$PANEL_70%in%p70s_to_shift, delayed$INVYR+1, delayed$INVYR)
delayed$INVDIFF <- delayed$NEWINVYR - delayed$INVYR
table(delayed$INVDIFF)

table(delayed$INVYR)
table(delayed$NEWINVYR)

# Under this VAE regimen, what prop/pct of plots are measured in each INVYR?
invyrs <- unique(sort(delayed$NEWINVYR))
props_measured <- vector(mode = "numeric", length = length(invyrs))
for(i in 1:length(invyrs)){
  props_measured[i] <- nrow(delayed[delayed$NEWINVYR==invyrs[i],])/nrow(delayed)
}

round(props_measured*100,2) 

# Realized pct's of plots measured in each INVYR in cycle: # 20.38 / 20.07 / 19.95 / 19.84 / 19.75
# When you delay 1 P70 per year: 18.87 / 20.21 / 19.87 / 19.95 / 19.75 / 1.34
# When you delay 2 P70s per year: 17.35 / 20.24 / 19.87 / 20.10 / 19.75 / 2.69
# When you delay 3 P70s per year: 15.92 / 20.27 / 19.90 / 19.98 / 19.87 / 4.06

nsim <- 1000

delayed_results <- data.frame(matrix(NA, nrow = nsim, ncol = 2))
colnames(delayed_results) <- c("total","se_pct")

for(i in 1:nsim){
  set.seed(i)
  growths <- rweibull(n=nrow(delayed), shape = fit[1]$estimate[1], scale = fit[1]$estimate[2]) - shift
  delayed$NEWVOL <- 0
  
  delayed$NEWVOL <- ifelse(delayed$PLOT_STATUS_CD==1,
                        delayed$VOLCFNET_plot + (delayed$INVDIFF * growths),
                        0)
  delayed$NEWVOL <- ifelse(delayed$NEWVOL < 0, 0, delayed$NEWVOL)
  
  # Compare original volumes to new "grown" volumes
  # plot(density(plots_p70$VOLCFNET_plot))
  # lines(density(delayed$NEWVOL),col="red")
  # summary(delayed$VOLCFNET_plot)
  # summary(delayed$NEWVOL)
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL <- sum(delayed$NEWVOL * delayed$EXPNS)
  
  # Standard Error
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(delayed$NEWVOL,
                         by=list(ESTN_UNIT=delayed$ESTN_UNIT,
                                 STRATUMCD=delayed$STRATUMCD),
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
  
  delayed_results$total[i] <- NEW_ESTIMATED_TOTAL
  delayed_results$se_pct[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
}

head(delayed_results)

total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se_pct <- sqrt(total_var) / ESTIMATED_TOTAL * 100

x_bump <- 0.01
y_bump <- 0.03

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL, se_pct, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct - (y_bump), se_pct + (y_bump)),xlim = c(ESTIMATED_TOTAL - (x_bump*ESTIMATED_TOTAL),ESTIMATED_TOTAL + (x_bump*ESTIMATED_TOTAL)),
     xaxt = "n",
     main = "Maine")
points(delayed_results$total,delayed_results$se_pct, pch=19, col = t_blk) # Drop 1 P70/INVYR
points(delayed_results$total,delayed_results$se_pct, pch=19, col = t_grn) # Drop 2 P70/INVYR
points(delayed_results$total,delayed_results$se_pct, pch=19, col = t_org) # Drop 3 P70/INVYR

x_vals <- ESTIMATED_TOTAL * (1 + seq(-0.01, 0.01, by = 0.002))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-1.0, 1.0, 0.2),1), "%"))
abline(h = se_pct, v = ESTIMATED_TOTAL, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL, se_pct, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","Shift 1 P70 forward", "Shift 2 P70s forward", "Shift 3 P70s forward"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

#plot(delayed_results$total,delayed_results$se_pct)