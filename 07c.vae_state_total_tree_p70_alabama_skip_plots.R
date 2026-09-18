# Initialize
source("00.initialize.R")
# Load plots_p70 (combined previous and current cycle data) for Maine
load(file.path("data","plots_p70_al.Rdata"))
# Load current cycle data for Maine
load(file.path("data","total_vol_al.Rdata"))

### Dropped Panel Scenario
# Here, we simulate a scenario where a fixed number of Panel 70's per Panel are skipped altogether 
incomplete <- plots_p70

# NEED TO ACOUNT FOR THE FACT THAT 2020 INVYR WAS NOT PANEL70 1-14, 2020 STARTED WITH P70=15 as the following shows:
table(plots_p70$INVYR,plots_p70$PANEL_70)
table(plots_p70$P2PANEL,plots_p70$PANEL_70) # P70's are aligned with P2 Panels, not necessarily INVYRS at beginning of cycle...

# Set number of panel 70s per INVYR you want to drop 
p70s_skipped <- 1:3
# Create vector of unique inventory years 
invyrs <- unique(sort(incomplete$INVYR))
# Create empty list
p70_by_invyr <- vector(mode = "list", length = length(invyrs))
# Fill that list with the ranges of Panel 70 values for each inventory year
for(i in 1:length(invyrs)){
  p70_by_invyr[[i]] <- range(incomplete$PANEL_70[incomplete$INVYR==invyrs[i]])
}

nsim <- 1000

incomplete_results <- data.frame(matrix(NA, nrow = nsim, ncol = 6))
colnames(incomplete_results) <- c("total_1","se_pct_1", "total_2","se_pct_2", "total_3","se_pct_3")

#i <- 1

for(i in 1:nsim){
  set.seed(i)
  # For sim i, loop through the different numbers of P70s/panel dropped, specified above
  for(skip in p70s_skipped){
  # Create another empty list 
  p70s_to_drop <- vector(mode = "list",length = length(invyrs))
  # Fill that list with vectors of uniform random P70's to drop for each INVYR; each vector is of length 'yrs_skipped'  
  for(j in 1:length(p70s_to_drop)){
    p70s_to_drop[[j]] <- round(runif(n = skip, min = p70_by_invyr[[j]][1], max = p70_by_invyr[[j]][2]))
  }
  
  # Create a new data frame where plots with the random Panel 70 values selected above are dropped
  incomplete2 <- incomplete
  incomplete2$keep <- 0 + !(incomplete$PANEL_70%in%unlist(p70s_to_drop))
  incomplete2$all <- 1
  
  # Count the number of plots by ESTN_UNIT X STRATUMCD
  incomplete_agg <- aggregate(incomplete2[,c("keep","all")],
                              by=list(ESTN_UNIT=incomplete2$ESTN_UNIT,
                              STRATUMCD=incomplete2$STRATUMCD),
                              FUN=sum) 
  # merge onto incomplete2
  incomplete3 <- merge(incomplete_agg,incomplete2,by=c("ESTN_UNIT","STRATUMCD"))
  # Compute new expansion factor scaled by the reduction in n from dropped P70s
  incomplete3$EXPNS2 <- incomplete3$EXPNS *(incomplete3$all.x/incomplete3$keep.x) 
  incomplete3 <- incomplete3[incomplete3$keep.y==1,] 
  
  # Compute the ith new estimated total in the simulation 
  NEW_ESTIMATED_TOTAL <- sum(incomplete3$VOLCFNET_plot * incomplete3$EXPNS2)
  
  # Standard Error
  # get within stratum standard errors [GB2 eq 4 on page 8]
  v_Yhd_new <- aggregate(incomplete3$VOLCFNET_plot,
                         by=list(ESTN_UNIT=incomplete3$ESTN_UNIT,
                                 STRATUMCD=incomplete3$STRATUMCD),
                         FUN=function(z){var(z)/length(z)}) 
  # note: var includes /(n-1), /n added via /length(z)
  
  colnames(v_Yhd_new)[ncol(v_Yhd_new)] <- "VOLCFNET_eu_strat_se"
  
  # add the stratum point/pixel count stuff to the latter
  v_Yhd_plus_total_new <- merge(v_Yhd_new,pop_stratum)
  v_Yhd_plus_total_new <- merge(v_Yhd_plus_total_new,incomplete_agg)
  
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
    n <- sum(strata_in_unit$keep)
    # implement GB2 equation 3 page 8 in two parts for this estimation unit
    part1 <- sum(with(strata_in_unit,W_h*keep*VOLCFNET_eu_strat_se))
    part2 <- sum(with(strata_in_unit,(1-W_h)*keep*VOLCFNET_eu_strat_se)/n)
    # stick the result on the  copied list of estimation units
    pop_estn_unit_total_new$var_vol_new[pop_estn_unit_total_new$ESTN_UNIT==eu] <- (part1 + part2)/n
  }
  
  # combine the estimation unit level variances together, using the area variable
  total_var_new <- sum(pop_estn_unit_total_new$var_vol_new*pop_estn_unit_total_new$AREA_USED^2)
  se_new <- sqrt(total_var_new)
  
  # Append results for current sim and p70s dropped to correct row/column
  if(skip==1){
    incomplete_results$total_1[i] <- NEW_ESTIMATED_TOTAL
    incomplete_results$se_pct_1[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
  } else if(skip==2){
    incomplete_results$total_2[i] <- NEW_ESTIMATED_TOTAL
    incomplete_results$se_pct_2[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
  } else if(skip==3){
    incomplete_results$total_3[i] <- NEW_ESTIMATED_TOTAL
    incomplete_results$se_pct_3[i] <- se_new / NEW_ESTIMATED_TOTAL * 100
  }
 }
}

head(incomplete_results)

total_var <- sum(pop_estn_unit_total$var_vol*pop_estn_unit_total$AREA_USED^2)
se_pct <- sqrt(total_var) / ESTIMATED_TOTAL * 100

x_bump <- 0.03
y_bump <- 0.3

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL, se_pct, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct - (y_bump), se_pct + (y_bump)),xlim = c(ESTIMATED_TOTAL - (x_bump*ESTIMATED_TOTAL),ESTIMATED_TOTAL + (x_bump*ESTIMATED_TOTAL)),
     xaxt = "n",
     main="Alabama")
points(incomplete_results$total_1,incomplete_results$se_pct_1, pch=19, col = t_blk) # Drop 1 P70/INVYR
points(incomplete_results$total_2,incomplete_results$se_pct_2, pch=19, col = t_grn) # Drop 2 P70/INVYR
points(incomplete_results$total_3,incomplete_results$se_pct_3, pch=19, col = t_org) # Drop 3 P70/INVYR

x_vals <- ESTIMATED_TOTAL * (1 + seq(-0.03, 0.03, by = 0.003))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-3.0, 3.0, .3),1), "%"))
abline(h = se_pct, v = ESTIMATED_TOTAL, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL, se_pct, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","Drop 1 P70", "Drop 2 P70s", "Drop 3 P70s"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

#plot(incomplete_results$total,incomplete_results$se_pct)
