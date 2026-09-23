# Initialize
source("00.initialize.R")
# Load fast/slow simulation results for Maine, Montana, and Alabama
load(file.path("data","ext_cycle_me.Rdata"))
load(file.path("data","ext_cycle_mt.Rdata"))
load(file.path("data","ext_cycle_al.Rdata"))

# make some figs
t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

resol <- 640
png(file.path("images","ext_cycle_three_states.png"),width=11,height=5, res=resol,units="in")
par(mfrow = c(1, 3))
# ME
x_bump <- 0.08
y_bump <- 0.05

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL_me, se_pct_me, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct_me - (y_bump), se_pct_me + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_me - (x_bump*ESTIMATED_TOTAL_me),ESTIMATED_TOTAL_me + (x_bump*ESTIMATED_TOTAL_me)),
     xaxt = "n",
     main = "Maine")
points(ext_cycle_results_me$total_7,ext_cycle_results_me$se_pct_7, pch=19, col = t_blk) # 7 year cycle
points(ext_cycle_results_me$total_10,ext_cycle_results_me$se_pct_10, pch=19, col = t_grn) # 10 year cycle
points(ext_cycle_results_me$total_14,ext_cycle_results_me$se_pct_14, pch=19, col = t_org) # 14 year cycle

x_vals <- ESTIMATED_TOTAL_me * (1 + seq(-0.1, 0.1, by = 0.005))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-10.0, 10.0, 0.5),1), "%"))
abline(h = se_pct_me, v = ESTIMATED_TOTAL_me, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_me, se_pct_me, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","7 Year Cycle", "10 Year Cycle", "14 Year Cycle"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

# MT
x_bump <- 0.06
y_bump <- 0.05

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL_mt, se_pct_mt, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct_mt - (y_bump), se_pct_mt + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_mt - (x_bump*ESTIMATED_TOTAL_mt),ESTIMATED_TOTAL_mt + (x_bump*ESTIMATED_TOTAL_mt)),
     xaxt = "n",
     main = "Montana")
points(ext_cycle_results_mt$total_5,ext_cycle_results_mt$se_pct_5, pch=19, col = t_blk) # 7 year cycle
points(ext_cycle_results_mt$total_7,ext_cycle_results_mt$se_pct_7, pch=19, col = t_grn) # 7 year cycle
points(ext_cycle_results_mt$total_14,ext_cycle_results_mt$se_pct_14, pch=19, col = t_org) # 14 year cycle

x_vals <- ESTIMATED_TOTAL_mt * (1 + seq(-0.1, 0.1, by = 0.005))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-10.0, 10.0, 0.5),1), "%"))
abline(h = se_pct_mt, v = ESTIMATED_TOTAL_mt, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_mt, se_pct_mt, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","5 Year Cycle", "7 Year Cycle", "14 Year Cycle"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

# AL
x_bump <- 0.1
y_bump <- 0.05

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL_al, se_pct_al, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct_al - (y_bump), se_pct_al + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_al - (x_bump*ESTIMATED_TOTAL_al),ESTIMATED_TOTAL_al + (x_bump*ESTIMATED_TOTAL_al)),
     xaxt = "n",
     main = "Alabama")
points(ext_cycle_results_al$total_5,ext_cycle_results_al$se_pct_5, pch=19, col = t_blk) # 7 year cycle
points(ext_cycle_results_al$total_10,ext_cycle_results_al$se_pct_10, pch=19, col = t_grn) # 10 year cycle
points(ext_cycle_results_al$total_14,ext_cycle_results_al$se_pct_14, pch=19, col = t_org) # 14 year cycle

x_vals <- ESTIMATED_TOTAL_al * (1 + seq(-0.1, 0.1, by = 0.005))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-10.0, 10.0, 0.5),1), "%"))
abline(h = se_pct_al, v = ESTIMATED_TOTAL_al, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_al, se_pct_al, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","5 Year Cycle", "10 Year Cycle", "14 Year Cycle"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

dev.off()
