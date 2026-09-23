# Initialize
source("00.initialize.R")
# Load fast/slow simulation results for Maine, Montana, and Alabama
load(file.path("data","incomplete_me.Rdata"))
load(file.path("data","incomplete_mt.Rdata"))
load(file.path("data","incomplete_al.Rdata"))

# make some figs
t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

resol <- 640
png(file.path("images","incomplete_three_states.png"),width=11,height=5, res=resol,units="in")
par(mfrow = c(1, 3))
# ME
x_bump <- 0.03
y_bump <- 0.25

plot(ESTIMATED_TOTAL_me, se_pct_me, col="red", pch = 19,
     ylab = "Standard Error (% of Estimated Total)", 
     xlab = "",
     ylim = c(se_pct_me - (0.5*y_bump), se_pct_me + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_me - (x_bump*ESTIMATED_TOTAL_me),ESTIMATED_TOTAL_me + (x_bump*ESTIMATED_TOTAL_me)),
     xaxt = "n",
     main="Maine")
points(incomplete_results_me$total_1,incomplete_results_me$se_pct_1, pch=19, col = t_blk) # Drop 1 P70/INVYR
points(incomplete_results_me$total_2,incomplete_results_me$se_pct_2, pch=19, col = t_grn) # Drop 2 P70/INVYR
points(incomplete_results_me$total_3,incomplete_results_me$se_pct_3, pch=19, col = t_org) # Drop 3 P70/INVYR

x_vals <- ESTIMATED_TOTAL_me * (1 + seq(-0.03, 0.03, by = 0.003))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-3.0, 3.0, .3),1), "%"))
abline(h = se_pct_me, v = ESTIMATED_TOTAL_me, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_me, se_pct_me, col="red", pch = 19)

legend("bottomleft", 
       legend = c("Original Estimate","Drop 1 P70", "Drop 2 P70s", "Drop 3 P70s"), 
       col = c("red",t_blk, t_grn, t_org), 
       pch = 19, 
       title = "VAE Regimen")

# MT
x_bump <- 0.05
y_bump <- 0.5

t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)

plot(ESTIMATED_TOTAL_mt, se_pct_mt, col="red", pch = 19,
     #ylab = "Standard Error (% of Estimated Total)", 
     ylab = "",
     xlab = "Percent Change in Estimated Total",
     ylim = c(se_pct_mt - (0.5*y_bump), se_pct_mt + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_mt - (x_bump*ESTIMATED_TOTAL_mt),ESTIMATED_TOTAL_mt + (x_bump*ESTIMATED_TOTAL_mt)),
     xaxt = "n",
     main="Montana")
points(incomplete_results_mt$total_1,incomplete_results_mt$se_pct_1, pch=19, col = t_blk) # Drop 1 P70/INVYR
points(incomplete_results_mt$total_2,incomplete_results_mt$se_pct_2, pch=19, col = t_grn) # Drop 2 P70/INVYR
points(incomplete_results_mt$total_3,incomplete_results_mt$se_pct_3, pch=19, col = t_org) # Drop 3 P70/INVYR

x_vals <- ESTIMATED_TOTAL_mt * (1 + seq(-0.05, 0.05, by = 0.005))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-5.0, 5.0, 0.5),1), "%"))
abline(h = se_pct_mt, v = ESTIMATED_TOTAL_mt, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_mt, se_pct_mt, col="red", pch = 19)

# AL
x_bump <- 0.03
y_bump <- 0.3

plot(ESTIMATED_TOTAL_al, se_pct_al, col="red", pch = 19,
     # ylab = "Standard Error (% of Estimated Total)", 
     # xlab = "Percent Change in Estimated Total",
     ylab = "", 
     xlab = "",
     ylim = c(se_pct_al - (0.5*y_bump), se_pct_al + (y_bump)),
     xlim = c(ESTIMATED_TOTAL_al - (x_bump*ESTIMATED_TOTAL_al),ESTIMATED_TOTAL_al + (x_bump*ESTIMATED_TOTAL_al)),
     xaxt = "n",
     main="Alabama")
points(incomplete_results_al$total_1,incomplete_results_al$se_pct_1, pch=19, col = t_blk) # Drop 1 P70/INVYR
points(incomplete_results_al$total_2,incomplete_results_al$se_pct_2, pch=19, col = t_grn) # Drop 2 P70/INVYR
points(incomplete_results_al$total_3,incomplete_results_al$se_pct_3, pch=19, col = t_org) # Drop 3 P70/INVYR

x_vals <- ESTIMATED_TOTAL_al * (1 + seq(-0.03, 0.03, by = 0.003))
axis(side = 1, at = x_vals, labels = paste0(round(seq(-3.0, 3.0, .3),1), "%"))
abline(h = se_pct_al, v = ESTIMATED_TOTAL_al, col = "lightgray", lty = 3)
points(ESTIMATED_TOTAL_al, se_pct_al, col="red", pch = 19)

# legend("bottomleft", 
#        legend = c("Original Estimate","Drop 1 P70", "Drop 2 P70s", "Drop 3 P70s"), 
#        col = c("red",t_blk, t_grn, t_org), 
#        pch = 19, 
#        title = "VAE Regimen")

dev.off()
