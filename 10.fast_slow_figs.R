# Initialize
library(ggplot2)
source("00.initialize.R")
# Load fast/slow simulation results for Maine, Montana, and Alabama
load(file.path("data","fast_slow_me.Rdata"))
load(file.path("data","fast_slow_mt.Rdata"))
load(file.path("data","fast_slow_al.Rdata"))

# # make some figs
# t_blk <- rgb(0, 0, 0, alpha = 128, maxColorValue = 255)
# t_grn <- rgb(0, 155, 0, alpha = 128, maxColorValue = 255)
# t_org <- rgb(255, 69, 0, alpha = 128, maxColorValue = 255)
# 
# resol <- 640
# png(file.path("images","fast_slow_three_states.png"),width=11,height=5, res=resol,units="in")
# par(mfrow = c(1, 3))
# # ME
# x_bump <- 0.006
# y_bump <- 0.02
# plot(ESTIMATED_TOTAL_me, se_me / ESTIMATED_TOTAL_me * 100, col="red", pch = 19,
#      ylab = "Standard Error (% of Estimated Total)", 
#      #xlab = "Percent Change in Estimated Total",
#      xlab="",
#      ylim = c(se_me / ESTIMATED_TOTAL_me * 100 - y_bump, se_me / ESTIMATED_TOTAL_me * 100 + y_bump),
#      xlim = c(ESTIMATED_TOTAL_me - (x_bump*ESTIMATED_TOTAL_me),ESTIMATED_TOTAL_me + (x_bump*ESTIMATED_TOTAL_me)),
#      xaxt = "n", main = "Maine")
# #points(fast_results$total_weibull,fast_results$se_pct_weibull, pch=19, col = t_grn)
# points(slow_results_me$total_empirical,slow_results_me$se_pct_empirical, pch=19, col = t_blk)
# points(fast_results_me$total_empirical,fast_results_me$se_pct_empirical, pch=19, col = t_grn)
# #points(slow_results$total_weibull,slow_results$se_pct_weibull, pch=19, col = t_blk)
# x_vals <- ESTIMATED_TOTAL_me * (1 + seq(-0.04, 0.04, by = 0.002))
# axis(side = 1, at = x_vals, labels = paste0(round(seq(-4.0, 4.0, 0.2),1), "%"))
# abline(h = se_me / ESTIMATED_TOTAL_me * 100, v = ESTIMATED_TOTAL_me, col = "lightgray", lty = 3)
# points(ESTIMATED_TOTAL_me, se_me / ESTIMATED_TOTAL_me * 100, col="red", pch = 19, cex = 1.5)
# 
# legend("bottomleft",
#        legend = c("Original Estimate","Slow Start", "Fast Start"),
#        col = c("red",t_blk, t_grn),
#        pch = 19,
#        title = "VAE Regimen")
# 
# # MT
# x_bump <- 0.006 # Fast 1
# y_bump <- 0.02
# 
# plot(ESTIMATED_TOTAL_mt, se_mt / ESTIMATED_TOTAL_mt * 100, col="red", pch = 19,
#      #ylab = "Standard Error (% of Estimated Total)", 
#      ylab="",
#      xlab = "Percent Change in Estimated Total",
#      ylim = c(se_mt / ESTIMATED_TOTAL_mt * 100 - y_bump, se_mt / ESTIMATED_TOTAL_mt * 100 + y_bump),
#      xlim = c(ESTIMATED_TOTAL_mt - (x_bump*ESTIMATED_TOTAL_mt),ESTIMATED_TOTAL_mt + (x_bump*ESTIMATED_TOTAL_mt)),
#      xaxt = "n",
#      main="Montana")
# #points(slow_results$total_weibull,slow_results$se_pct_weibull, pch=19, col = t_blk)
# #points(fast_results$total_weibull,fast_results$se_pct_weibull, pch=19, col = t_grn) # Fast 1 Weibull
# points(slow_results_mt$total_empirical,slow_results_mt$se_pct_empirical, pch=19, col = t_blk)
# points(fast_results_mt$total_empirical,fast_results_mt$se_pct_empirical, pch=19, col = t_grn) # Fast 1 Emp
# x_vals <- ESTIMATED_TOTAL_mt * (1 + seq(-0.04, 0.04, by = 0.002))
# axis(side = 1, at = x_vals, labels = paste0(round(seq(-4.0, 4.0, 0.2),1), "%"))
# abline(h = se_mt / ESTIMATED_TOTAL_mt * 100, v = ESTIMATED_TOTAL_mt, col = "lightgray", lty = 3)
# points(ESTIMATED_TOTAL_mt, se_mt / ESTIMATED_TOTAL_mt * 100, col="red", pch = 19,)
# 
# # AL
# x_bump <- 0.015
# y_bump <- 0.03
# 
# plot(ESTIMATED_TOTAL_al, se_pct_al, col="red", pch = 19,
#      #ylab = "Standard Error (% of Estimated Total)", 
#      #xlab = "Percent Change in Estimated Total",
#      ylab="",
#      xlab="",
#      ylim = c(se_pct_al - (y_bump), se_pct_al + (y_bump)),
#      xlim = c(ESTIMATED_TOTAL_al - (x_bump*ESTIMATED_TOTAL_al),ESTIMATED_TOTAL_al + (x_bump*ESTIMATED_TOTAL_al)),
#      xaxt = "n",
#      main="Alabama")
# 
# points(slow_results_al$total_empirical,slow_results_al$se_pct_empirical, pch=19, col = t_blk)
# points(fast_results_al$total_empirical,fast_results_al$se_pct_empirical, pch=19, col = t_grn)
# #points(fast_results$total_weibull,fast_results$se_pct_weibull, pch=19, col = t_grn)
# #points(slow_results$total_weibull,slow_results$se_pct_weibull, pch=19, col = t_blk)
# x_vals <- ESTIMATED_TOTAL_al * (1 + seq(-0.015, 0.015, by = 0.005))
# axis(side = 1, at = x_vals, labels = paste0(seq(-1.5, 1.5, 0.5), "%"))
# abline(h = se_pct_al, v = ESTIMATED_TOTAL_al, col = "lightgray", lty = 3)
# points(ESTIMATED_TOTAL_al,se_pct_al, col="red", pch = 19, cex = 1.5)
# 
# dev.off()

# Try it another way
# GGplot is asking me to reshape everything to be long with groups so do that for all three states.
slow_results_me <- slow_results_me[,c(3,4)]
slow_results_mt <- slow_results_mt[,c(3,4)]
slow_results_al <- slow_results_al[,c(3,4)]

fast_results_me <- fast_results_me[,c(3,4)]
fast_results_mt <- fast_results_mt[,c(3,4)]
fast_results_al <- fast_results_al[,c(3,4)]

# ME
me <- data.frame(total_slow = slow_results_me$total_empirical,
                 se_slow = slow_results_me$se_pct_empirical,
                 total_fast = fast_results_me$total_empirical,
                 se_fast = fast_results_me$se_pct_empirical)
head(me)

# Reshape using base R
me_long <- reshape(
  me, 
  varying = list(c("total_slow", "total_fast"), c("se_slow", "se_fast")),
  v.names = c("total", "se"),
  timevar = "group",
  times = c("slow", "fast"),
  direction = "long"
)

head(me_long)
me_long <- me_long[,c(1:3)]
# Clean up row names
row.names(me_long) <- NULL
me_long$state <- "ME"
head(me_long)

ggplot(me_long, aes(x = total, y = se, color = group)) +
  geom_point()

# MT
mt <- data.frame(total_slow = slow_results_mt$total_empirical,
                 se_slow = slow_results_mt$se_pct_empirical,
                 total_fast = fast_results_mt$total_empirical,
                 se_fast = fast_results_mt$se_pct_empirical)
head(mt)

# Reshape using base R
mt_long <- reshape(
  mt, 
  varying = list(c("total_slow", "total_fast"), c("se_slow", "se_fast")),
  v.names = c("total", "se"),
  timevar = "group",
  times = c("slow", "fast"),
  direction = "long"
)

head(mt_long)
mt_long <- mt_long[,c(1:3)]
# Clean up row names
row.names(mt_long) <- NULL
mt_long$state <- "MT"
head(mt_long)

ggplot(mt_long, aes(x = total, y = se, color = group)) +
  geom_point()

# AL
al <- data.frame(total_slow = slow_results_al$total_empirical,
                 se_slow = slow_results_al$se_pct_empirical,
                 total_fast = fast_results_al$total_empirical,
                 se_fast = fast_results_al$se_pct_empirical)
head(al)

# Reshape using base R
al_long <- reshape(
  al, 
  varying = list(c("total_slow", "total_fast"), c("se_slow", "se_fast")),
  v.names = c("total", "se"),
  timevar = "group",
  times = c("slow", "fast"),
  direction = "long"
)

head(al_long)
al_long <- al_long[,c(1:3)]
# Clean up row names
row.names(al_long) <- NULL
al_long$state <- "AL"
head(al_long)

ggplot(al_long, aes(x = total, y = se, color = group)) +
  geom_point()

all_states <- rbind(me_long,mt_long,al_long)

# Make state an ordered factor so they plot in order of cycle length (5 , 7, 10)?
state_order <- c("ME","AL","MT")
all_states$state_fac <- factor(all_states$state, levels = state_order)

# Now do some random ass ai internet code
library(tibble)
library(patchwork)

# Create a named vector mapping state codes to your baseline R objects
state_baselines <- c(
  "ME" = ESTIMATED_TOTAL_me, 
  "MT" = ESTIMATED_TOTAL_mt, 
  "AL" = ESTIMATED_TOTAL_al
)

# Compute percent change: ((Current - Baseline) / Baseline) * 100
all_states$pct_change <- ((all_states$total - state_baselines[all_states$state]) / state_baselines[all_states$state]) * 100

all_states_baseline_dots <- data.frame(
  state_fac = c("ME", "AL", "MT"),
  pct_change = c(0, 0, 0), 
  se_pct = c(se_pct_me, se_pct_al, se_pct_mt)
)

all_states_baseline_dots$state_fac <- factor(all_states_baseline_dots$state, levels = state_order)

state_labels <- c(
  "ME" = "ME (5)",
  "AL" = "AL (7)",
  "MT" = "MT (10)"
)

#Define a modern color palette
# Build the modified modern 3-panel plot ---
panel_colors <- c("fast" = "#00b4d8", "slow" = "#ffb703")

resol <- 640
png(file.path("images","fast_slow_three_states_ggplot.png"),width=11,height=5, res=resol,units="in")

ggplot(all_states, aes(x = pct_change, y = se)) +
  # Main data layers (grouped color/fill mapped locally here)
  #geom_smooth(aes(color = group, fill = group), method = "lm", formula = y ~ x, alpha = 0.1, size = 1.2, se = TRUE) +
  geom_point(aes(color = group), alpha = 0.6, size = 2.5, stroke = 0) +
  
  # NEW: Modern red dot layer overlaying the baseline point per facet panel
  geom_point(
    data = all_states_baseline_dots, 
    aes(x = pct_change, y = se_pct), 
    color = "#e63946",       # Vibrant modern red
    size = 2,                # Made slightly larger to stand out
    shape = 16,              # Solid circle
    inherit.aes = FALSE      # Ignores main color/fill groupings
  ) +
  
  facet_wrap(~ state_fac, nrow = 1, labeller = as_labeller(state_labels)) +
  labs(
    title = "Simulated Fast Start / Slow Start Scenarios",
    subtitle = "Percent change in simulated net total cubic-foot volume estimates relative to actual estimate",
    x = "Percentage Change in Total (%)", 
    y = "Standard Error (% of total)",
    color = "Scenario",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0", size = 0.5),
    strip.background = element_rect(fill = "#f8f9fa", color = NA),
    strip.text = element_text(face = "bold", size = 12, color = "#212529", margin = margin(8, 8, 8, 8)),
    panel.spacing = unit(1.5, "lines"),
    plot.title = element_text(face = "bold", size = 16, color = "#212529", margin = margin(b = 6)),
    plot.subtitle = element_text(color = "#6c757d", size = 11, margin = margin(b = 20)),
    axis.title.x = element_text(margin = margin(t = 12), color = "#495057", face = "bold", size = 10),
    axis.title.y = element_text(margin = margin(r = 12), color = "#495057", face = "bold", size = 10),
    axis.text = element_text(color = "#6c757d", size = 9),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(face = "bold", size = 10, color = "#495057"),
    legend.text = element_text(size = 10, color = "#495057")
  ) +
  scale_color_manual(values = panel_colors) +
  scale_fill_manual(values = panel_colors)

dev.off()
