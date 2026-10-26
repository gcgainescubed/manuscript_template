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

# ggplot way 

# Reshape using base R
library(ggplot2)
head(ext_cycle_results_me)

me_long <- reshape(
  ext_cycle_results_me, 
  varying = list(c("total_7", "total_10","total_14"), c("se_pct_7", "se_pct_10","se_pct_14")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("7", "10","14"),
  direction = "long"
)

head(me_long)
me_long <- me_long[,c(1:3)]
# Clean up row names
row.names(me_long) <- NULL
me_long$state <- "ME"
head(me_long)

ggplot(me_long, aes(x = total, y = se_pct, color = group)) +
  geom_point()

# MT
head(ext_cycle_results_mt)

mt_long <- reshape(
  ext_cycle_results_mt, 
  varying = list(c("total_5", "total_7","total_14"), c("se_pct_5", "se_pct_7","se_pct_14")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("5", "7","14"),
  direction = "long"
)

head(mt_long)
mt_long <- mt_long[,c(1:3)]
# Clean up row names
row.names(mt_long) <- NULL
mt_long$state <- "MT"
head(mt_long)

ggplot(mt_long, aes(x = total, y = se_pct, color = group)) + # 16 missing values? 16 sims where there were too few obs in a stratum to compute \hat(v)?
  geom_point()

# AL
head(ext_cycle_results_al)

al_long <- reshape(
  ext_cycle_results_al, 
  varying = list(c("total_5", "total_10","total_14"), c("se_pct_5", "se_pct_10","se_pct_14")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("5", "10","14"),
  direction = "long"
)

head(al_long)
al_long <- al_long[,c(1:3)]
# Clean up row names
row.names(al_long) <- NULL
al_long$state <- "AL"
head(al_long)

ggplot(al_long, aes(x = total, y = se_pct, color = group)) + 
  geom_point()

all_states <- rbind(me_long,mt_long,al_long)

# Make state an ordered factor so they plot in order of cycle length (5 , 7, 10)?
state_order <- c("ME","AL","MT")
all_states$state_fac <- factor(all_states$state, levels = state_order)

# Make group an ordered factor so that dots plot in legend in increasing order (5 , 7, 10, 14)?
all_states$group_fac <- factor(all_states$group, levels = c("5", "7", "10", "14"))

# Now do some random ass ai internet code
library(tibble)
library(patchwork)

# Create a named vector mapping state codes to your baseline R objects
state_baselines <- c(
  "ME" = ESTIMATED_TOTAL_me, 
  "AL" = ESTIMATED_TOTAL_al,
  "MT" = ESTIMATED_TOTAL_mt
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
panel_colors <- c("5" = "#00b4d8", "7" = "#ffb703", "10" = "#7209b7", "14" = "#f72585")

resol <- 640
png(file.path("images","ext_cycle_three_states_ggplot.png"),width=11,height=5, res=resol,units="in")

ggplot(all_states, aes(x = pct_change, y = se_pct)) +
  # Main data layers (grouped color/fill mapped locally here)
  #geom_smooth(aes(color = group, fill = group), method = "lm", formula = y ~ x, alpha = 0.1, size = 1.2, se = TRUE) +
  geom_point(aes(color = group_fac), alpha = 0.6, size = 2.5, stroke = 0) +
  
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
    title = "Simulated Extended Cycle Length Scenarios",
    subtitle = "Percent change in simulated net total cubic-foot volume estimates relative to actual estimate",
    x = "Percent Change in Total VOLCFNET Estimate (%)", 
    y = "Standard Error (% of Total)",
    color = "Extended/Shortened Cycle Length (Years)"
    #fill = "Scenario"
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

