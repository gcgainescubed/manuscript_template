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

# Try it another way

# Reshape using base R
library(ggplot2)
head(incomplete_results_me)

me_long <- reshape(
  incomplete_results_me, 
  #varying = list(c("total_slow", "total_fast"), c("se_slow", "se_fast")),
  varying = list(c("total_1", "total_2","total_3"), c("se_pct_1", "se_pct_2","se_pct_3")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("1", "2","3"),
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
head(incomplete_results_mt)

mt_long <- reshape(
  incomplete_results_mt, 
  varying = list(c("total_1", "total_2","total_3"), c("se_pct_1", "se_pct_2","se_pct_3")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("1", "2","3"),
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
head(incomplete_results_al)

al_long <- reshape(
  incomplete_results_al, 
  varying = list(c("total_1", "total_2","total_3"), c("se_pct_1", "se_pct_2","se_pct_3")),
  v.names = c("total", "se_pct"),
  timevar = "group",
  times = c("1", "2","3"),
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
panel_colors <- c("1" = "#00b4d8", "2" = "#ffb703", "3" = "#7209b7")

resol <- 640
png(file.path("images","incomplete_three_states_ggplot.png"),width=11,height=5, res=resol,units="in")

ggplot(all_states, aes(x = pct_change, y = se_pct)) +
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
    title = "Simulated Dropped Panel 70 Scenarios",
    subtitle = "Percent change in simulated net total cubic-foot volume estimates relative to actual estimate",
    x = "Percent Change in Total VOLCFNET Estimate (%)", 
    y = "Standard Error (% of Total)",
    color = "Number of Dropped P70's per Annual Panel",
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
