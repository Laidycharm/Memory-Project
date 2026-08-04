#Plot graph for time in CZ
time_colours <- c(
  "BAS"   = "#F07C7C",
  "PRO 1" = "#D98AC8",
  "PRO 2" = "#E67E22",
  "PRO 3" = "#6F2C8F",
  "PRO 4" = "#9E2A2B"
)

tp_labels <- c(
  "BAS"   = "Baseline",
  "PRO 1" = "Post-Con 3",
  "PRO 2" = "Post-Con 7",
  "PRO 3" = "Post-Con 11",
  "PRO 4" = "Memory "
)

FLM_plot_facet <- ggplot(
  FLM_Data,
  aes(x = group, y = log_CSTime, fill = timepoint)
) +
  stat_summary(
    fun = mean,
    geom = "col",
    position = position_dodge(0.85),
    width = 0.75
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(0.85),
    width = 0.18
  ) +
  geom_point(
    aes(group = timepoint),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.85
    ),
    size = 1.3,
    alpha = 0.55,
    colour = "black"
  ) +
  facet_wrap(~ personality, nrow = 1) +
  scale_fill_manual(
    values = time_colours,
    labels = tp_labels,
    name = "Trials"
  ) +
  scale_x_discrete(
    labels = c(
      "CONTROL" = "Control",
      "TREATMENT" = "Treatment"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 8),
    expand = expansion(mult = c(0, 0.03))
  ) +
  guides(
    fill = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  labs(
    x = "Group",
    y = "Time Spent in Conditioned Zone (log secs)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, colour = "black"),
    strip.text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.6, "cm")
  )

FLM_plot_facet

# Significance annotations for Shy-Treatment comparisons
sig_time <- data.frame(
  personality = "Shy",
  x1 = c(1.66, 1.66, 1.66, 1.66),   # Baseline bar in Treatment group
  x2 = c(1.83, 2.00, 2.17, 2.34),   # Post-Con 3, 7, 11, Memory bars
  y  = c(6.3, 6.75, 7.2, 7.65),
  label = c("***", "****", "****", "****")
)

FLM_plot_facet_sig <- FLM_plot_facet +
  geom_segment(
    data = sig_time,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = sig_time,
    aes(x = x1, xend = x1, y = y - 0.08, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = sig_time,
    aes(x = x2, xend = x2, y = y - 0.08, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_text(
    data = sig_time,
    aes(x = (x1 + x2) / 2, y = y + 0.12, label = label),
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  )

FLM_plot_facet_sig

library(cowplot)
p1 <- FLM_plot_facet_sig +
  scale_y_continuous(
    limits = c(0, 8.2),
    expand = expansion(mult = c(0, 0.03))
  ) +
  theme(
    plot.margin = margin(t = 35, r = 35, b = 10, l = 35),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 8))
  )

p1_main <- ggdraw(p1) +
  draw_line(
    x = c(0.25, 0.25),
    y = c(0.895, 0.915),
    linewidth = 0.6
  ) +
  draw_line(
    x = c(0.25, 0.60),
    y = c(0.915, 0.915),
    linewidth = 0.6
  ) +
  draw_line(
    x = c(0.60, 0.60),
    y = c(0.895, 0.915),
    linewidth = 0.6
  ) +
  draw_label(
    "**",
    x = 0.425,
    y = 0.935,
    size = 13,
    fontface = "bold"
  ) +

# Trials / conditioned day main effect vertical line
  draw_line(
    x = c(0.918, 0.918),   # move vertical line right
    y = c(0.30, 0.60),     # make line longer
    linewidth = 0.8
  ) +
  draw_label(
    "**",
    x = 0.935,             # move stars right of line
    y = 0.450,
    size = 13,
    fontface = "bold"
  )  
p1_main
ggsave(filename = "C:/Users/bjimm/Downloads/memory/Memory-Project/Figures/Time in CZ.png", p1_main, width =6.5, height = 4, dpi = 600)



