# Plot graph for latency to first entry into CZ
Latency_plot_facet <- ggplot(
  FLM_Data,
  aes(x = group, y = log_CS_lat, fill = timepoint)
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
    y = "Latency to First Entry into 
    the Conditioned Zone(log sec)"
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

Latency_plot_facet

# Significance annotations for Shy comparisons
# Baseline vs Post-Con 11 and Baseline vs Memory
latency_letters <- data.frame(
  personality = "Shy",
  xpos = c(
    0.62, 0.82, 1.02, 1.22, 1.42,   # Shy-Control
    1.62, 1.82, 2.02, 2.22, 2.42    # Shy-Treatment
  ),
  y = rep(6.25, 10),
  letter = c(
    "a", "ab", "ab", "b", "b",
    "a", "ab", "ab", "b", "b"
  )
)

Latency_plot_facet_letters <- Latency_plot_facet +
  geom_text(
    data = latency_letters,
    aes(x = xpos, y = y, label = letter),
    inherit.aes = FALSE,
    size = 3.0,
    fontface = "bold"
  )

Latency_plot_facet_letters

p2 <- Latency_plot_facet_letters +
  scale_y_continuous(
    limits = c(0, 7),
    expand = expansion(mult = c(0, 0.03))
  ) +
  theme(
    plot.margin = margin(t = 35, r = 35, b = 10, l = 35),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 8))
  )

p2_main <- ggdraw(p2) +
  # Personality main effect: Bold vs Shy
  draw_line(
    x = c(0.33, 0.33),   # left tick
    y = c(0.925, 0.942),
    linewidth = 0.6
  ) +
  draw_line(
    x = c(0.33, 0.54),   # top bar (narrower)
    y = c(0.942, 0.942),
    linewidth = 0.6
  ) +
  draw_line(
    x = c(0.54, 0.54),   # right tick
    y = c(0.925, 0.942),
    linewidth = 0.6
  ) +
  draw_label(
    "**",
    x = 0.435,
    y = 0.955,           # move label up
    size = 13,
    fontface = "bold"
  )
p2_main
ggsave(filename = "C:/Users/bjimm/Downloads/memory/Memory-Project/Figures/Latency to first into CZ.png", p2_main, width =6.6, height = 4, dpi = 600)
