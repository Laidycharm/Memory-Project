# Plot graph for PC1 scores
PC1_plot_facet <- ggplot(
  Pca_scores,
  aes(x = group, y = PC1, fill = timepoint)
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
  geom_hline(
    yintercept = 0,
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  facet_grid(sex ~ personality) +
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
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  coord_cartesian(
    ylim = c(-6, 3.5),
    clip = "off"
  ) +
  guides(
    fill = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  labs(
    x = "Group",
    y = "PC1 Score"
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, colour = "black"),
    strip.text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(0.6, "cm")
  )

PC1_plot_facet

# ================================
# PC1 annotations: male letters only
# ================================

# Male overall letters
# Within males: Baseline differs from Post-Con 7, Post-Con 11, and Memory
# Baseline = a; Post-Con 3 = ab; Post-Con 7 = b; Post-Con 11 = b; Memory = b

pc1_letters_male <- data.frame(
  sex = "M",
  personality = c(rep("Bold", 10), rep("Shy", 10)),
  xpos = c(
    0.62, 0.82, 1.02, 1.22, 1.42,   # Male-Bold Control
    1.62, 1.82, 2.02, 2.22, 2.42,   # Male-Bold Treatment
    0.62, 0.82, 1.02, 1.22, 1.42,   # Male-Shy Control
    1.62, 1.82, 2.02, 2.22, 2.42    # Male-Shy Treatment
  ),
  y = rep(1.85, 20),
  letter = rep(c("a", "ab", "b", "b", "b"), 4)
)

PC1_plot_facet_sig <- PC1_plot_facet +
  geom_text(
    data = pc1_letters_male,
    aes(x = xpos, y = y, label = letter),
    inherit.aes = FALSE,
    size = 3.0,
    fontface = "bold"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  coord_cartesian(
    ylim = c(-6, 3.2),
    clip = "off"
  )

PC1_plot_facet_sig


p4 <- PC1_plot_facet_sig +
  theme(
    plot.margin = margin(t = 35, r = 35, b = 10, l = 45),
    axis.title.y = element_text(
      size = 13,
      face = "bold",
      margin = margin(r = 8)
    )
  )
p4_main <- ggdraw(p4) +
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
p4_main
ggsave(filename = "C:/Users/bjimm/Downloads/memory/Memory-Project/Figures/PC1.png", p4_main, width =6.7, height = 4, dpi = 600)

