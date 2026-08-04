# Plot graph for frequency of entry into CZ
Freq_plot_facet <- ggplot(
  FLM_Data,
  aes(x = group, y = log_CS_freq, fill = timepoint)
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
    limits = c(0, 5.5),
    expand = expansion(mult = c(0, 0.03))
  ) +
  guides(
    fill = guide_legend(ncol = 1, byrow = TRUE)
  ) +
  labs(
    x = "Group",
    y = "Frequency of Entry into 
    Conditioned Zone (log secs)"
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

Freq_plot_facet

# Significance annotations for shy-treated males
# Baseline vs Post-Con 7, Post-Con 11, and Memory
# Letters for shy-treated males only
freq_letters_shy_treated_male <- data.frame(
  personality = "Shy",
  sex = "M",
  xpos = c(1.59, 1.80, 2.00, 2.22, 2.45),
  y = rep(4.0, 5),
  letter = c("a", "ab", "b", "bc", "c")
)

Freq_plot_facet_letters <- Freq_plot_facet +
  geom_text(
    data = freq_letters_shy_treated_male,
    aes(x = xpos, y = y, label = letter),
    inherit.aes = FALSE,
    size = 3.5,
    fontface = "bold"
  )

Freq_plot_facet_letters

# Add treatment × conditioned day significance brackets
# Treatment group: Baseline vs Post-Con 7 and Baseline vs Post-Con 11
# This is repeated in each facet because the interaction is overall

sig_freq_treat_day <- expand.grid(
  sex = c("F", "M"),
  personality = c("Bold", "Shy")
)

sig_freq_treat_day <- sig_freq_treat_day[rep(seq_len(nrow(sig_freq_treat_day)), each = 2), ]

sig_freq_treat_day$x1 <- c(1.59, 1.59)
sig_freq_treat_day$x2 <- c(2.00, 2.22)
sig_freq_treat_day$y  <- c(4.35, 4.65)
sig_freq_treat_day$label <- c("*", "*")

Freq_plot_facet_letters_2way <- Freq_plot_facet_letters +
  geom_segment(
    data = sig_freq_treat_day,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = sig_freq_treat_day,
    aes(x = x1, xend = x1, y = y - 0.06, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_segment(
    data = sig_freq_treat_day,
    aes(x = x2, xend = x2, y = y - 0.06, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +
  geom_text(
    data = sig_freq_treat_day,
    aes(x = (x1 + x2) / 2, y = y + 0.12, label = label),
    inherit.aes = FALSE,
    size = 4.2,
    fontface = "bold"
  )

Freq_plot_facet_letters_2way

p3 <- Freq_plot_facet_letters_2way +
  scale_y_continuous(
    limits = c(0, 5),
    expand = expansion(mult = c(0, 0.03))
  ) +
  theme(
    plot.margin = margin(t = 35, r = 35, b = 10, l = 35),
    axis.title.y = element_text(size = 13, face = "bold", margin = margin(r = 8))
  )

p3_main <- ggdraw(p3) +
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
p3_main
ggsave(filename = "C:/Users/bjimm/Downloads/memory/Memory-Project/Figures/Frequency of Entry CZ.png", p3_main, width =6.5, height = 4.5, dpi = 600)

