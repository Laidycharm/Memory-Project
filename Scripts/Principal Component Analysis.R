#Run PCA: to compute PCA score, we need to keep metadata and numeric variables separate
# let's select the data we need
PC_info <- FLM_Data %>%
  select("subject_id","group","GroupLabel","personality", "sex", "timepoint")

PC_data <- FLM_Data %>%
  select("log_CS_freq", "log_CS_lat", "log_CSTime")

# Compute PCA
PCA_behavior <- prcomp(
  PC_data,
  center = TRUE,
  scale. = TRUE
)
summary(PCA_behavior)

# Combine metadata back with PCA scores
Pca_scores <- as.data.frame(PCA_behavior$x) %>%
  bind_cols(PC_info) # ensures the row stays in the same order

# Plot graph
# Calculate percentage variance
pca_var <- PCA_behavior$sdev^2 / sum(PCA_behavior$sdev^2)
pc1_var <- round(pca_var[1] * 100, 1)
pc2_var <- round(pca_var[2] * 100, 1)

# Plot graph
PCA_plot <- ggplot(Pca_scores, aes(x = PC1, y = PC2, color = personality, shape = group)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = personality), level = 0.95, linewidth = 0.7) +
  scale_color_manual(values = c(
    "Bold" = "#1a80bb",
    "Shy" = "#f1a226"
  )) +
  theme_classic(base_size = 13, base_family = "Arial") +
  labs(
    title = "PCA of Behavioral Measurements",
    x = paste0("PC1 (", pc1_var, "%)"),
    y = paste0("PC2 (", pc2_var, "%)"),
    color = "Personality",
    shape = "Group"
  )

PCA_plot
ggsave("PCA_plot.png", width = 8, height = 6, units = "in", dpi=600)

# Quick check on the variable driving the base plot.
biplot(PCA_behavior)

# Generate eigenvalues(some paper request it is reported)
library(factoextra)
PCA_behavior$rotation
get_eig(PCA_behavior)

# improved biplot to remove fish I.D, so you see it represented as only point
PCA_data <- FLM_Data %>%
  select(
    `Entry frequency` = log_CS_freq,
    `Latency` = log_CS_lat,
    `Time spent` = log_CSTime
  )
PCA_behavior <- prcomp(
  PCA_data,
  center = TRUE,
  scale. = TRUE
)
biplot.better <- fviz_pca_biplot(PCA_behavior,geom = c("point"),
                                 label = "var",
                                 invisible = "none",
                                 labelsize = 5,
                                 pointsize = 2,
                                 col.var = "#1a80bb",
                                 repel = TRUE
) +
  coord_cartesian(xlim = c(-6.2, 3.2), ylim = c(-2.4, 2.2)) +
  theme_bw(9) +
  xlab(paste0("PC1 (", pc1_var, "%)")) +
  ylab(paste0("PC2 (", pc2_var, "%)")) +
  theme(
    plot.title = element_blank(),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, color = "black")
  )
biplot.better
ggsave("biplot.better.png", width = 6, height = 6, units = "in", dpi=600)


# Results for individuals
res.ind <- get_pca_ind(PCA_behavior)
res.ind$coord          # Coordinates

# Contributions to the PCs
res.ind$contrib     

# Quality of representation on the PCA axis
res.ind$cos2 


# Combine data DIM of each fish to FishI.D info
Combined_pca <- as.data.frame(res.ind$coord)
Combined_pca$Fish.ID <- rownames(Combined_pca)
write.csv(res.ind$coord, "memory.csv")

# Total combined sheet
pca.df <- cbind(FLM_Data, Combined_pca)

