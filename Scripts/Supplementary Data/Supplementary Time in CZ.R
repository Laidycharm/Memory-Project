# EXPORT TIME SPENT IN CZ ANOVA + POST HOC TABLES TO WORD
# Install once if needed
install.packages(c("officer", "flextable", "dplyr"))

library(officer)
library(flextable)
library(dplyr)

# Helper function for clean tables
make_ft <- function(df) {
  flextable(df) %>%
    autofit() %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    set_table_properties(width = 1, layout = "autofit")
}

# 1. ANOVA table
anova_time <- get_anova_table(F_three.way, correction = "GG") %>%
  as.data.frame()

# 2. Post hoc tables
# Group comparison within each timepoint
posthoc_group_within_time <- pairs(
  emm_group_time,
  adjust = "bonferroni"
) %>%
  as.data.frame()

# Timepoint comparison within each group
posthoc_time_within_group <- pairs(
  emm_time_group,
  adjust = "tukey"
) %>%
  as.data.frame()

# Main effect of personality
posthoc_personality_main <- pairs(
  emm_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Main effect of timepoint
posthoc_time_main <- pairs(
  emm_time,
  adjust = "tukey"
) %>%
  as.data.frame()

# Bold vs Shy at each timepoint
posthoc_personality_within_time <- pairs(
  emm_personality_time,
  adjust = "bonferroni"
) %>%
  as.data.frame()

# Timepoint comparison within each personality
posthoc_time_within_personality <- pairs(
  emm_time_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Three-way/trend post hoc:
# Timepoint comparisons within each group × personality combination
posthoc_time_within_group_personality <- Five_apairs %>%
  as.data.frame()

# 3. Create Word document
doc <- read_docx()

doc <- doc %>%
  body_add_par(
    "Supplementary Tables: Time Spent in the Conditioned Zone",
    style = "heading 1"
  ) %>%
  body_add_par(
    "Supplementary Table S1. Repeated-measures ANOVA for time spent in the conditioned zone.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(anova_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S2. Post hoc comparisons for group within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_group_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S3. Post hoc comparisons for conditioned day within each group.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_time_within_group)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S4. Post hoc comparison for the main effect of personality.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_personality_main)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S5. Post hoc comparisons for the main effect of conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_time_main)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S6. Post hoc comparisons for personality within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_personality_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S7. Post hoc comparisons for conditioned day within each personality type.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_time_within_personality)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S8. Post hoc comparisons for conditioned day within each group × personality combination.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_time_within_group_personality))

# 4. Save Word document
print(
  doc,
  target = "C:/Users/bjimm/Downloads/memory/Memory-Project/Supplementary Data/Supplementary_TimeSpent_CZ_Tables.docx"
)

