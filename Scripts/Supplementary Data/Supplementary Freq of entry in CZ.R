# EXPORT FREQUENCY OF ENTRY ANOVA + POST HOC TABLES TO WORD
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
anova_freq <- get_anova_table(Fr_three.way, correction = "GG") %>%
  as.data.frame()

# 2. Post hoc tables
# Main effect of personality
posthoc_freq_personality_main <- pairs(
  emm_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Main effect of sex
posthoc_freq_sex_main <- pairs(
  emm_sex,
  adjust = "tukey"
) %>%
  as.data.frame()

# Group comparison within each conditioned day
posthoc_freq_group_within_time <- pairs(
  emm_group_time,
  adjust = "bonferroni"
) %>%
  as.data.frame()

# Conditioned day comparison within each group
posthoc_freq_time_within_group <- pairs(
  emm_time_group,
  adjust = "tukey"
) %>%
  as.data.frame()

# Personality comparison within each conditioned day
posthoc_freq_personality_within_time <- pairs(
  emm_personality_time,
  adjust = "bonferroni"
) %>%
  as.data.frame()

# Conditioned day comparison within each personality type
posthoc_freq_time_within_personality <- pairs(
  emm_time_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Sex comparison within each conditioned day
posthoc_freq_sex_within_time <- pairs(
  emm_sex_time,
  adjust = "bonferroni"
) %>%
  as.data.frame()

# Conditioned day comparison within each sex
posthoc_freq_time_within_sex <- pairs(
  emm_time_sex,
  adjust = "tukey"
) %>%
  as.data.frame()

# Group comparison within each personality type
posthoc_freq_group_within_personality <- pairs(
  emm_per_grp,
  adjust = "tukey"
) %>%
  as.data.frame()

# Personality comparison within each group
posthoc_freq_personality_within_group <- pairs(
  emm_grp_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Personality comparison within each sex
posthoc_freq_personality_within_sex <- pairs(
  emm_per_sex,
  adjust = "tukey"
) %>%
  as.data.frame()

# Sex comparison within each personality type
posthoc_freq_sex_within_personality <- pairs(
  emm_sex_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Four-way post hoc:
# Conditioned day comparisons within each group × personality × sex combination
posthoc_freq_time_within_group_personality_sex <- Fre_apairs %>%
  as.data.frame()

# 3. Create Word document
doc_freq <- read_docx()
doc_freq <- doc_freq %>%
  body_add_par(
    "Supplementary Tables: Frequency of Entry into the Conditioned Zone",
    style = "heading 1"
  ) %>%
  
  body_add_par(
    "Supplementary Table S1. Repeated-measures ANOVA for frequency of entry into the conditioned zone.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(anova_freq)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S2. Post hoc comparison for the main effect of personality.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_personality_main)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S3. Post hoc comparison for the main effect of sex.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_sex_main)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S4. Post hoc comparisons for group within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_group_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S5. Post hoc comparisons for conditioned day within each group.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_time_within_group)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S6. Post hoc comparisons for personality within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_personality_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S7. Post hoc comparisons for conditioned day within each personality type.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_time_within_personality)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S8. Post hoc comparisons for sex within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_sex_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S9. Post hoc comparisons for conditioned day within each sex.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_time_within_sex)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S10. Post hoc comparisons for group within each personality type.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_group_within_personality)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S11. Post hoc comparisons for personality within each group.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_personality_within_group)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S12. Post hoc comparisons for personality within each sex.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_personality_within_sex)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S13. Post hoc comparisons for sex within each personality type.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_sex_within_personality)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S14. Post hoc comparisons for conditioned day within each group × personality × sex combination.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_freq_time_within_group_personality_sex))

# 4. Save Word document
out_file_freq <- "C:/Users/bjimm/Downloads/memory/Memory-Project/Supplementary Tables/Supplementary_Frequency_CZ_Tables.docx"

print(doc_freq, target = out_file_freq)

cat("Saved frequency supplementary tables here:\n", normalizePath(out_file_freq), "\n")

shell.exec(out_file_freq)