# EXPORT LATENCY TO FIRST ENTRY ANOVA + POST HOC TABLES TO WORD
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
anova_latency <- get_anova_table(La_three.way, correction = "GG") %>%
  as.data.frame()

# 2. Post hoc tables
# Main effect of personality
posthoc_lat_personality_main <- pairs(
  emm_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Personality comparison within each conditioned day
posthoc_lat_personality_within_time <- pairs(
  emm_personality_time,
  adjust = "tukey"
) %>%
  as.data.frame()

# Conditioned day comparison within each personality type
posthoc_lat_time_within_personality <- pairs(
  emm_time_personality,
  adjust = "tukey"
) %>%
  as.data.frame()

# Conditioned day comparisons within each group × personality combination
posthoc_lat_time_within_group_personality <- lat_apairs %>%
  as.data.frame()

# 3. Create Word document
doc_lat <- read_docx()

doc_lat <- doc_lat %>%
  body_add_par(
    "Supplementary Tables: Latency to First Entry into the Conditioned Zone",
    style = "heading 1"
  ) %>%
  
  body_add_par(
    "Supplementary Table S1. Repeated-measures ANOVA for latency to first entry into the conditioned zone.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(anova_latency)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S2. Post hoc comparison for the main effect of personality.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_lat_personality_main)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S3. Post hoc comparisons for personality within each conditioned day.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_lat_personality_within_time)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S4. Post hoc comparisons for conditioned day within each personality type.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_lat_time_within_personality)) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(
    "Supplementary Table S5. Post hoc comparisons for conditioned day within each group × personality combination.",
    style = "heading 2"
  ) %>%
  body_add_flextable(make_ft(posthoc_lat_time_within_group_personality))

# 4. Save Word document
out_file_lat <- "C:/Users/bjimm/Downloads/memory/Memory-Project/Supplementary Tables/Supplementary_Latency_CZ_Tables.docx"

print(doc_lat, target = out_file_lat)

cat("Saved latency supplementary tables here:\n", normalizePath(out_file_lat), "\n")

shell.exec(out_file_lat)