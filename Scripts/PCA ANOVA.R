# RUN ANALYSIS FOR PCA SCORES
# Run PC1 scores 
PC1_three.way <- anova_test(
  data    = Pca_scores,
  dv      = PC1,
  wid     = subject_id,
  within  = timepoint,                    # only timepoint is repeated
  between = c(group, personality,sex)         # these don't change per fish
)
get_anova_table(PC1_three.way,correction = "GG")


# Comparison model
PC1_model <- aov_ez(
  id = "subject_id",
  dv = "PC1",
  data = Pca_scores,
  within = "timepoint",
  between = c("group", "personality", "sex"),
  type = 3,
  anova_table = list(correction = "GG", es = "ges")
)

# main effect of personality and sex
emm_personality <- emmeans(PC1_model, ~ personality)

pairs(
  emm_personality,
  adjust = "tukey"
)

#sex
# main effect of personality and sex
emm_sex <- emmeans(PC1_model, ~ sex)

pairs(
  emm_sex,
  adjust = "tukey"
)
#personality*timepoint:bold vs shy at each timepoint
emm_personality_time_PC1 <- emmeans(PC1_model,~ personality | timepoint)

pairs(emm_personality_time_PC1,
      adjust = "tukey"
)

emm_time_personality_PC1 <- emmeans(PC1_model,~ timepoint | personality)

pairs(
  emm_time_personality_PC1,
  adjust = "tukey"
)

#group*timepoint:bold vs shy at each timepoint
emm_group_time_PC1 <- emmeans(PC1_model,~ group | timepoint)

pairs(emm_group_time_PC1,
      adjust = "tukey"
)

emm_time_group_PC1 <- emmeans(PC1_model,~ timepoint | group)

pairs(
  emm_time_group_PC1,
  adjust = "tukey"
)

#sex*timepoint:bold vs shy at each timepoint
emm_sex_time_PC1 <- emmeans(PC1_model,~ sex | timepoint)

pairs(emm_sex_time_PC1,
      adjust = "tukey"
)

emm_time_sex_PC1 <- emmeans(PC1_model,~ timepoint | sex)

pairs(
  emm_time_sex_PC1,
  adjust = "tukey"
)

#group*personality:bold vs shy at each timepoint
emm_group_personality_PC1 <- emmeans(PC1_model,~ group | personality)

pairs(emm_group_personality_PC1,
      adjust = "tukey"
)

emm_personality_group_PC1 <- emmeans(PC1_model,~ personality | group)

pairs(
  emm_personality_group_PC1,
  adjust = "tukey"
)

#personality*sex:bold vs shy at each timepoint
emm_personality_sex_PC1 <- emmeans(PC1_model,~ personality | sex)

pairs(emm_personality_sex_PC1,
      adjust = "tukey"
)

emm_sex_personality_PC1 <- emmeans(PC1_model,~ sex | personality)

pairs(
  emm_sex_personality_PC1,
  adjust = "tukey"
)

# Run PC2 scores 
PC2_three.way <- anova_test(
  data    = Pca_scores,
  dv      = PC2,
  wid     = subject_id,
  within  = timepoint,                    # only timepoint is repeated
  between = c(group, personality)         # these don't change per fish
)
get_anova_table(PC2_three.way,correction = "GG")
