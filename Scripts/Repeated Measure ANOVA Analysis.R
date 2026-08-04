# Run ANOVA for log CSTime
# Run three way Anova
F_three.way <- anova_test(
  data    = FLM_Data,
  dv      = log_CSTime,
  wid     = subject_id,
  within  = timepoint,                    # only timepoint is repeated
  between = c(group, personality)         # these don't change per fish
)
get_anova_table(F_three.way,correction = "GG")

library(afex)
library(emmeans)

#Group * timepoint
model <- aov_ez(
  id = "subject_id",
  dv = "log_CSTime",
  data = FLM_Data,
  within = "timepoint",
  between = c("group", "personality"),
  type = 3,
  anova_table = list(correction = "GG", es = "ges")
)

emm_group_time <- emmeans(model, ~ group | timepoint)

pairs(
  emm_group_time,
  adjust = "bonferroni"
)

#
emm_time_group <- emmeans(model, ~ timepoint | group)

pairs(
  emm_time_group,
  adjust = "tukey"
)
# main effect of personality and timepoint
emm_personality <- emmeans(model, ~ personality)

pairs(
  emm_personality,
  adjust = "tukey"
)
#timepoint
emm_time <- emmeans(model, ~ timepoint)

pairs(
  emm_time,
  adjust = "tukey"
)

# Bold vs Shy at each time point
emm_personality_time <- emmeans(
  model,
  ~ personality | timepoint
)

pairs(
  emm_personality_time,
  adjust = "bonferroni"
)

# Time-point comparisons within each personality
emm_time_personality <- emmeans(
  model,
  ~ timepoint | personality
)

pairs(
  emm_time_personality,
  adjust = "tukey"
)
# Comparison for trends
#Trends
# Overall interaction
Model_five<- aov(data = FLM_Data, formula = log_CSTime ~ timepoint*group*personality + Error(subject_id/timepoint))

Five_posthoc <- emmeans(Model_five, "timepoint", by = c("group", "personality"))

Five_apairs<-pairs(Five_posthoc, adjust="tukey")

Five_apairs


# Run ANOVA for log Freq
Fr_three.way <- anova_test(
  data    = FLM_Data,
  dv      = log_CS_freq,
  wid     = subject_id,
  within  = timepoint,                    # only timepoint is repeated
  between = c(group, personality,sex)         # these don't change per fish
)
get_anova_table(Fr_three.way,correction = "GG")

#Comparison model
model_fr <- aov_ez(
  id = "subject_id",
  dv = "log_CS_freq",
  data = FLM_Data,
  within = "timepoint",
  between = c("group", "personality","sex"),
  type = 3,
  anova_table = list(correction = "GG", es = "ges")
)

# main effect of personality and sex
emm_personality <- emmeans(model_fr, ~ personality)

pairs(
  emm_personality,
  adjust = "tukey"
)
#sex
emm_sex <- emmeans(model_fr, ~ sex)

pairs(
  emm_sex,
  adjust = "tukey"
)

# group *timepoint :Control versus treatment at each time point
emm_group_time <- emmeans(model_fr, ~ group | timepoint)

pairs(
  emm_group_time,
  adjust = "bonferroni"
)

# Time points within each group
emm_time_group <- emmeans(model_fr, ~ timepoint | group)

pairs(
  emm_time_group,
  adjust = "tukey"
)

# personality *timepoint :bold versus shy at each time point

emm_personality_time <- emmeans(model_fr,~ personality | timepoint)

pairs(
  emm_personality_time,
  adjust = "bonferroni"
)
# Time points within each personality
emm_time_personality <- emmeans(model_fr,~ timepoint | personality)

pairs(
  emm_time_personality,
  adjust = "tukey"
)

# sex *timepoint :male versus female at each time point
emm_sex_time <- emmeans(model_fr, ~ sex | timepoint)

pairs(emm_sex_time,
      adjust = "bonferroni"
)

# Time points within each sex
emm_time_sex <- emmeans(model_fr,~ timepoint | sex)

pairs(
  emm_time_sex,
  adjust = "tukey"
)

# group *sex :bold versus shy at female vs male
emm_per_grp <- emmeans(model_fr, ~ group| personality)

pairs(emm_per_grp,
      adjust = "tukey"
)
# personlityvwithin each group
emm_grp_personality <- emmeans(model_fr,~ personality | group)

pairs(
  emm_grp_personality,
  adjust = "tukey"
)


# personality *sex :bold versus shy at female vs male
emm_per_sex <- emmeans(model_fr, ~ personality| sex)

pairs(emm_per_sex,
      adjust = "tukey"
)
# sex within each personality
emm_sex_personality <- emmeans(model_fr,~ sex | personality)

pairs(
  emm_sex_personality,
  adjust = "tukey"
)

# 4.Personality: timepoint — Control vs Treatment at each personality
Model_freq<- aov(data = FLM_Data, formula = log_CS_freq ~ timepoint*group*personality*Sex + Error(subject_id/timepoint))

Fre_posthoc <- emmeans(model_fr, "timepoint", by = c("group", "personality", "sex"))

Fre_apairs<-pairs(Fre_posthoc, adjust="tukey")

Fre_apairs

# Run ANOVA for log lat
La_three.way <- anova_test(
  data    = FLM_Data,
  dv      = log_CS_lat,
  wid     = subject_id,
  within  = timepoint,                    # only timepoint is repeated
  between = c(group, personality)         # these don't change per fish
)
get_anova_table(La_three.way,correction = "GG")


# Comparison model
model_lat <- aov_ez(
  id = "subject_id",
  dv = "log_CS_lat",
  data = FLM_Data,
  within = "timepoint",
  between = c("group", "personality"),
  type = 3,
  anova_table = list(correction = "GG", es = "ges")
)

# main effect of personality 
emm_personality <- emmeans(model_lat, ~ personality)

pairs(
  emm_personality,
  adjust = "tukey"
)

#personality*timepoint:bold vs shy at each timepoint
emm_personality_time <- emmeans(model_lat,~ personality | timepoint)

pairs(emm_personality_time,
      adjust = "tukey"
)

emm_time_personality <- emmeans(model_lat,~ timepoint | personality)

pairs(
  emm_time_personality,
  adjust = "tukey"
)


# 2.Personality: timepoint — Control vs Treatment at each personality
Model_lat<- aov(data = FLM_Data, formula = log_CS_lat ~ timepoint*group*personality + Error(subject_id/timepoint))

lat_posthoc <- emmeans(model_lat, "timepoint", by = c("group", "personality"))

lat_apairs<-pairs(lat_posthoc, adjust="bonferroni")

lat_apairs


# Compare across timepoint all four combination (interested in probe 4)
emm_pro4 <- emmeans(
  model,
  ~ group * personality | timepoint
)

pairs(
  emm_pro4,
  by = "timepoint",
  adjust = "tukey"
)






