# LIBRARIES 
library(readxl); library(tidyverse); library(rstatix)
library(ggpubr); library(writexl); library(tibble); library(emmeans)

# LOAD DATA 
Learning_Memory_Data <- read_excel("C:/Users/bjimm/Downloads/memory/Manuscript Prep/Learning & Memory Data.xlsx")

# SETTINGS 
time_levels <- c("BAS", "PRO 1", "PRO 2", "PRO 3", "PRO 4")

tp_labels <- c(
  "BAS"   = "Baseline",
  "PRO 1" = "Day 4",
  "PRO 2" = "Day 8",
  "PRO 3" = "Day 12",
  "PRO 4" = "Day 19"
)

# ZONE ASSIGNMENT
# Auto-assign CS zone from last 5 mins of baseline
zone_auto <- Learning_Memory_Data %>%
  filter(TRIAL == "BAS", `Time bin` == "0:05:00-0:10:00") %>%
  mutate(
    fish_num   = parse_number(`FISH ID`),
    subject_id = factor(paste(Batch, fish_num, sep = "_")),
    solid_s    = suppressWarnings(as.numeric(`In zone...22`)),
    checked_s  = suppressWarnings(as.numeric(`In zone...25`))
  ) %>%
  group_by(subject_id) %>%
  summarise(
    cs_pattern = if_else(solid_s > checked_s, "Checked", "Solid"),
    .groups = "drop"
  )

# Manual overrides
LM_manual_cs <- tribble(
  ~subject_id, ~cs_pattern,
  "B_3",  "Checked", "B_6",  "Solid",   "B_8",  "Solid",
  "B_9",  "Checked", "B_14", "Checked", "B_18", "Solid",
  "C_2",  "Solid",   "C_3",  "Solid",   "C_7",  "Checked",
  "C_11", "Checked", "C_13", "Solid",   "C_14", "Solid",
  "C_15", "Checked", "C_16", "Checked", "C_17", "Checked"
)

zone_key <- zone_auto %>%
  rows_update(LM_manual_cs, by = "subject_id") %>%
  mutate(cs_pattern = factor(cs_pattern, levels = c("Solid", "Checked")))

# BUILD MAIN DATASET (LAST 5 MINS ONLY)
FLM_Data <- Learning_Memory_Data %>%
  filter(TRIAL %in% time_levels, `Time bin` == "0:05:00-0:10:00") %>%
  mutate(
    fish_num    = parse_number(`FISH ID`),
    subject_id  = factor(paste(Batch, fish_num, sep = "_")),
    timepoint   = factor(TRIAL, levels = time_levels),
    group       = factor(GROUP1, levels = c("CONTROL", "TREATMENT")),
    sex         = factor(Sex),
    personality = factor(case_when(
      str_detect(str_to_upper(STRAIN), "^BOLD") ~ "Bold",
      str_detect(str_to_upper(STRAIN), "^SHY")  ~ "Shy"
    ), levels = c("Bold", "Shy")),
    solid_s    = suppressWarnings(as.numeric(`In zone...22`)),
    checked_s  = suppressWarnings(as.numeric(`In zone...25`)),
    zone1_freq = suppressWarnings(as.numeric(`In zone...21`)),
    zone2_freq = suppressWarnings(as.numeric(`In zone...24`)),
    zone1_lat_raw  = suppressWarnings(as.numeric(`In zone...23`)),
    zone2_lat_raw  = suppressWarnings(as.numeric(`In zone...26`)),
    
    # Convert full-trial timestamps to latency within the final 5-min bin.
    # A raw latency of 0 with zero entries means the zone was never entered.
    zone1_lat = case_when(
      zone1_freq == 0 ~ 300,
      TRUE ~ pmin(pmax(zone1_lat_raw - 300, 0), 300)
    ),
    
    zone2_lat = case_when(
      zone2_freq == 0 ~ 300,
      TRUE ~ pmin(pmax(zone2_lat_raw - 300, 0), 300)
    )    
  ) %>%
  filter(
    !is.na(subject_id), !is.na(personality), !is.na(group), !is.na(sex),
    !is.na(solid_s), !is.na(checked_s),
    !is.na(zone1_freq), !is.na(zone2_freq),
    !is.na(zone1_lat), !is.na(zone2_lat)
  ) %>%
  
  # Assign CS/NS zones
  left_join(zone_key, by = "subject_id") %>%
  mutate(
    # CSTime
    CSTime      = if_else(cs_pattern == "Solid", solid_s,    checked_s),
    NSTime      = if_else(cs_pattern == "Solid", checked_s,  solid_s),
    # Frequency
    CS_freq     = if_else(cs_pattern == "Solid", zone1_freq, zone2_freq),
    NS_freq     = if_else(cs_pattern == "Solid", zone2_freq, zone1_freq),
    # Latency
    CS_lat      = if_else(cs_pattern == "Solid", zone1_lat,  zone2_lat),
    NS_lat      = if_else(cs_pattern == "Solid", zone2_lat,  zone1_lat),
    # Log transformations
    log_CSTime  = log(CSTime  + 1),
    log_CS_freq = log(CS_freq + 1),
    log_CS_lat  = log(CS_lat  + 1),
    # Group label
    GroupLabel  = factor(
      case_when(
        personality == "Bold" & group == "CONTROL"   ~ "Bold Control",
        personality == "Bold" & group == "TREATMENT" ~ "Bold Treatment",
        personality == "Shy"  & group == "CONTROL"   ~ "Shy Control",
        personality == "Shy"  & group == "TREATMENT" ~ "Shy Treatment"
      ),
      levels = c("Bold Control", "Bold Treatment", "Shy Control", "Shy Treatment")
    )
  ) %>%
  # Balance — keep only fish with all 5 timepoints
  group_by(personality, group, subject_id) %>%
  filter(n_distinct(timepoint) == 5) %>%
  ungroup()

# CONFIRM 
cat("N fish:", n_distinct(FLM_Data$subject_id), "\n")
cat("N rows:", nrow(FLM_Data), "\n")

# Sanity check — total time should be ~300s
FLM_Data %>%
  mutate(total = CSTime + NSTime) %>%
  summarise(min = min(total), max = max(total), mean = mean(total))

glimpse(FLM_Data)


# SECTION A: CS FREQUENCY
# Check normality
FLM_Data %>%
  group_by(personality, group, timepoint) %>%
  shapiro_test(log_CS_freq)

# SECTION B: CS LATENCY 
# Check normality
FLM_Data %>%
  group_by(personality, group, timepoint) %>%
  shapiro_test(log_CS_lat)

# SECTION C: CSTime
# Check normality
FLM_Data %>%
  group_by(personality, group, timepoint) %>%
  shapiro_test(log_CSTime)

# Create QQ plot for each cell of design:
# CSTime
ggqqplot(FLM_Data, "log_CSTime", ggtheme = theme_bw()) +
  facet_grid(personality + group ~ timepoint, labeller = "label_both")

# CS_freq
ggqqplot(FLM_Data, "log_CS_freq", ggtheme = theme_bw()) +
  facet_grid(personality + group ~ timepoint, labeller = "label_both")

# CS_lat
ggqqplot(FLM_Data, "log_CS_lat", ggtheme = theme_bw()) +
  facet_grid(personality + group ~ timepoint, labeller = "label_both")

