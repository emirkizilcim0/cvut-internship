
rm(list = ls())
library(devtools)
library("eyetrackingR")
library(dplyr)
library(readxl)
library(lme4)
library(broom.mixed)

# ==========================================
# 1. LOAD AND PREPARE PARTICIPANT 1
# ==========================================
p1_raw <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")
colnames(p1_raw)[ncol(p1_raw)] <- "Item"

p1_raw <- p1_raw %>%
  mutate(
    ParticipantName = "Participant_1",
    TrackLoss = FALSE,
    Trial = consecutive_id(Item),
    TargetParagraph = MonitorX >= 500 & MonitorX <= 1400 & MonitorY >= 150 & MonitorY <= 1100,
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Condition_A", "Condition_B")
  ) %>%
  filter(!is.na(Item)) %>% 
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)

# ==========================================
# 2. LOAD AND PREPARE PARTICIPANT 2
# ==========================================
p2_raw <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_2.xlsx") 
colnames(p2_raw)[ncol(p2_raw)] <- "Item"

p2_raw <- p2_raw %>%
  mutate(
    ParticipantName = "Participant_2",
    TrackLoss = FALSE,
    Trial = consecutive_id(Item),
    TargetParagraph = MonitorX >= 50 & MonitorX <= 1750 & MonitorY >= 150 & MonitorY <= 1100,
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Condition_A", "Condition_B")
  ) %>%
  filter(!is.na(Item)) %>%
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)

# ==========================================
# 3. COMBINE BOTH DATASETS
# ==========================================
combined_raw <- bind_rows(p1_raw, p2_raw)

# ==========================================
# 4. RUN THE EYETRACKINGR PIPELINE
# ==========================================
data_et <- make_eyetrackingr_data(combined_raw, 
                                  participant_column = "ParticipantName",
                                  trial_column = "Trial",
                                  time_column = "Time[ms]",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = "TargetParagraph",
                                  treat_non_aoi_looks_as_missing = TRUE)

response_window <- make_time_sequence_data(data_et, 
                                           time_bin_size = 100, 
                                           aois = "TargetParagraph",
                                           predictor_columns = "ContextDifficulty",
                                           summarize_by = "ParticipantName")


cluster_data <- make_time_cluster_data(response_window, 
                                       predictor_column = "ContextDifficulty",
                                       aoi = "TargetParagraph",
                                       test = "lm",              # Changed from lmer to lm
                                       formula = Prop ~ ContextDifficulty, # Removed random effects
                                       threshold = 0.5, 
                                       treatment_level = "Condition_B")

# Check if it successfully completes now!
summary(cluster_data)

