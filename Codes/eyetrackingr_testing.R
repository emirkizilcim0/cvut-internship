# import the library.
library(eyetrackingR)
library(devtools)


# import the raw data (easy one)
library(readxl)
data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")
head(data)
str(data)
View(data)

library(dplyr)
names(data)

data <- data %>% select(-"Císlo obrazovky")
View(data)

raw_data <- data
colnames(raw_data)[ncol(raw_data)] <- "Item"

# 1. Map coordinates
raw_data$TargetParagraph <- with(raw_data, 
                                 MonitorX >= 50 & MonitorX <= 1750 & MonitorY >= 150 & MonitorY <= 1100
)

# 2. Create Predictor
raw_data$ContextDifficulty <- ifelse(raw_data$Item == "EasyB_1", "Easy", "Hard")

# 3. Create UNIQUE Trial Numbers based on when the Item shifts
raw_data$ParticipantName <- "Participant_1"
raw_data$TrackLoss <- FALSE 
raw_data <- raw_data %>% 
  mutate(Trial = consecutive_id(Item))

# 4. NEW FIX: Drop any accidental duplicate rows at the exact same millisecond
raw_data <- raw_data %>% 
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)

# 5. Convert to eyetrackingR format (This worked!)
data_et <- make_eyetrackingr_data(raw_data, 
                                  participant_column = "ParticipantName",
                                  trial_column = "Trial",
                                  time_column = "Time[ms]",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = "TargetParagraph",
                                  treat_non_aoi_looks_as_missing = TRUE)

# 6. Time Binning & Feature Creation (Summarized by Trial for single-participant data)
response_window <- make_time_sequence_data(data_et, 
                                           time_bin_size = 100, 
                                           aois = "TargetParagraph",
                                           predictor_columns = "ContextDifficulty",
                                           summarize_by = "Trial")


# 7. Identify Cognitive Effort Windows (With treatment_level specified)
cluster_data <- make_time_cluster_data(response_window, 
                                       predictor_column = "ContextDifficulty",
                                       aoi = "TargetParagraph",
                                       test = "lm",              
                                       formula = Prop ~ ContextDifficulty, 
                                       threshold = 2.0,
                                       treatment_level = "Hard") # Tells R to look for 'ContextDifficultyHard'

summary(cluster_data)

print(cluster_data)


rm(list = ls())
library(devtools)
library("eyetrackingR")
library(dplyr)
library(readxl)

# ==========================================
# 1. LOAD AND PREPARE SINGLE PARTICIPANT DATA
# ==========================================
p1_raw <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")
colnames(p1_raw)[ncol(p1_raw)] <- "Item"

p1_raw <- p1_raw %>%
  mutate(
    ParticipantName = "Participant_1",
    TrackLoss = FALSE,
    Trial = consecutive_id(Item),
    # Shrunken AOI to capture centered text gaze variance
    TargetParagraph = MonitorX >= 50 & MonitorX <= 1400 & MonitorY >= 150 & MonitorY <= 1100,
    # Classifying trials into comparison groups
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Condition_A", "Condition_B")
  ) %>%
  filter(!is.na(Item)) %>% 
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)

# ==========================================
# 2. RUN THE EYETRACKINGR PIPELINE
# ==========================================
data_et <- make_eyetrackingr_data(p1_raw, 
                                  participant_column = "ParticipantName",
                                  trial_column = "Trial",
                                  time_column = "Time[ms]",
                                  trackloss_column = "TrackLoss",
                                  aoi_columns = "TargetParagraph",
                                  treat_non_aoi_looks_as_missing = TRUE)

# CRITICAL CHANGE: Summarize by Trial so R treats each text block as a unique data source
response_window <- make_time_sequence_data(data_et, 
                                           time_bin_size = 100, 
                                           aois = "TargetParagraph",
                                           predictor_columns = "ContextDifficulty",
                                           summarize_by = "Trial")

# Run standard linear model tracking trial-by-trial variance
cluster_data <- make_time_cluster_data(response_window, 
                                       predictor_column = "ContextDifficulty",
                                       aoi = "TargetParagraph",
                                       test = "lm",              
                                       formula = Prop ~ ContextDifficulty, 
                                       threshold = 0.01, 
                                       treatment_level = "Condition_B")

# Check for clusters
summary(cluster_data)


library(dplyr)
# Let's inspect the actual gaze proportions for each condition
response_window %>%
  group_by(ContextDifficulty) %>%
  summarise(
    Avg_Prop = mean(Prop, na.rm = TRUE),
    Min_Prop = min(Prop, na.rm = TRUE),
    Max_Prop = max(Prop, na.rm = TRUE),
    Total_Rows = n()
  )


rm(list = ls())
library(devtools)
library("eyetrackingR")
library(dplyr)
library(readxl)

# ==========================================
# 1. LOAD AND PREPARE DATA
# ==========================================
raw_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")
colnames(raw_data)[ncol(raw_data)] <- "Item"

raw_data <- raw_data %>%  
  mutate(
    ParticipantName = "Participant_1",
    TrackLoss = FALSE ,
    Trial = consecutive_id(Item),
    # FIXED: Shrink the horizontal AOI (700 to 1100) to capture center-reading gaze variance
    TargetParagraph = MonitorX >= 700 & MonitorX <= 1100 & MonitorY >= 150 & MonitorY <= 1100,
    # FIXED: Balanced split (Blocks 1 & 2 vs Blocks 3 & 4) to allow mathematical variance
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Condition_A", "Condition_B")
  ) %>% 
  filter(!is.na(Item)) %>% 
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)

# ==========================================
# 2. RUN EYETRACKINGR PIPELINE
# ==========================================
data_et <- make_eyetrackingr_data(raw_data, 
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
                                           summarize_by = "Trial")

# Run cluster analysis with a lower threshold to catch small differences in pilot data
cluster_data <- make_time_cluster_data(response_window, 
                                       predictor_column = "ContextDifficulty",
                                       aoi = "TargetParagraph",
                                       test = "lm",              
                                       formula = Prop ~ ContextDifficulty, 
                                       threshold = 0.5, 
                                       treatment_level = "Condition_B")

summary(cluster_data)




# ==============================================================================
# READING MECHANICS EYE-TRACKING PIPELINE (FROM SCRATCH)
# ==============================================================================
# Clear workspace and load core packages
rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------------------------
# 1. LOAD RAW DATA
# ------------------------------------------------------------------------------
# Load the raw spreadsheet data directly
raw_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")

# Ensure the last column containing the text block IDs is explicitly named "Item"
colnames(raw_data)[ncol(raw_data)] <- "Item"

# ------------------------------------------------------------------------------
# 2. DATA CLEANING & METADATA INJECTION
# ------------------------------------------------------------------------------
cleaned_data <- raw_data %>%
  # Filter out missing rows where the tracker lost the text session label
  filter(!is.na(Item)) %>%
  
  mutate(
    ParticipantName = "Participant_1",
    
    # Track discrete shifts in reading tasks to cleanly isolate trial boundaries
    Trial = consecutive_id(Item),
    
    # Balanced Predictor Splitting: Map blocks to their true condition groupings
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Easy", "Hard")
  ) %>%
  
  # Remove technical duplicates sharing identical timestamps to preserve data sanity
  distinct(ParticipantName, Trial, `Time[ms]`, .keep_all = TRUE)



# This will show you the exact pixel zones where gaze points cluster heaviest
hist(raw_data$MonitorY, breaks = 100, main = "Vertical Gaze Clusters")

# ------------------------------------------------------------------------------
# 3. SPATIAL ALIGNMENT (LINE-BY-LINE AOIs MAPPED TO THE PNG LAYOUT)
# ------------------------------------------------------------------------------
# This slices the screen vertically into horizontal bands matching your paragraphs
spatial_data <- cleaned_data %>%
  arrange(Trial, `Time[ms]`) %>%
  mutate(
    Line_Reading = case_when(
      MonitorY >= 150 & MonitorY < 250  ~ "Title",
      MonitorY >= 250 & MonitorY < 370  ~ "Line_1",
      MonitorY >= 370 & MonitorY < 490  ~ "Line_2",
      MonitorY >= 490 & MonitorY < 610  ~ "Line_3",
      MonitorY >= 610 & MonitorY < 730  ~ "Line_4",
      MonitorY >= 730 & MonitorY < 850  ~ "Line_5",
      MonitorY >= 850 & MonitorY < 970  ~ "Line_6",
      MonitorY >= 970 & MonitorY <= 1100 ~ "Line_7",
      TRUE                               ~ "Margin_Looking"
    )
  )

# ------------------------------------------------------------------------------
# 4. COMPUTE COGNITIVE EFFORT & DWELL ANALYSIS
# ------------------------------------------------------------------------------
# Step A: Track row-by-row gaze shifts to catch real-time movements
movement_calculated <- spatial_data %>%
  group_by(Trial) %>%
  mutate(
    # Track distance changes frame-to-frame
    Delta_X = MonitorX - lag(MonitorX),
    Delta_Y = MonitorY - lag(MonitorY),
    Total_Distance = sqrt(Delta_X^2 + Delta_Y^2),
    
    # Flag a frame if the eye remained perfectly stable on a specific coordinate
    Is_Fixated = ifelse(Total_Distance < 8, 1, 0)
  ) %>%
  ungroup()

# Step B: Aggregate reading metrics per line across trials
final_reading_metrics <- movement_calculated %>%
  filter(Line_Reading != "Margin_Looking") %>%
  group_by(ContextDifficulty, Item, Trial, Line_Reading) %>%
  summarise(
    # 1. Total absolute viewing time spent on this line (in seconds)
    Total_Dwell_Time_Sec = (max(`Time[ms]`) - min(`Time[ms]`)) / 1000,
    
    # 2. Count total raw data points captured inside this sentence strip
    Gaze_Data_Volume = n(),
    
    # 3. Total time the eye completely stopped moving (Fixation processing count)
    Stabilized_Frames = sum(Is_Fixated == 1, na.rm = TRUE),
    
    # 4. Regressions: Count how many times the eye leaped backward right-to-left
    Horizontal_Backtracks = sum(Delta_X < -80, na.rm = TRUE),
    
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 5. RENDER CLINICAL READING OUTCOMES
# ------------------------------------------------------------------------------
print("=== COMPLETED EYE-TRACKING PARAGRAPH ANALYSIS ===")
print(final_reading_metrics)


library(ggplot2)

# Order the lines chronologically for clean plotting
final_reading_metrics$Line_Reading <- factor(
  final_reading_metrics$Line_Reading, 
  levels = c("Title", "Line_1", "Line_2", "Line_3", "Line_4", "Line_5", "Line_6", "Line_7")
)

# Render the Cognitive Load Plot
ggplot(final_reading_metrics, aes(x = Line_Reading, y = Total_Dwell_Time_Sec, group = Item, color = ContextDifficulty)) +
  geom_line(linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Easy" = "#2ca02c", "Hard" = "#d62728")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Reading Engagement Profile: Single Subject Pilot",
    subtitle = "Tracking time spent per line across text difficulties",
    x = "Paragraph Structural Line",
    y = "Total Dwell Time (Seconds)",
    color = "Text Condition"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print("=== STATISTICAL EVALUATION: COGNITIVE ENGAGEMENT ===")

# Test 1: Does 'Hard' text cause a statistically significant increase in Dwell Time?
dwell_model <- lm(Total_Dwell_Time_Sec ~ ContextDifficulty, data = final_reading_metrics)
print("--- DWELL TIME ANALYSIS SUMMARY ---")
print(summary(dwell_model))

# Test 2: Does 'Hard' text trigger more reading backtracks (regressions)?
backtrack_model <- lm(Horizontal_Backtracks ~ ContextDifficulty, data = final_reading_metrics)
print("--- READING REGRESSIONS (BACKTRACKS) SUMMARY ---")
print(summary(backtrack_model))





library(dplyr)
library(ggplot2)

# Ensure the last column is named "Item" just like in the main pipeline
colnames(raw_data)[ncol(raw_data)] <- "Item"

# Extract ALL raw pauses without the 100ms filter to check the data distribution
raw_pauses <- raw_data %>%
  filter(!is.na(Item)) %>%
  # Dynamically generate Trial based on text block shifts
  mutate(Trial = consecutive_id(Item)) %>%
  arrange(Trial, `Time[ms]`) %>%
  group_by(Trial) %>%
  mutate(
    Delta_X = MonitorX - lag(MonitorX),
    Delta_Y = MonitorY - lag(MonitorY),
    Distance = sqrt(Delta_X^2 + Delta_Y^2),
    Is_Stationary = ifelse(Distance < 10, 1, 0),
    Pause_ID = consecutive_id(Is_Stationary)
  ) %>%
  filter(Is_Stationary == 1) %>%
  ungroup() %>%
  group_by(Pause_ID) %>%
  summarise(Duration = max(`Time[ms]`) - min(`Time[ms]`), .groups = "drop")

# Plot the distribution to see the physical vs. biological threshold break
ggplot(raw_pauses, aes(x = Duration)) +
  geom_histogram(binwidth = 10, fill = "#8e44ad", alpha = 0.7, color = "white") +
  geom_vline(xintercept = 100, color = "red", linetype = "dashed", linewidth = 1.2) +
  xlim(0, 600) +  # Focus on the most relevant window for reading fixations
  labs(
    title = "Objective Verification of Fixation Thresholds",
    subtitle = "Red dashed line marks the biological minimum (100 ms)",
    x = "Raw Pause Duration (Milliseconds)",
    y = "Frequency Count"
  ) +
  theme_minimal()



#################################3

library(dplyr)

# ==============================================================================
# SELF-CONTAINED FIXED I-VT FIXATION EXTRACTION ENGINE
# ==============================================================================

# Ensure the last column is correctly named "Item"
colnames(raw_data)[ncol(raw_data)] <- "Item"

fixation_events <- raw_data %>%
  # Filter out missing rows where text tracking isn't active
  filter(!is.na(Item)) %>%
  
  # DYNAMIC FIXES: Generate missing predictor and trial structural columns
  mutate(
    Trial = consecutive_id(Item),
    ContextDifficulty = ifelse(Item %in% c("EasyB_1", "EasyB_2"), "Easy", "Hard")
  ) %>%
  
  arrange(Trial, `Time[ms]`) %>%
  group_by(Trial) %>%
  mutate(
    # 1. Calculate distance between consecutive eye tracking samples
    Delta_X = MonitorX - lag(MonitorX),
    Delta_Y = MonitorY - lag(MonitorY),
    Distance = sqrt(Delta_X^2 + Delta_Y^2),
    
    # 2. Map coordinates to the approximate layout lines
    Line_Reading = case_when(
      MonitorY >= 150 & MonitorY < 250  ~ "Title",
      MonitorY >= 250 & MonitorY < 360  ~ "Line_1",
      MonitorY >= 360 & MonitorY < 470  ~ "Line_2",
      MonitorY >= 470 & MonitorY < 580  ~ "Line_3",
      MonitorY >= 580 & MonitorY < 690  ~ "Line_4",
      MonitorY >= 690 & MonitorY < 800  ~ "Line_5",
      MonitorY >= 800 & MonitorY < 910  ~ "Line_6",
      MonitorY >= 910 & MonitorY <= 1020 ~ "Line_7",
      TRUE                               ~ "Margin_Looking"
    ),
    
    # 3. Flag as "Stationary" if the movement is tiny (under 10 pixels)
    Is_Stationary = ifelse(Distance < 10, 1, 0),
    
    # 4. Generate a unique ID number every time the eye moves or switches lines
    Fixation_ID = consecutive_id(Is_Stationary, Line_Reading)
  ) %>%
  filter(Is_Stationary == 1, Line_Reading != "Margin_Looking") %>%
  ungroup() %>%
  
  # 5. Collapse raw samples into distinct, timed fixation events
  group_by(ContextDifficulty, Item, Trial, Line_Reading, Fixation_ID) %>%
  summarise(
    Start_Time_Ms = min(`Time[ms]`),
    End_Time_Ms = max(`Time[ms]`),
    
    # Crucial Metric: How many milliseconds did this single fixation last?
    Fixation_Duration_Ms = End_Time_Ms - Start_Time_Ms,
    
    # The exact center coordinate on the screen where the eye was looking
    Centroid_X = mean(MonitorX, na.rm = TRUE),
    Centroid_Y = mean(MonitorY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # 6. Apply standard biological filter: Fixations must last at least 100ms
  filter(Fixation_Duration_Ms >= 100) %>%
  arrange(Trial, Start_Time_Ms)

# Print the formal fixation registry
print("=== EXTRACTED FORMAL FIXATION EVENTS ===")
print(head(fixation_events, 20))


################# GRAPH

library(dplyr)
library(ggplot2)

# ==============================================================================
# FIXATION COALESCENCE PIPELINE (SPATIAL MERGING)
# ==============================================================================

coalesced_fixations <- fixation_events %>%
  arrange(Trial, Start_Time_Ms) %>%
  group_by(Trial, Line_Reading) %>%
  mutate(
    # Calculate horizontal distance between this fixation and the previous one
    Horizontal_Gap = Centroid_X - lag(Centroid_X),
    
    # TRIGGER: If the eye moved less than 40 pixels on the same line, 
    # it's the same word block! Mark it as 0 (no shift).
    Is_New_Word_Look = ifelse(is.na(Horizontal_Gap) | abs(Horizontal_Gap) > 40, 1, 0),
    
    # Generate a unique ID for this specific word landing
    Macro_Cluster_ID = cumsum(Is_New_Word_Look)
  ) %>%
  ungroup() %>%
  
  # Group by our new spatial word blocks to merge them
  group_by(ContextDifficulty, Item, Trial, Line_Reading, Macro_Cluster_ID) %>%
  summarise(
    # The macro-fixation starts at the first pause and ends when the eye leaves the word
    Macro_Start_Ms = min(Start_Time_Ms),
    Macro_End_Ms = max(End_Time_Ms),
    Macro_Duration_Ms = Macro_End_Ms - Macro_Start_Ms,
    
    # Count how many micro-fixations were swallowed up into this one word
    Micro_Fixation_Count = n(),
    
    # Calculate the true visual center of the word block
    Final_Centroid_X = mean(Centroid_X),
    .groups = "drop"
  ) %>%
  # Filter out casual glances: only keep intense concentrations
  filter(Macro_Duration_Ms >= 250)

# ------------------------------------------------------------------------------
# PLOT THE MERGED MACRO-FIXATIONS
# ------------------------------------------------------------------------------
line_order <- c("Title", "Line_1", "Line_2", "Line_3", "Line_4", "Line_5", "Line_6", "Line_7")
coalesced_fixations$Line_Reading <- factor(coalesced_fixations$Line_Reading, levels = rev(line_order))

ggplot(coalesced_fixations, aes(x = Final_Centroid_X, y = Line_Reading, color = ContextDifficulty)) +
  annotate("rect", xmin = 200, xmax = 1700, ymin = 0.5, ymax = 8.5, alpha = 0.04, fill = "blue") +
  geom_point(aes(size = Macro_Duration_Ms), alpha = 0.7) +
  scale_size_continuous(range = c(4, 14), name = "Word Gaze Duration (ms)") +
  scale_color_manual(values = c("Easy" = "#2ca02c", "Hard" = "#d62728")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Coalesced Macro-Fixation Map",
    subtitle = "Micro-pauses closer than 40px merged into singular word-level events",
    x = "Horizontal Position on Screen (Pixels)",
    y = "Layout Position"
  )



#####################
####################
####################
# Fixation Coalescence
######


library(dplyr)
library(ggplot2)

# ==============================================================================
# FIXATION COALESCENCE PIPELINE (SPATIAL MERGING)
# ==============================================================================

coalesced_fixations <- fixation_events %>%
  arrange(Trial, Start_Time_Ms) %>%
  group_by(Trial, Line_Reading) %>%
  mutate(
    # Calculate horizontal distance between this fixation and the previous one
    Horizontal_Gap = Centroid_X - lag(Centroid_X),
    
    # TRIGGER: If the eye moved less than 40 pixels on the same line, 
    # it's the same word block! Mark it as 0 (no shift).
    Is_New_Word_Look = ifelse(is.na(Horizontal_Gap) | abs(Horizontal_Gap) > 40, 1, 0),
    
    # Generate a unique ID for this specific word landing
    Macro_Cluster_ID = cumsum(Is_New_Word_Look)
  ) %>%
  ungroup() %>%
  
  # Group by our new spatial word blocks to merge them
  group_by(ContextDifficulty, Item, Trial, Line_Reading, Macro_Cluster_ID) %>%
  summarise(
    # The macro-fixation starts at the first pause and ends when the eye leaves the word
    Macro_Start_Ms = min(Start_Time_Ms),
    Macro_End_Ms = max(End_Time_Ms),
    Macro_Duration_Ms = Macro_End_Ms - Macro_Start_Ms,
    
    # Count how many micro-fixations were swallowed up into this one word
    Micro_Fixation_Count = n(),
    
    # Calculate the true visual center of the word block
    Final_Centroid_X = mean(Centroid_X),
    .groups = "drop"
  ) %>%
  # Filter out casual glances: only keep intense concentrations
  filter(Macro_Duration_Ms >= 250)

# ------------------------------------------------------------------------------
# PLOT THE MERGED MACRO-FIXATIONS
# ------------------------------------------------------------------------------
line_order <- c("Title", "Line_1", "Line_2", "Line_3", "Line_4", "Line_5", "Line_6", "Line_7")
coalesced_fixations$Line_Reading <- factor(coalesced_fixations$Line_Reading, levels = rev(line_order))

ggplot(coalesced_fixations, aes(x = Final_Centroid_X, y = Line_Reading, color = ContextDifficulty)) +
  annotate("rect", xmin = 200, xmax = 1700, ymin = 0.5, ymax = 8.5, alpha = 0.04, fill = "blue") +
  geom_point(aes(size = Macro_Duration_Ms), alpha = 0.7) +
  scale_size_continuous(range = c(4, 14), name = "Word Gaze Duration (ms)") +
  scale_color_manual(values = c("Easy" = "#2ca02c", "Hard" = "#d62728")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Coalesced Macro-Fixation Map",
    subtitle = "Micro-pauses closer than 40px merged into singular word-level events",
    x = "Horizontal Position on Screen (Pixels)",
    y = "Layout Position"
  )
