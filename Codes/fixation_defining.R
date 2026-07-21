# ==============================================================================
# STRICT I-DT DISPERSION-BASED COGNITIVE STRUGGLE SCANPATH
# ==============================================================================
rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)

# 1. Load and Prepare Raw Data
raw_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")
colnames(raw_data)[ncol(raw_data)] <- "Item"

# 2. Setup Spatial Constants (Dispersion Threshold)
# 50 pixels represents roughly 1° of visual angle on a standard desk setup.
max_dispersion_pixels <- 50 

cleaned_data <- raw_data %>%
  filter(!is.na(Item)) %>%
  mutate(
    Trial = consecutive_id(Item),
    # Map context: Only EasyB_1 is Easy, everything else is Hard
    ContextDifficulty = ifelse(Item == "EasyB_1", "Easy", "Hard")
  ) %>%
  arrange(Trial, `Time[ms]`)

# 3. Apply Grid Layout Lines (AOIs)
spatial_mapped <- cleaned_data %>%
  mutate(
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
    )
  ) %>%
  filter(Line_Reading != "Margin_Looking")

# 4. STRICT I-DT PROCESSING: Identify fixations using dispersion windows
idt_fixations <- spatial_mapped %>%
  group_by(Trial, Line_Reading) %>%
  mutate(
    # Check dispersion over a rolling 10-frame window (~150-200ms of data)
    window_min_x = zoo::rollapplyr(MonitorX, width = 10, FUN = min, fill = NA),
    window_max_x = zoo::rollapplyr(MonitorX, width = 10, FUN = max, fill = NA),
    window_min_y = zoo::rollapplyr(MonitorY, width = 10, FUN = min, fill = NA),
    window_max_y = zoo::rollapplyr(MonitorY, width = 10, FUN = max, fill = NA),
    
    # Mathematical Dispersion: (Max_X - Min_X) + (Max_Y - Min_Y)
    Dispersion = (window_max_x - window_min_x) + (window_max_y - window_min_y),
    
    # STRICT RULE: Must stay within the bounding box to be a fixation
    Is_Fixating_Window = ifelse(!is.na(Dispersion) & Dispersion <= max_dispersion_pixels, 1, 0),
    Fixation_Group = consecutive_id(Is_Fixating_Window)
  ) %>%
  filter(Is_Fixating_Window == 1) %>%
  ungroup()

# 5. Summarize into Distinct Chronological Fixation Events
final_scanpath_data <- idt_fixations %>%
  group_by(ContextDifficulty, Item, Trial, Line_Reading, Fixation_Group) %>%
  summarise(
    Start_Time = min(`Time[ms]`),
    Duration_Ms = max(`Time[ms]`) - min(`Time[ms]`),
    X = mean(MonitorX),
    Y = mean(MonitorY),
    .groups = "drop"
  ) %>%
  # Biological threshold: Discard events shorter than 100ms
  filter(Duration_Ms >= 100) %>%
  
  # Chronological sequencing across the trial
  arrange(Trial, Start_Time) %>%
  group_by(Trial) %>%
  mutate(
    Fixation_Sequence_No = row_number(),
    # COGNITIVE STRUGGLE FLAG: High-duration fixation in the sequence (>400ms)
    Is_Struggle = ifelse(Duration_Ms > 400, "Struggled", "Normal")
  ) %>%
  ungroup()

# Set factor levels so Y-axis layout matches a physical page
line_order <- c("Title", "Line_1", "Line_2", "Line_3", "Line_4", "Line_5", "Line_6", "Line_7")
final_scanpath_data$Line_Reading <- factor(final_scanpath_data$Line_Reading, levels = rev(line_order))

# 6. Filter a window to inspect (e.g., first 35 fixations of a Trial)
subset_scanpath <- final_scanpath_data %>% 
  filter(Fixation_Sequence_No <= 35)

# 7. Plot the True I-DT Struggle Scanpath
ggplot(subset_scanpath, aes(x = X, y = Line_Reading, group = Trial)) +
  # Background page boundaries
  annotate("rect", xmin = 200, xmax = 1700, ymin = 0.5, ymax = 8.5, alpha = 0.05, fill = "black") +
  
  # Saccade pathways (Color reflects Easy vs. Hard context)
  geom_path(
    aes(color = ContextDifficulty),
    linewidth = 1,
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    alpha = 0.8
  ) +
  
  # Normal I-DT points (White rim)
  geom_point(
    data = filter(subset_scanpath, Is_Struggle == "Normal"),
    aes(size = Duration_Ms, fill = ContextDifficulty), 
    shape = 21, color = "white", stroke = 1, alpha = 0.8
  ) +
  
  # STRUGGLE I-DT points (Thick black rim)
  geom_point(
    data = filter(subset_scanpath, Is_Struggle == "Struggled"),
    aes(size = Duration_Ms, fill = ContextDifficulty), 
    shape = 21, color = "black", stroke = 2.5, alpha = 0.95
  ) +
  
  # Sequence numbers inside the bubbles
  geom_text(
    aes(label = Fixation_Sequence_No), 
    color = "white", 
    size = 3.5, 
    fontface = "bold"
  ) +
  
  # Millisecond labels on top of struggled points
  geom_text(
    data = filter(subset_scanpath, Is_Struggle == "Struggled"),
    aes(label = paste0(Duration_Ms, "ms")),
    color = "black",
    vjust = -1.8,
    size = 3,
    fontface = "italic"
  ) +
  
  # Styling and Legends
  scale_size_continuous(range = c(3.5, 12), name = "Duration (ms)") +
  scale_color_manual(values = c("Easy" = "#2ca02c", "Hard" = "#d62728")) +
  scale_fill_manual(values = c("Easy" = "#2ca02c", "Hard" = "#d62728")) +
  xlim(100, 1800) +
  theme_minimal(base_size = 14) +
  labs(
    title = "I-DT Dispersion-Based Reading Scanpath Map",
    subtitle = "Green path = Easy text | Red path = Hard text | Bold black circles = Cognitive Stalls (>400ms)",
    x = "Horizontal Position on Screen (Pixels)",
    y = "Layout Position"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
