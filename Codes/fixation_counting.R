library(saccadr)
library(readxl)

p01_1_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")

library(saccadr)

sum(is.na(p01_1_data$MonitorX))
# 277 NA value
sum(is.na(p01_1_data$MonitorY))
# 277 NA value

colnames(p01_1_data)
# Going to use "Screen_name" for trial. 

## Cleaning Data
library(dplyr)

# Cleaning the NA values inside MonitorX and MonitorY.
clean_EasyB <- p01_1_data %>%
  dplyr::filter(
    !is.na(MonitorX),
    !is.na(MonitorY)
  )

table(p01_1_data$Screen_name)
table(clean_EasyB$Screen_name)


library(dplyr)

# 1. Prepare clean data
clean_EasyB <- p01_1_data %>%
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

# 2. Corrected Salvucci & Goldberg I-DT algorithm
detect_fixations_idt_fixed <- function(df, max_disp_px = 40, min_duration_ms = 100, sample_rate = 150) {
  
  # Eye tracker 150 FPS. So, Checking 15 samples at least to detect fixation.
  min_samples <- ceiling((min_duration_ms / 1000) * sample_rate)
  
  screens <- unique(df$Screen_name)
  all_fixations <- list()
  
  for (scr in screens) {
    sub_df <- df %>% dplyr::filter(Screen_name == scr)
    n_rows <- nrow(sub_df)
    
    if (n_rows < min_samples) next
    
    fix_list <- list()
    i <- 1
    
    while (i <= (n_rows - min_samples + 1)) {
      # Dispersion Algorithm. Also saving the starting and ending Time[ms] to check if the fixation is real. 
      win_idx <- i:(i + min_samples - 1)
      win_x <- sub_df$MonitorX[win_idx]
      win_y <- sub_df$MonitorY[win_idx]
      
      # Calculate initial dispersion
      dispersion <- (max(win_x) - min(win_x)) + (max(win_y) - min(win_y))
      
      if (dispersion <= max_disp_px) {
        # Yes, we have a fixation starting at "i"th Time[ms] 
        # Now extend the window forward point-by-point as long as dispersion stays below threshold
        j <- i + min_samples
        while (j <= n_rows) {
          extended_win_x <- sub_df$MonitorX[i:j]
          extended_win_y <- sub_df$MonitorY[i:j]
          ext_dispersion <- (max(extended_win_x) - min(extended_win_x)) + (max(extended_win_y) - min(extended_win_y))
          
          if (ext_dispersion <= max_disp_px) {
            j <- j + 1
          } else {
            break # Exceeded threshold; fixation ends at j-1
          }
        }
        
        fix_end_idx <- j - 1
        
        # Save the fixation
        fix_list[[length(fix_list) + 1]] <- data.frame(
          Screen_name = scr,
          Start_Time = sub_df$`Time[ms]`[i],
          End_Time = sub_df$`Time[ms]`[fix_end_idx],
          Duration_ms = sub_df$`Time[ms]`[fix_end_idx] - sub_df$`Time[ms]`[i],
          Mean_X = mean(sub_df$MonitorX[i:fix_end_idx]),
          Mean_Y = mean(sub_df$MonitorY[i:fix_end_idx]),
          Samples = (fix_end_idx - i + 1)
        )
        
        # The next search must start completely fresh AFTER this fixation ends.
        i <- fix_end_idx + 1
      } else {
        # No fixation here. Slide the window forward by exactly 1 sample.
        i <- i + 1
      }
    }
    
    if (length(fix_list) > 0) {
      all_fixations[[as.character(scr)]] <- do.call(rbind, fix_list)
    }
  }
  
  if (length(all_fixations) == 0) return(NULL)
  return(do.call(rbind, all_fixations))
}

# 3. Extract the real fixations
fixations_EasyB <- detect_fixations_idt_fixed(
  clean_EasyB, 
  max_disp_px = 40,      # Maximum pixel dispersion allowed 
  min_duration_ms = 100,  # Minimum fixation duration (100 ms)
  sample_rate = 150
)

# 4. View your new, highly realistic results!
View(fixations_EasyB)
head(fixations_EasyB, 10)
table(fixations_EasyB$Screen_name)


##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################




library(saccadr)
library(readxl)

p01_2_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_2.xlsx")

library(saccadr)

sum(is.na(p01_2_data$MonitorX))
# 340 NA value
sum(is.na(p01_2_data$MonitorY))
# 340 NA value

colnames(p01_1_data)
# Going to use "Screen_name" for trial. 

## Cleaning Data
library(dplyr)

# Cleaning the NA values inside MonitorX and MonitorY.
clean_HardB <- p01_2_data %>%
  dplyr::filter(
    !is.na(MonitorX),
    !is.na(MonitorY)
  )

table(p01_2_data$Screen_name)
table(clean_HardB$Screen_name)


library(dplyr)

# 1. Prepare clean data
clean_HardB <- p01_2_data %>%
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

# 2. Corrected Salvucci & Goldberg I-DT algorithm
detect_fixations_idt_fixed <- function(df, max_disp_px = 40, min_duration_ms = 100, sample_rate = 150) {
  
  # Eye tracker 150 FPS. So, Checking 15 samples at least to detect fixation.
  min_samples <- ceiling((min_duration_ms / 1000) * sample_rate)
  
  screens <- unique(df$Screen_name)
  all_fixations <- list()
  
  for (scr in screens) {
    sub_df <- df %>% dplyr::filter(Screen_name == scr)
    n_rows <- nrow(sub_df)
    
    if (n_rows < min_samples) next
    
    fix_list <- list()
    i <- 1
    
    while (i <= (n_rows - min_samples + 1)) {
      # Dispersion Algorithm. Also saving the starting and ending Time[ms] to check if the fixation is real. 
      win_idx <- i:(i + min_samples - 1)
      win_x <- sub_df$MonitorX[win_idx]
      win_y <- sub_df$MonitorY[win_idx]
      
      # Calculate initial dispersion
      dispersion <- (max(win_x) - min(win_x)) + (max(win_y) - min(win_y))
      
      if (dispersion <= max_disp_px) {
        # Yes, we have a fixation starting at "i"th Time[ms] 
        # Now extend the window forward point-by-point as long as dispersion stays below threshold
        j <- i + min_samples
        while (j <= n_rows) {
          extended_win_x <- sub_df$MonitorX[i:j]
          extended_win_y <- sub_df$MonitorY[i:j]
          ext_dispersion <- (max(extended_win_x) - min(extended_win_x)) + (max(extended_win_y) - min(extended_win_y))
          
          if (ext_dispersion <= max_disp_px) {
            j <- j + 1
          } else {
            break # Exceeded threshold; fixation ends at j-1
          }
        }
        
        fix_end_idx <- j - 1
        
        # Save the fixation
        fix_list[[length(fix_list) + 1]] <- data.frame(
          Screen_name = scr,
          Start_Time = sub_df$`Time[ms]`[i],
          End_Time = sub_df$`Time[ms]`[fix_end_idx],
          Duration_ms = sub_df$`Time[ms]`[fix_end_idx] - sub_df$`Time[ms]`[i],
          Mean_X = mean(sub_df$MonitorX[i:fix_end_idx]),
          Mean_Y = mean(sub_df$MonitorY[i:fix_end_idx]),
          Samples = (fix_end_idx - i + 1)
        )
        
        # The next search must start completely fresh AFTER this fixation ends.
        i <- fix_end_idx + 1
      } else {
        # No fixation here. Slide the window forward by exactly 1 sample.
        i <- i + 1
      }
    }
    
    if (length(fix_list) > 0) {
      all_fixations[[as.character(scr)]] <- do.call(rbind, fix_list)
    }
  }
  
  if (length(all_fixations) == 0) return(NULL)
  return(do.call(rbind, all_fixations))
}

# 3. Extract the real fixations
fixations_HardB <- detect_fixations_idt_fixed(
  clean_HardB, 
  max_disp_px = 40,      # Maximum pixel dispersion allowed 
  min_duration_ms = 100,  # Minimum fixation duration (100 ms)
  sample_rate = 150
)

# 4. View your new, highly realistic results!
View(fixations_HardB)
head(fixations_HardB, 10)
table(fixations_HardB$Screen_name)


############################
###########################
###########################
#Extract excel files

library(writexl)

# Define where you want to save the files (Update the folder path if needed)
output_path_easy <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1_EasyB_Fixations.xlsx"
output_path_hard <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_2_HardB_Fixations.xlsx"

# Write out the dataframes to separate Excel files
write_xlsx(fixations_EasyB, path = output_path_easy)
write_xlsx(fixations_HardB, path = output_path_hard)





###################################
###################################
### Finding regression
###################################
###################################

library(dplyr)

extract_detailed_regressions <- function(fixations_df, line_height_px = 30) {
  
  detailed_regressions <- fixations_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::arrange(Start_Time) %>%
    dplyr::mutate(
      Fixation_ID = dplyr::row_number(),
      
      # Target fixation details (where the eye jumped TO)
      To_Fixation_ID = dplyr::lead(Fixation_ID),
      From_X = Mean_X,
      From_Y = Mean_Y,
      To_X = dplyr::lead(Mean_X),
      To_Y = dplyr::lead(Mean_Y),
      
      Delta_X = To_X - From_X,
      Delta_Y = To_Y - From_Y,
      
      # Distance of the regression jump (in pixels)
      Regression_Distance_px = round(sqrt(Delta_X^2 + Delta_Y^2), 1),
      
      # Flag Regression
      Is_Regression = dplyr::case_when(
        is.na(Delta_X) ~ FALSE,
        Delta_Y < -line_height_px ~ TRUE, # Jumped UP to a previous line
        abs(Delta_Y) <= line_height_px & Delta_X < -20 ~ TRUE, # Jumped LEFT on same line
        TRUE ~ FALSE
      ),
      
      # Categorize the type of regression
      Regression_Type = dplyr::case_when(
        !Is_Regression ~ "None",
        Delta_Y < -line_height_px ~ "Inter-line (Jumped to Previous Line)",
        TRUE ~ "Intra-line (Jumped Back on Same Line)"
      )
    ) %>%
    # Explicitly use dplyr::filter to avoid base R filter() collision
    dplyr::filter(Is_Regression == TRUE) %>%
    dplyr::select(
      Screen_name, 
      From_Fixation_ID = Fixation_ID, 
      To_Fixation_ID,
      From_X, From_Y, 
      To_X, To_Y, 
      Delta_X, Delta_Y, 
      Regression_Distance_px,
      Regression_Type,
      Fixation_Duration_Before_Jump_ms = Duration_ms
    ) %>%
    dplyr::ungroup()
  
  return(detailed_regressions)
}

# Run the fixed function
regressions_EasyB <- extract_detailed_regressions(fixations_EasyB, line_height_px = 30)
regressions_HardB <- extract_detailed_regressions(fixations_HardB, line_height_px = 30)

# Verify execution
head(regressions_EasyB)
head(regressions_HardB)


library(writexl)

# Export EasyB Dataset
write_xlsx(
  list(
    "All_Fixations" = fixations_n_regression_EasyB,
    "Regression_Events_Only" = regressions_EasyB
  ),
  path = "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_Regressions_Detailed.xlsx"
)

# Export HardB Dataset
write_xlsx(
  list(
    "All_Fixations" = fixations_n_regression_HardB,
    "Regression_Events_Only" = regressions_HardB
  ),
  path = "C:/Users/Emir/Desktop/cvut intern/Original_Data/HardB_Regressions_Detailed.xlsx"
)





####################################
####################################
####################################
####################################
####################################
# Changing the blank space and px settings


library(dplyr)
library(readxl)
library(ggplot2)
library(png)
library(grid)

# ==========================================
# 1. Load & Normalize Data
# ==========================================
# Target paths
data_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx"
aoi_path  <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_Word_AOIs_Auto.xlsx"
img_path  <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_1.png"

# Load Raw Tracking Data
p01_1_data <- read_excel(data_path)

# Load AOI Data and normalize Screen_name ("EasyB_1" -> "1")
word_aois <- read_excel(aoi_path) %>%
  mutate(Screen_name = gsub("[^0-9]", "", as.character(Screen_name)))

# Clean tracking data and normalize Screen_name ("EasyB_1" -> "1")
clean_EasyB <- p01_1_data %>%
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  mutate(Screen_name = gsub("[^0-9]", "", as.character(Screen_name))) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

# ==========================================
# 2. Pure Duration-Based I-DT Algorithm (No min_samples)
# ==========================================
detect_fixations_idt_pure <- function(df, max_disp_x = 96, max_disp_y = 60, min_duration_ms = 100) {
  
  screens <- unique(df$Screen_name)
  all_fixations <- list()
  
  for (scr in screens) {
    sub_df <- df %>% dplyr::filter(Screen_name == scr)
    n_rows <- nrow(sub_df)
    if (n_rows < 2) next
    
    fix_list <- list()
    i <- 1
    
    while (i < n_rows) {
      j <- i + 1
      
      # Expand window as long as horizontal/vertical dispersion stays below thresholds
      while (j <= n_rows) {
        win_x <- sub_df$MonitorX[i:j]
        win_y <- sub_df$MonitorY[i:j]
        
        disp_x <- max(win_x) - min(win_x)
        disp_y <- max(win_y) - min(win_y)
        
        if (disp_x <= max_disp_x && disp_y <= max_disp_y) {
          j <- j + 1
        } else {
          break
        }
      }
      
      fix_end_idx <- j - 1
      duration <- sub_df$`Time[ms]`[fix_end_idx] - sub_df$`Time[ms]`[i]
      
      # Retain ONLY if physical duration meets or exceeds 100 ms
      if (duration >= min_duration_ms) {
        fix_list[[length(fix_list) + 1]] <- data.frame(
          Screen_name = as.character(scr),
          Start_Time  = sub_df$`Time[ms]`[i],
          End_Time    = sub_df$`Time[ms]`[fix_end_idx],
          Duration_ms = duration,
          Mean_X      = mean(sub_df$MonitorX[i:fix_end_idx]),
          Mean_Y      = mean(sub_df$MonitorY[i:fix_end_idx]),
          Samples     = (fix_end_idx - i + 1)
        )
        i <- fix_end_idx + 1
      } else {
        i <- i + 1
      }
    }
    
    if (length(fix_list) > 0) {
      all_fixations[[as.character(scr)]] <- do.call(rbind, fix_list)
    }
  }
  
  if (length(all_fixations) == 0) return(NULL)
  return(do.call(rbind, all_fixations))
}

# Run fixation detection
fixations_EasyB <- detect_fixations_idt_pure(
  clean_EasyB, 
  max_disp_x = 96,      # 96 px horizontal limit
  max_disp_y = 60,      # 60 px vertical limit
  min_duration_ms = 150 # Physical time threshold
)

# ==========================================
# 3. Single Bounding Box Text AOI (Text Rectangle)
# ==========================================

##
filter_single_text_block_aoi <- function(fixations_df, word_aois_df, padding_px = 0) {
  
  # Calculate exact envelope for the text per screen
  single_block_aois <- word_aois_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::summarise(
      Text_xmin = 150,
      Text_xmax = 1785,
      Text_ymin = 180,
      Text_ymax = 1080,
      .groups = "drop"
    )
  
  print(single_block_aois)
  # Keep only fixations strictly inside this tight bounding box
  filtered_fixations <- fixations_df %>%
    dplyr::inner_join(single_block_aois, by = "Screen_name") %>%
    dplyr::filter(
      Mean_X >= Text_xmin & Mean_X <= Text_xmax &
        Mean_Y >= Text_ymin & Mean_Y <= Text_ymax
    ) %>%
    dplyr::select(-Text_xmin, -Text_xmax, -Text_ymin, -Text_ymax)
  
  return(filtered_fixations)
}

# Apply rectangular AOI filter
fixations_EasyB_filtered <- filter_single_text_block_aoi(fixations_EasyB, word_aois, padding_px = 0)

# Check counts
cat("Fixations BEFORE filtering:", nrow(fixations_EasyB), "\n")
cat("Fixations INSIDE Text AOI :", nrow(fixations_EasyB_filtered), "\n")

# ==========================================
# 4. Plotting Matching Results (Screen "1")
# ==========================================
screen_target <- "1"

# Load background stimulus
img <- readPNG(img_path)
img_w <- dim(img)[2] # 1920
img_h <- dim(img)[1] # 1200
bg_image <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# Calculate tight bounding box for screen "1"
screen_aoi <- word_aois %>%
  filter(Screen_name == screen_target) %>%
  summarise(
    Text_xmin = min(xmin),
    Text_xmax = max(xmax),
    Text_ymin = min(ymin),
    Text_ymax = max(ymax)
  )

cat("\nCalculated Tight AOI Bounds for Screen", screen_target, ":\n")
print(screen_aoi)

# Get filtered fixations for screen "1"
plot_fixations <- fixations_EasyB_filtered %>%
  filter(Screen_name == screen_target) %>%
  arrange(Start_Time) %>%
  mutate(Fixation_Order = row_number())

# Render Plot
ggplot(plot_fixations, aes(x = Mean_X, y = Mean_Y)) +
  # Background Stimulus
  annotation_custom(bg_image, xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
  
  # Tight Red Rectangular AOI Boundary around Paragraph Block
  geom_rect(
    data = screen_aoi,
    aes(xmin = Text_xmin, xmax = Text_xmax, ymin = Text_ymin, ymax = Text_ymax),
    inherit.aes = FALSE,
    fill = NA, color = "red", linetype = "dashed", linewidth = 1.2
  ) +
  
  # Chronological Fixation Path & Points
  geom_path(color = "blue", alpha = 0.7, linewidth = 0.8) +
  geom_point(aes(size = Duration_ms), color = "blue", alpha = 0.8) +
  geom_text(aes(label = Fixation_Order), color = "white", size = 2.5, fontface = "bold") +
  
  # Axis Scaling
  scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h, 0), expand = c(0, 0)) +
  
  theme_minimal() +
  labs(
    title = paste("Text AOI Bounding Box & Fixation Path - Screen", screen_target),
    subtitle = "Red dashed line = Tight Text AOI Boundary",
    x = "X Position (px)",
    y = "Y Position (px)",
    size = "Duration (ms)"
  )







