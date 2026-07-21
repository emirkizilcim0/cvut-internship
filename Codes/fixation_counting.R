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










