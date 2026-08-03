library(dplyr)
library(readxl)
library(purrr)
library(ggplot2)
library(png)
library(grid)
# ==============================================================================
# 1. CORE ALGORITHM & HELPER FUNCTIONS
# ==============================================================================

#I-DT Algorithm (Salvucci & Goldberg, 2000)
detect_fixations_idt_paper <- function(df, 
                                       max_disp_x,        
                                       max_disp_y,        
                                       min_duration_ms) {     
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(sub_df) {
      n_rows <- nrow(sub_df)
      if (n_rows < 2) return(NULL)
      
      sub_df <- sub_df %>% dplyr::arrange(`Time[ms]`)
      
      fix_list <- list()
      i <- 1
      
      while (i <= n_rows) {
        j <- i
        
        # Build candidate window until reaching min_duration_ms
        while (j <= n_rows) {
          if ((sub_df$`Time[ms]`[j] - sub_df$`Time[ms]`[i]) >= min_duration_ms) {
            break
          }
          j <- j + 1
        }
        
        # If remaining samples cannot satisfy min_duration_ms, break out
        if (j > n_rows) {
          i <- i + 1
          next
        }
        
        # Calculate spatial dispersion directly from raw coordinates
        disp_x <- max(sub_df$MonitorX[i:j]) - min(sub_df$MonitorX[i:j])
        disp_y <- max(sub_df$MonitorY[i:j]) - min(sub_df$MonitorY[i:j])
        
        # If window dispersion fits within spatial bounding limits
        if (disp_x <= max_disp_x && disp_y <= max_disp_y) {
          # Keep extending the window point-by-point
          while (j + 1 <= n_rows) {
            new_disp_x <- max(sub_df$MonitorX[i:(j + 1)]) - min(sub_df$MonitorX[i:(j + 1)])
            new_disp_y <- max(sub_df$MonitorY[i:(j + 1)]) - min(sub_df$MonitorY[i:(j + 1)])
            
            if (new_disp_x <= max_disp_x && new_disp_y <= max_disp_y) {
              j <- j + 1
            } else {
              # Eye moved beyond spatial threshold
              break
            }
          }
          
          # Record the detected fixation
          fix_list[[length(fix_list) + 1]] <- data.frame(
            Screen_name = as.character(sub_df$Screen_name[1]),
            Start_Time  = sub_df$`Time[ms]`[i],
            End_Time    = sub_df$`Time[ms]`[j],
            Duration_ms = sub_df$`Time[ms]`[j] - sub_df$`Time[ms]`[i],
            Mean_X      = mean(sub_df$MonitorX[i:j], na.rm = TRUE),
            Mean_Y      = mean(sub_df$MonitorY[i:j], na.rm = TRUE),
            Samples     = (j - i + 1)                      # How many times window got expended.
          )
          
          # Advance start index past the recorded fixation
          i <- j + 1
        } else {
          # Dispersion too high; advance start point by 1 sample
          i <- i + 1
        }
      }
      
      dplyr::bind_rows(fix_list)
    })
}

#' Defined Area of Interest (AOI) bounding box
filter_aoi <- function(fix_df, xmin, xmax, ymin, ymax) {
  if (is.null(fix_df) || nrow(fix_df) == 0) return(NULL)
  
  fix_df %>%
    dplyr::filter(
      Mean_X >= xmin, Mean_X <= xmax,
      Mean_Y >= ymin, Mean_Y <= ymax
    )
}


#' Classify forward reading vs. regressions (intra-line and inter-line)
detect_regressions <- function(fix_df, x_threshold = 50, y_threshold = 100) {
  if (is.null(fix_df) || nrow(fix_df) < 2) return(NULL)
  
  fix_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::arrange(Start_Time, .by_group = TRUE) %>%
    dplyr::mutate(
      Prev_X  = lag(Mean_X),
      Prev_Y  = lag(Mean_Y),
      Delta_X = Mean_X - Prev_X,
      Delta_Y = Mean_Y - Prev_Y,
      
      # Going backward on the same row.
      Is_X_Regression = !is.na(Delta_X) & Delta_X < -x_threshold & abs(Delta_Y) < y_threshold,
      # Going back to previous line.
      Is_Y_Regression = !is.na(Delta_Y) & Delta_Y < -y_threshold,
      Is_Regression   = Is_X_Regression | Is_Y_Regression,
      
      Regression_Type = case_when(
        Is_Y_Regression ~ "Inter-line (Upward Re-read)",
        Is_X_Regression ~ "Intra-line (Leftward Re-read)",
        TRUE            ~ "Forward Reading"
      )
    ) %>%
    dplyr::ungroup()
}

#' Plotting function
plot_scanpath <- function(fix_df, target_screen, bg_image_path = NULL, aoi_bounds = NULL) {
  df_scr <- fix_df %>% 
    dplyr::filter(as.character(Screen_name) == as.character(target_screen)) %>% 
    dplyr::arrange(Start_Time) %>% 
    dplyr::mutate(Fix_ID = row_number())
  
  if (nrow(df_scr) == 0) {
    message(paste("No fixations found for Screen:", target_screen))
    return(NULL)
  }
  
  p <- ggplot(df_scr, aes(x = Mean_X, y = Mean_Y))
  
  if (!is.null(bg_image_path) && file.exists(bg_image_path)) {
    img <- readPNG(bg_image_path)
    g_img <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
    p <- p + annotation_custom(g_img, xmin = 0, xmax = 1920, ymin = 0, ymax = 1080)
  }
  
  if (!is.null(aoi_bounds)) {
    p <- p + annotate("rect", 
                      xmin = aoi_bounds$xmin, xmax = aoi_bounds$xmax, 
                      ymin = aoi_bounds$ymin, ymax = aoi_bounds$ymax,
                      color = "red", fill = NA, linetype = "dashed", linewidth = 1)
  }
  
  p <- p +
    geom_path(color = "#1F77B4", linewidth = 0.8, alpha = 0.75) +
    geom_point(aes(size = Duration_ms), color = "#1F77B4", alpha = 0.8) +
    geom_text(aes(label = Fix_ID), color = "white", size = 2.5, fontface = "bold") +
    scale_size_continuous(name = "Duration (ms)", range = c(3, 9)) +
    scale_y_reverse(limits = c(1080, 0), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) +
    labs(
      title = paste("Scanpath Analysis — Screen", target_screen),
      subtitle = paste("Detected Fixations:", nrow(df_scr)),
      x = "X Position (px)", y = "Y Position (px)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.background = element_rect(fill = "gray90")
    )
  
  return(p)
}

merge_fixations <- function(fix_df, max_time_gap_ms, max_dist_px, min_final_duration_ms = 100) {
  if (is.null(fix_df) || nrow(fix_df) < 2) return(fix_df)
  
  fix_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::arrange(Start_Time, .by_group = TRUE) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(sub_df) {
      merged <- list()
      curr <- sub_df[1, ]
      
      for (k in 2:nrow(sub_df)) {
        next_fix <- sub_df[k, ]
        
        # Calculate time gap and Euclidean distance
        time_gap <- next_fix$Start_Time - curr$End_Time
        dist_px  <- sqrt((next_fix$Mean_X - curr$Mean_X)^2 + (next_fix$Mean_Y - curr$Mean_Y)^2)
        
        if (time_gap <= max_time_gap_ms && dist_px <= max_dist_px) {
          # Calculate duration-weighted average position
          total_dur <- (next_fix$End_Time - curr$Start_Time)
          
          curr$Mean_X <- ((curr$Mean_X * curr$Duration_ms) + (next_fix$Mean_X * next_fix$Duration_ms)) / total_dur
          curr$Mean_Y <- ((curr$Mean_Y * curr$Duration_ms) + (next_fix$Mean_Y * next_fix$Duration_ms)) / total_dur
          
          # Update timestamps and overall duration
          curr$End_Time    <- next_fix$End_Time
          curr$Duration_ms <- total_dur
          
        } else {
          # Append completed fixation and advance pointer
          merged[[length(merged) + 1]] <- curr
          curr <- next_fix
        }
      }
      # Push final accumulated fixation
      merged[[length(merged) + 1]] <- curr
      
      res <- dplyr::bind_rows(merged)
      
      # Filter out remaining micro-noise under minimum duration threshold
      res %>% dplyr::filter(Duration_ms >= min_final_duration_ms)
    })
}

# ==============================================================================
# 2. EXECUTION & DIAGNOSTICS
# ==============================================================================

raw_file_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx"
bg_image_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_1.png"
output_image  <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/Scanpath_Clean.png"

aoi_box <- list(xmin = 125, xmax = 1850, ymin = 150, ymax = 990)

clean_data <- read_excel(raw_file_path) %>% 
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

fixations_raw <- clean_data %>%
  detect_fixations_idt_paper(
    max_disp_x = 96,        
    max_disp_y = 60,        
    min_duration_ms = 250
  )

table(fixations_raw$Screen_name)
fixations_merged <- fixations_raw %>%
  merge_fixations(
    max_time_gap_ms       = 45, 
    max_dist_px           = 100, 
    min_final_duration_ms = 100 
  )
table(fixations_merged$Screen_name)

fixations_final <- fixations_merged %>%
  filter_aoi(xmin = aoi_box$xmin, xmax = aoi_box$xmax, ymin = aoi_box$ymin, ymax = aoi_box$ymax) %>%
  detect_regressions(x_threshold = 50, y_threshold = 100)

cat("\n--- Execution Summary ---\n")
cat("Total Fixations Detected:", nrow(fixations_raw), "\n")
cat("Fixations inside Text AOI:", nrow(fixations_final), "\n\n")

View(fixations_final)

print(table(fixations_final$Screen_name, dnn = list("Screen Name")))

cat("\n--- Regression Classification Distribution ---\n")
print(prop.table(table(fixations_final$Regression_Type)))

scanpath_plot <- plot_scanpath(
  fix_df = fixations_final, 
  target_screen = "1", 
  bg_image_path = bg_image_path,
  aoi_bounds = aoi_box
)

if (!is.null(scanpath_plot)) {
  print(scanpath_plot)
  ggsave(output_image, plot = scanpath_plot, width = 10, height = 8, dpi = 300)
}

print(scanpath_plot)


### Exporting the "fixations_final" into excel.
library(writexl)

write_xlsx(fixations_final,
           "C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1_fixation_regression.xlsx")


### Creating pure heatmap.

#' Plot Minimal Fixation Heatmap
plot_pure_heatmap <- function(fix_df, target_screen, bg_image_path = NULL, bins = 30, alpha_val = 0.6) {
  
  # Filter fixations for the selected screen
  df_scr <- fix_df %>% 
    dplyr::filter(as.character(Screen_name) == as.character(target_screen))
  
  if (nrow(df_scr) == 0) {
    message(paste("No fixations found for Screen:", target_screen))
    return(NULL)
  }
  
  p <- ggplot(df_scr, aes(x = Mean_X, y = Mean_Y))
  
  # 1. Background Stimulus Image
  if (!is.null(bg_image_path) && file.exists(bg_image_path)) {
    img <- readPNG(bg_image_path)
    g_img <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
    p <- p + annotation_custom(g_img, xmin = 0, xmax = 1920, ymin = 0, ymax = 1080)
  }
  
  
  # 2. Pure Fixation Density Layer (Weighted by Fixation Duration)
  p <- p +
    stat_density_2d(
      aes(fill = after_stat(level), weight = Duration_ms),
      geom = "polygon",
      bins = bins,
      alpha = alpha_val
    ) +
    # Color Scale: Transparent Blue -> Green -> Yellow -> Red
    scale_fill_gradientn(
      colors = c("navy", "blue", "cyan", "green", "yellow", "red"),
      name = "Fixation\nDensity"
    ) +
    # Axis Alignment & Origin Mapping (Top-Left 0,0)
    scale_y_reverse(limits = c(1080, 0), expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) +
    labs(
      title = paste("Fixation Heatmap — Screen", target_screen),
      x = "X Position (px)", 
      y = "Y Position (px)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.background = element_rect(fill = "gray90")
    )
  
  return(p)
}


heatmap_image <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/Heatmap_Pure_Screen_1.png"

# Generate Pure Heatmap for Screen 4
heatmap_plot <- plot_pure_heatmap(
  fix_df = fixations_final, 
  target_screen = "1", 
  bg_image_path = bg_image_path,
  bins = 25,        # Higher = smoother / Lower = distinct visual hotspots
  alpha_val = 0.55  # Heatmap opacity over image (0 = invisible, 1 = solid)
)

if (!is.null(heatmap_plot)) {
  print(heatmap_plot)
  ggsave(heatmap_image, plot = heatmap_plot, width = 10, height = 8, dpi = 300)
}

