library(ggplot2)
library(png)
library(grid)
library(dplyr)
#######################
#######################
#######################



library(ggplot2)
library(png)
library(grid)
library(readxl)
library(dplyr)

# Load AOIs and filter for Screen 1
aois <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_Word_AOIs_Auto.xlsx")
aois_sub <- aois %>% filter(Screen_name == "1")

# Load Background Image
img <- readPNG("C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_1.png")
img_w <- dim(img)[2]
img_h <- dim(img)[1]
bg_image <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# Plot Word AOI Bounding Boxes over Stimulus Image
ggplot(aois_sub) +
  annotation_custom(bg_image, xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "blue", size = 0.5) +
  scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h, 0), expand = c(0, 0)) + # Y reversed for image orientation
  theme_minimal() +
  labs(title = "OCR Word AOI Verification (EasyB_1)")









#######################
#######################
#######################



# 1. Filter for Screen 1
fixations_sub <- fixations_EasyB %>%
  dplyr::filter(Screen_name == "1") %>%
  dplyr::mutate(Order = row_number()) # Adds fixation sequence (1, 2, 3...)

# 2. Load Image
img_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_1.png"
img <- readPNG(img_path)
img_w <- dim(img)[2]
img_h <- dim(img)[1]
bg_image <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# 3. Simple Scanpath Plot
scanpath_plot <- ggplot(fixations_sub, aes(x = Mean_X, y = Mean_Y)) +
  # Background Text Image
  annotation_custom(bg_image, xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
  
  # Eye Jump Lines (Saccades)
  geom_path(color = "blue", alpha = 0.5, size = 1) +
  
  # Fixation Circles (Sized by Duration)
  geom_point(aes(size = Duration_ms), color = "red", alpha = 0.6) +
  
  # Fixation Order Numbers inside the circles
  geom_text(aes(label = Order), color = "white", size = 3, fontface = "bold") +
  
  # Coordinates & Scaling
  scale_size_continuous(range = c(5, 15), name = "Duration (ms)") +
  scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h, 0), expand = c(0, 0)) +
  
  labs(
    title = "Reading Scanpath - HardB Screen 2",
    x = "Screen X (px)", 
    y = "Screen Y (px)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))

print(scanpath_plot)

#########################################
#########################################
#########################################
#########################################

library(ggplot2)
library(png)
library(grid)
library(dplyr)

# 1. Filter for Screen 1
fixations_sub <- fixations_EasyB %>%
  dplyr::filter(Screen_name == "2") %>%
  dplyr::mutate(Order = row_number()) # Adds fixation sequence (1, 2, 3...)

# 2. Load Image
img_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_2.png"
img <- readPNG(img_path)
img_w <- dim(img)[2]
img_h <- dim(img)[1]
bg_image <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# 3. Simple Scanpath Plot
scanpath_plot <- ggplot(fixations_sub, aes(x = Mean_X, y = Mean_Y)) +
  # Background Text Image
  annotation_custom(bg_image, xmin = 0, xmax = img_w, ymin = 0, ymax = img_h) +
  
  # Eye Jump Lines (Saccades)
  geom_path(color = "blue", alpha = 0.5, size = 1) +
  
  # Fixation Circles (Sized by Duration)
  geom_point(aes(size = Duration_ms), color = "red", alpha = 0.6) +
  
  # Fixation Order Numbers inside the circles
  geom_text(aes(label = Order), color = "white", size = 3, fontface = "bold") +
  
  # Coordinates & Scaling
  scale_size_continuous(range = c(5, 15), name = "Duration (ms)") +
  scale_x_continuous(limits = c(0, img_w), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h, 0), expand = c(0, 0)) +
  
  labs(
    title = "Reading Scanpath - EasyB Screen 1",
    x = "Screen X (px)", 
    y = "Screen Y (px)"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold"))

print(scanpath_plot)


#########################################
#########################################
#########################################
#########################################


library(ggplot2)
library(png)
library(grid)
library(dplyr)

# 1. Filter fixations specifically for Screen 3
fixations_EasyB_3 <- fixations_EasyB %>%
  dplyr::filter(Screen_name == "3")

# 2. Load the background image for Screen 3
img_path_3 <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_3.png" # Update path if needed
img_3 <- readPNG(img_path_3)

# Get dimensions of EasyB_3 image
img_w_3 <- dim(img_3)[2]
img_h_3 <- dim(img_3)[1]

# Create raster object for background
bg_image_3 <- rasterGrob(img_3, width = unit(1, "npc"), height = unit(1, "npc"))

# 3. Build the Heatmap Plot for Screen 3
heatmap_plot_3 <- ggplot(fixations_EasyB_3, aes(x = Mean_X, y = Mean_Y)) +
  # Draw background image
  annotation_custom(bg_image_3, xmin = 0, xmax = img_w_3, ymin = 0, ymax = img_h_3) +
  
  # Density heatmap layer
  stat_density_2d(
    aes(fill = ..level.., alpha = ..level..),
    geom = "polygon",
    bins = 15
  ) +
  
  # Color gradient: Blue (low density) -> Red (high density)
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) +
  scale_alpha(range = c(0.2, 0.7), guide = "none") +
  
  # Overlay individual fixation points
  geom_point(color = "red", size = 1.5, alpha = 0.6) +
  
  # Match screen coordinates (Reverse Y so top-left is 0,0)
  scale_x_continuous(limits = c(0, img_w_3), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h_3, 0), expand = c(0, 0)) +
  
  # Formatting
  labs(
    title = "Fixation Heatmap - EasyB Screen 3",
    x = "Screen X (pixels)",
    y = "Screen Y (pixels)",
    fill = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

# 4. Display the plot
print(heatmap_plot_3)

# 5. Save the plot to file
ggsave(
  filename = "C:/Users/Emir/Desktop/cvut intern/Original_Data/Heatmap_EasyB_3.png",
  plot = heatmap_plot_3,
  width = 10,
  height = 6,
  dpi = 300
)

###########################################################
###########################################################
###########################################################


library(ggplot2)
library(png)
library(grid)
library(dplyr)

# 1. Filter fixations specifically for Screen 4
fixations_EasyB_4 <- fixations_EasyB %>%
  dplyr::filter(Screen_name == "4")

# 2. Load the background image for Screen 4
img_path_4 <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_4.png" # Update path if needed
img_4 <- readPNG(img_path_4)

# Get dimensions of EasyB_4 image
img_w_4 <- dim(img_4)[2]
img_h_4 <- dim(img_4)[1]

# Create raster object for background
bg_image_4 <- rasterGrob(img_4, width = unit(1, "npc"), height = unit(1, "npc"))

# 3. Build the Heatmap Plot for Screen 4
heatmap_plot_4 <- ggplot(fixations_EasyB_4, aes(x = Mean_X, y = Mean_Y)) +
  # Draw background image
  annotation_custom(bg_image_4, xmin = 0, xmax = img_w_4, ymin = 0, ymax = img_h_4) +
  
  # Density heatmap layer
  stat_density_2d(
    aes(fill = ..level.., alpha = ..level..),
    geom = "polygon",
    bins = 15
  ) +
  
  # Color gradient: Blue (low density) -> Red (high density)
  scale_fill_gradientn(colors = c("blue", "green", "yellow", "red")) +
  scale_alpha(range = c(0.2, 0.7), guide = "none") +
  
  # Overlay individual fixation points
  geom_point(color = "red", size = 1.5, alpha = 0.6) +
  
  # Match screen coordinates (Reverse Y so top-left is 0,0)
  scale_x_continuous(limits = c(0, img_w_4), expand = c(0, 0)) +
  scale_y_reverse(limits = c(img_h_4, 0), expand = c(0, 0)) +
  
  # Formatting
  labs(
    title = "Fixation Heatmap - EasyB Screen 4",
    x = "Screen X (pixels)",
    y = "Screen Y (pixels)",
    fill = "Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

# 4. Display the plot
print(heatmap_plot_4)

# 5. Save the plot to file
ggsave(
  filename = "C:/Users/Emir/Desktop/cvut intern/Original_Data/Heatmap_EasyB_4.png",
  plot = heatmap_plot_4,
  width = 10,
  height = 6,
  dpi = 300
)
