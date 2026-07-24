library(dplyr)
library(readxl)
library(purrr)
library(ggplot2)
library(png)  # Arka plan PNG resimlerini okumak için
library(grid)

# ==========================================
# 1. YARDIMCI FONKSİYONLAR
# ==========================================

# AOI Filtreleme (Sınırlar Düzeltildi)
filter_gaze_by_custom_aoi <- function(df, xmin = 125, xmax = 1850, ymin = 150, ymax = 1000) {
  df %>%
    dplyr::filter(
      MonitorX >= xmin & MonitorX <= xmax &
        MonitorY >= ymin & MonitorY <= ymax
    )
}

detect_fixations_idt_pure <- function(df, max_disp_x = 96, max_disp_y = 60, min_duration_ms = 100) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(function(sub_df) {
      n_rows <- nrow(sub_df)
      if (n_rows < 2) return(NULL)
      
      fix_list <- list()
      i <- 1
      
      while (i < n_rows) {
        j <- i + 1
        while (j <= n_rows) {
          disp_x <- diff(range(sub_df$MonitorX[i:j]))
          disp_y <- diff(range(sub_df$MonitorY[i:j]))
          
          if (disp_x <= max_disp_x && disp_y <= max_disp_y) {
            j <- j + 1
          } else {
            break
          }
        }
        
        fix_end <- j - 1
        duration <- sub_df$`Time[ms]`[fix_end] - sub_df$`Time[ms]`[i]
        
        if (duration >= min_duration_ms) {
          fix_list[[length(fix_list) + 1]] <- data.frame(
            Screen_name = as.character(sub_df$Screen_name[1]),
            Start_Time  = sub_df$`Time[ms]`[i],
            End_Time    = sub_df$`Time[ms]`[fix_end],
            Duration_ms = duration,
            Mean_X      = mean(sub_df$MonitorX[i:fix_end]),
            Mean_Y      = mean(sub_df$MonitorY[i:fix_end]),
            Samples     = (fix_end - i + 1)
          )
          i <- fix_end + 1
        } else {
          i <- i + 1
        }
      }
      
      dplyr::bind_rows(fix_list)
    })
}

detect_regressions <- function(fixations_df, x_threshold = 50, y_threshold = 200) {
  if (is.null(fixations_df) || nrow(fixations_df) < 2) return(NULL)
  
  fixations_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::arrange(Start_Time, .by_group = TRUE) %>%
    dplyr::mutate(
      Prev_Mean_X = lag(Mean_X),
      Prev_Mean_Y = lag(Mean_Y),
      Delta_X     = Mean_X - Prev_Mean_X,
      Delta_Y     = Mean_Y - Prev_Mean_Y,
      
      Is_X_Regression = ifelse(!is.na(Delta_X) & Delta_X < -x_threshold, TRUE, FALSE),
      Is_Y_Regression = ifelse(!is.na(Delta_Y) & Delta_Y < -y_threshold, TRUE, FALSE),
      Is_Regression   = Is_X_Regression | Is_Y_Regression,
      
      Regression_Type = case_when(
        Is_X_Regression & Is_Y_Regression ~ "Diagonal (Up-Left)",
        Is_X_Regression ~ "Intra-line (Leftward)",
        Is_Y_Regression ~ "Inter-line (Upward)",
        TRUE ~ "Forward Progress"
      )
    ) %>%
    dplyr::ungroup()
}

# ==========================================
# 2. SCANPATH GÖRSELLEŞTİRME FONKSİYONU
# ==========================================

plot_scanpath_single_screen <- function(fixations_df, 
                                        screen_id, 
                                        bg_image_path = NULL,
                                        xmin = 125, xmax = 1850, 
                                        ymin = 150, ymax = 1000) {
  
  if (is.null(fixations_df) || nrow(fixations_df) == 0) {
    message("İşlenecek fixation verisi bulunamadı (NULL/Boş).")
    return(NULL)
  }
  
  # Sadece istenen ekranı süz
  df_scr <- fixations_df %>% 
    dplyr::filter(as.character(Screen_name) == as.character(screen_id)) %>% 
    dplyr::arrange(Start_Time) %>% 
    dplyr::mutate(Fix_ID = row_number())
  
  if (nrow(df_scr) == 0) {
    message(paste("Screen", screen_id, "için çizdirilecek veri bulunamadı."))
    return(NULL)
  }
  
  p <- ggplot(df_scr, aes(x = Mean_X, y = Mean_Y))
  
  # 1. Arka Plan Resmi Varsa Ekle
  if (!is.null(bg_image_path) && file.exists(bg_image_path)) {
    img <- readPNG(bg_image_path)
    g_img <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
    p <- p + annotation_custom(g_img, xmin = 0, xmax = 1920, ymin = 0, ymax = 1080)
  }
  
  p <- p +
    # 2. Kırmızı Kesikli AOI Kutusu (Tight Text AOI Boundary)
    annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             color = "red", fill = NA, linetype = "dashed", linewidth = 1) +
    
    # 3. Fixation'lar Arasındaki Bağlantı Çizgileri (Mavi Scanpath Çizgisi)
    geom_path(color = "blue", linewidth = 0.8, alpha = 0.8) +
    
    # 4. Fixation Balonları (Süreye Göre Ölçeklenen Mavi Daireler)
    geom_point(aes(size = Duration_ms), color = "blue", alpha = 0.8) +
    
    # 5. Balon İçi Sıra Numaraları (Beyaz Metin)
    geom_text(aes(label = Fix_ID), color = "white", size = 2.5, fontface = "bold") +
    
    # Boyut ve Eksen Ayarları
    scale_size_continuous(
      name = "Duration (ms)",
      range = c(3, 9),
      breaks = c(200, 300, 400, 500)
    ) +
    scale_y_reverse(limits = c(1080, 0), breaks = c(0, 300, 600, 900, 1200), expand = c(0,0)) +
    scale_x_continuous(limits = c(0, 1920), breaks = c(0, 500, 1000, 1500), expand = c(0,0)) +
    
    # Başlık ve Etiketler
    labs(
      title = paste("Text AOI Bounding Box & Fixation Path - Screen", screen_id),
      subtitle = "Red dashed line = Tight Text AOI Boundary",
      x = "X Position (px)",
      y = "Y Position (px)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "red", size = 10),
      panel.background = element_rect(fill = "gray50")
    )
  
  return(p)
}

# ==========================================
# 3. PIPELINE & ÇIKTI ÜRETİMİ
# ==========================================

raw_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")

clean_data <- raw_data %>%
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

# ymax = 1000 olarak güncellendi!
fixations_final <- clean_data %>%
  filter_gaze_by_custom_aoi(xmin = 0, xmax = 1920, ymin = 0, ymax = 1200) %>%
  detect_fixations_idt_pure(max_disp_x = 12, max_disp_y = 6, min_duration_ms = 100) %>%
  detect_regressions(x_threshold = 50, y_threshold = 200)



View(fixations_final)
cat("AOI İçinde Kalan Ham Veri Adedi:", nrow(fixations_final), "\n")
# ------------------------------------------
# Screen 1 Grafiğini Çiz
# ------------------------------------------
screen1_bg_path <- "C:/Users/Emir/Desktop/cvut intern/Original_Data/EasyB_1.png"

p_screen1 <- plot_scanpath_single_screen(
  fixations_df = fixations_final, 
  screen_id = "1",
  bg_image_path = screen1_bg_path,
  xmin = 0, xmax = 1920, ymin = 0, ymax = 1200
)

# Grafiği Ekranda Göster
if (!is.null(p_screen1)) {
  print(p_screen1)
  
  # Kaydet
  ggsave("C:/Users/Emir/Desktop/cvut intern/Original_Data/Scanpath_Screen1.png", 
         plot = p_screen1, width = 10, height = 8, dpi = 300)
}

