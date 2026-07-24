library(dplyr)
library(readxl)
library(purrr)

# ==========================================
# 1. Yardımcı Fonksiyonlar
# ==========================================


# ADIM 2: Basitleştirilmiş I-DT Fixation Detection Fonksiyonu
detect_fixations_idt_pure <- function(df, max_disp_x = 12, max_disp_y = 6, min_duration_ms = 100) {
  
  if (nrow(df) == 0) return(NULL)
  
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
            Min_X       = min(sub_df$MonitorX[i:fix_end]),
            Max_X       = max(sub_df$MonitorX[i:fix_end]),
            Mean_Y      = mean(sub_df$MonitorY[i:fix_end]),
            Min_Y       = min(sub_df$MonitorY[i:fix_end]),
            Max_Y       = max(sub_df$MonitorY[i:fix_end]),
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



# ==========================================
# 2. Veri Yükleme ve Akış
# ==========================================

# 1. Ham Veriyi Yükle ve Temizle (mutate kaldırıldı)
p01_1_data <- read_excel("C:/Users/Emir/Desktop/cvut intern/Original_Data/P01_1.xlsx")



clean_EasyB <- p01_1_data %>%
  dplyr::filter(!is.na(MonitorX), !is.na(MonitorY)) %>%
  dplyr::arrange(Screen_name, `Time[ms]`)

range(clean_EasyB$MonitorX)
range(clean_EasyB$MonitorY)

# 2. AOI Filtreleme (Sadece verilen sınırlar)

# 3. Fixation Tespiti
fixations_EasyB_filtered <- detect_fixations_idt_pure(
  clean_EasyB
)

# ADIM 3: Regression (Geri Sıçrama) Tespit Fonksiyonu
detect_regressions <- function(fixations_df, 
                               x_threshold = 50, 
                               y_threshold = 200) {
  
  if (is.null(fixations_df) || nrow(fixations_df) < 2) return(NULL)
  
  fixations_df %>%
    dplyr::group_by(Screen_name) %>%
    dplyr::arrange(Start_Time, .by_group = TRUE) %>%
    dplyr::mutate(
      Prev_Mean_X = lag(Mean_X),
      Prev_Mean_Y = lag(Mean_Y),
      
      Delta_X = Mean_X - Prev_Mean_X,
      Delta_Y = Mean_Y - Prev_Mean_Y,
      
      # Sola kayma (Intra-line regression)
      Is_X_Regression = ifelse(!is.na(Delta_X) & Delta_X < -x_threshold, TRUE, FALSE),
      
      # Yukarı kayma (Inter-line regression / üst satıra geçiş)
      Is_Y_Regression = ifelse(!is.na(Delta_Y) & Delta_Y < -y_threshold, TRUE, FALSE),
      
      # Herhangi bir geri gidiş var mı?
      Is_Regression = Is_X_Regression | Is_Y_Regression,
      
      Regression_Type = case_when(
        Is_X_Regression & Is_Y_Regression ~ "Diagonal (Up-Left)",
        Is_X_Regression ~ "Intra-line (Leftward)",
        Is_Y_Regression ~ "Inter-line (Upward)",
        TRUE ~ "Forward Progress"
      )
    ) %>%
    dplyr::ungroup()
}

# 4. Regression Analizi
fixations_with_regressions <- detect_regressions(
  fixations_EasyB_filtered, 
  x_threshold = 50,  
  y_threshold = 200  
)

View(fixations_with_regressions)

# ==========================================
# 3. Kontrol
# ==========================================
cat("AOI İçinde Kalan Ham Veri Adedi:", nrow(clean_EasyB_in_aoi), "\n")
cat("Tespit Edilen Fixation Sayısı   :", nrow(fixations_EasyB_filtered), "\n")


# İlk 10 sabitlemeyi (fixation) ekrana yazdırır
print(head(fixations_EasyB_filtered, 10))

View(fixations_EasyB_filtered)


