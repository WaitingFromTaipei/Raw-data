library(dplyr)
library(readr)
library(openxlsx)

# 讀取 TSV 檔案
file_m <- "trial-1/20241010_trial-1_yellow_M.tsv"
file_l <- "trial-1/20241010_trial-1_yellow_L.tsv"
file_r <- "trial-1/20241010_trial-1_yellow_R.tsv"

df_m <- read_tsv(file_m) %>% mutate(Source = "M", Time_M = Time, Time_Other = NA, Index_M = `Image index`, Index_Other = NA)
df_l <- read_tsv(file_l) %>% mutate(Source = "L", Time_M = NA, Time_Other = Time, Index_M = NA, Index_Other = `Image index`)
df_r <- read_tsv(file_r) %>% mutate(Source = "R", Time_M = NA, Time_Other = Time, Index_M = NA, Index_Other = `Image index`)

# 合併所有資料
df_combined <- bind_rows(df_m, df_l, df_r) %>%
  arrange(Time) %>% 
  mutate(Time_error = NA) %>% # 新增一個空白欄
  relocate(Time_error, .after = Time_Other)  # 把 time_error 移動到 Time_Other 右邊 

# 建立 Excel Workbook
wb <- createWorkbook()
addWorksheet(wb, "Behavior Data")

# 定義不同 Source 的底色
style_M <- createStyle(fgFill = "#FFDDC1")  # 淺橘色 (M)
style_L <- createStyle(fgFill = "#C1E1FF")  # 淺藍色 (L)
style_R <- createStyle(fgFill = "#C1FFC1")  # 淺綠色 (R)

# 寫入資料
writeData(wb, "Behavior Data", df_combined)

# 依據 Source 設定底色
for (i in 1:nrow(df_combined)) {
  if (df_combined$Source[i] == "M") {
    addStyle(wb, "Behavior Data", style_M, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  } else if (df_combined$Source[i] == "L") {
    addStyle(wb, "Behavior Data", style_L, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  } else if (df_combined$Source[i] == "R") {
    addStyle(wb, "Behavior Data", style_R, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  }
}

# 儲存 Excel 檔案
saveWorkbook(wb, "trial-1/20241010_trial-1_yellow_marked.xlsx", overwrite = TRUE)
