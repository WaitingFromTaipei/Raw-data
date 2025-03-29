library(dplyr)
library(readr)
library(openxlsx)

# 讀取 TSV 檔案
file_m <- "trial-2/20241011_trial-2_59187_M.tsv"
file_l <- "trial-2/20241011_trial-2_59187_L.tsv"
file_r <- "trial-2/20241011_trial-2_59187_R.tsv"


df_m <- read_tsv(file_m) %>% mutate(Source = "M", Time_M = Time, Time_Other = NA, Index_M = `Image index`, Index_Other = NA)
df_l <- read_tsv(file_l) %>% mutate(Source = "L", Time_M = NA, Time_Other = Time, Index_M = NA, Index_Other = `Image index`)
df_r <- read_tsv(file_r) %>% mutate(Source = "R", Time_M = NA, Time_Other = Time, Index_M = NA, Index_Other = `Image index`)

# **步驟 1：合併所有資料**
df_combined <- bind_rows(df_m, df_l, df_r) %>%
  arrange(Time)

# **步驟 2：計算時間誤差**
df_combined <- df_combined %>%
  arrange(Time) %>%
  group_by(Behavior, `Behavior type`) %>%
  mutate(
    time_error = ifelse(!is.na(Time_M) & !is.na(Time_Other), Time_Other - Time_M, NA) # 計算 `_M` 與其他檔案的誤差
  ) %>%
  ungroup()

# **步驟 3：建立 Excel Workbook**
wb <- createWorkbook()
addWorksheet(wb, "Behavior Data")

# **步驟 4：定義不同 Source 的底色**
style_M <- createStyle(fgFill = "#FFDDC1")  # 淺橘色 (M)
style_L <- createStyle(fgFill = "#C1E1FF")  # 淺藍色 (L)
style_R <- createStyle(fgFill = "#C1FFC1")  # 淺綠色 (R)

# **步驟 5：寫入資料**
writeData(wb, "Behavior Data", df_combined)

# **步驟 6：依據 Source 設定底色**
for (i in 1:nrow(df_combined)) {
  if (df_combined$Source[i] == "M") {
    addStyle(wb, "Behavior Data", style_M, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  } else if (df_combined$Source[i] == "L") {
    addStyle(wb, "Behavior Data", style_L, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  } else if (df_combined$Source[i] == "R") {
    addStyle(wb, "Behavior Data", style_R, rows = i + 1, cols = 1:ncol(df_combined), gridExpand = TRUE)
  }
}

# **步驟 7：儲存 Excel 檔案**
saveWorkbook(wb, "trial-2/20241011_trial-2_59187_marked.xlsx", overwrite = TRUE)

print("檔案已整合完成")