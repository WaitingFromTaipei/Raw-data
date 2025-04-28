library(readr)
library(dplyr)


# 匯入先前用BORIS紀錄的tsv檔案
Trial1_Exp <- read_tsv("trial-1/20241010_trial-1_yellow_R.tsv")
Trial1_Ctr <- read_tsv("trial-1/20241010_trial-1_yellow_L.tsv")

# 以function計算行為時長
Duration <- function(df, behavior_name) {
  df_filtered <- df %>% 
    filter(Behavior == behavior_name) %>%
    select(`Behavior type`, Time)
  
  df_start <- df_filtered %>%
    filter(`Behavior type` == "START") %>%
    rename(Start = Time)
  
  df_stop <- df_filtered %>%
    filter(`Behavior type` == "STOP") %>%
    rename(Stop = Time)
  
  result <- df_start %>%
    select(Start) %>%
    mutate(Stop = df_stop$Stop) %>%
    mutate(Duration = Stop - Start)
  
  return(result)
}

# 將你要的行為分別抓出
Dura1_exp <- Duration(Trial1_Exp, "Right cage")
Dura1_ctr <- Duration(Trial1_Ctr, "Left cage")

