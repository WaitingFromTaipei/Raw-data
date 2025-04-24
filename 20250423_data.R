library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

Info <- read_csv("20250331高田前測進度.csv")

Trial1_Exp <- read_tsv("trial-1/20241010_trial-1_yellow_R.tsv")
Trial1_Ctr <- read_tsv("trial-1/20241010_trial-1_yellow_L.tsv")

Trial2_Exp <- read_tsv("trial-2/20241011_trial-2_59187_L.tsv")
Trial2_Ctr <- read_tsv("trial-2/20241011_trial-2_59187_R.tsv")

Trial3_Exp <- read_tsv("trial-3/20241013_trial-3_59215_R.tsv")
Trial3_Ctr <- read_tsv("trial-3/20241013_trial-3_59215_L.tsv")

Trial4_Exp <- read_tsv("trial-4/20241018_trial-4_59181_L.tsv")
Trial4_Ctr <- read_tsv("trial-4/20241018_trial-4_59181_R.tsv")

Trial5_Exp <- read_tsv("trial-5/20241017_trial-5_12693_L.tsv")
Trial5_Ctr <- read_tsv("trial-5/20241017_trial-5_12693_R.tsv")

Trial6_Exp <- read_tsv("trial-6/20241017_trial-6_59200_R.tsv")
Trial6_Ctr <- read_tsv("trial-6/20241017_trial-6_59200_L.tsv")

Trial7_Exp <- read_tsv("trial-7/20241024_trial-7_yellow_L.tsv")
Trial7_Ctr <- read_tsv("trial-7/20241024_trial-7_yellow_R.tsv")

Trial9_Exp <- read_tsv("trial-9/20241026_trial-9_59187_L.tsv")
Trial9_Ctr <- read_tsv("trial-9/20241026_trial-9_59187_R.tsv")

# 高田進食重量比較

Food_weight <- Info %>% select(Trial, `Exp剩餘食物量(g)`,`Control剩餘食物量(g)`)
Food_weight <- Food_weight %>%mutate(`Exp food intake` = 30-`Exp剩餘食物量(g)`,
                                     `Control food intake` = 30-`Control剩餘食物量(g)`)
Food_weight <- Food_weight %>% slice(-1,-2,-3) # slice:使用橫列索引選取特定橫列
# 後續可使用slice來選取特定次序之實驗數據

boxplot(Food_weight$`Exp food intake`, Food_weight$`Control food intake`)
# ggplot待研究



# 高田碰觸時間比較
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

Dura1_exp <- Duration(Trial1_Exp, "Right cage")
Dura1_ctr <- Duration(Trial1_Ctr, "Left cage")

Dura2_exp <- Duration(Trial2_Exp, "Left cage")
Dura2_ctr <- Duration(Trial2_Ctr, "Right cage")

Dura3_exp <- Duration(Trial3_Exp, "Right cage")
Dura3_ctr <- Duration(Trial3_Ctr, "Left cage")

Dura4_exp <- Duration(Trial4_Exp, "Left cage")
Dura4_ctr <- Duration(Trial4_Ctr, "Right cage")

Dura5_exp <- Duration(Trial5_Exp, "Left cage")
Dura5_ctr <- Duration(Trial5_Ctr, "Right cage")

Dura6_exp <- Duration(Trial6_Exp, "Right cage")
Dura6_ctr <- Duration(Trial6_Ctr, "Left cage")

Dura7_exp <- Duration(Trial7_Exp, "Left cage")
Dura7_ctr <- Duration(Trial7_Ctr, "Right cage")

Dura9_exp <- Duration(Trial9_Exp, "Left cage")
Dura9_ctr <- Duration(Trial9_Ctr, "Right cage")

# 用3,4,5,6,7,9次實驗資料?
